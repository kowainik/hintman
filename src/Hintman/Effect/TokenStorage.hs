{- | Monad to manipulate storage tokens:

1. Add new key-value pairs.
2. Delete key-value pairs.
-}

module Hintman.Effect.TokenStorage
       ( MonadTokenStorage (..)
       , acquireInstallationToken
       , initialiseInstallationIds
       ) where

import UnliftIO (MonadUnliftIO)
import UnliftIO.MVar (modifyMVar)

import Hintman.App (App, AppErrorType (..), Has, TokenCache, WithError, grab, throwError)
import Hintman.Core.PrInfo (FullRepo, Repositories (..), displayFullRepo)
import Hintman.Core.Token (AppInfo, InstallationId, InstallationToken (..))
import Hintman.Installation (createInstallationToken, mkInstallationsRequest, mkJwtToken,
                             mkRepositoriesRequest, performRequest, renewToken)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


class Monad m => MonadTokenStorage m where
    insertToken :: FullRepo -> InstallationToken -> m ()
    deleteToken :: FullRepo -> m ()
    lookupToken :: FullRepo -> m (Maybe (MVar InstallationToken))

instance MonadTokenStorage App where
    insertToken = insertTokenImpl
    deleteToken = deleteTokenImpl
    lookupToken = lookupTokenImpl

{- | This function returns 'InstallationToken'. This function automatically
renews and updates it if needed. So it returns token that can be used to perform
queries.
-}
acquireInstallationToken
    :: ( MonadUnliftIO m
       , MonadTokenStorage m
       , Has AppInfo env
       , WithError m
       , WithLog env m
       )
    => FullRepo
    -> m InstallationToken
acquireInstallationToken fullRepo = lookupToken fullRepo >>= \case
    Nothing -> throwError $ ServerError $
        "Can't find token for: " <> displayFullRepo fullRepo
    Just tokenVar -> modifyMVar tokenVar $ \oldToken -> do
        newToken <- renewToken oldToken
        pure (newToken, newToken)

{- | This function asks all installation ids for Hintman from GitHub. This
function should be called only once at the application start.

See the following issue for more details:

* https://github.com/kowainik/hintman/issues/73
-}
initialiseInstallationIds
    :: ( MonadIO m
       , MonadTokenStorage m
       , WithError m
       , WithLog env m
       , Has AppInfo env
       )
    => m ()
initialiseInstallationIds = do
    jwtToken <- mkJwtToken

    installationIds <- performRequest @[InstallationId]
        $ mkInstallationsRequest jwtToken

    for_ installationIds $ \installationId -> do
        log D $ "Installation id: " <> show installationId
        installationToken <- createInstallationToken jwtToken installationId

        Repositories repositories <- performRequest @Repositories
            $ mkRepositoriesRequest $ itToken installationToken
        log D $ "This ID is for the following repos: "
            <> T.intercalate ", " (map displayFullRepo repositories)

        for_ repositories $ \fullRepo -> insertToken fullRepo installationToken

----------------------------------------------------------------------------
-- Internals
----------------------------------------------------------------------------

insertTokenImpl
    :: (MonadReader env m, Has TokenCache env, MonadIO m)
    => FullRepo
    -> InstallationToken
    -> m ()
insertTokenImpl fullRepo token = do
    tokenVar <- newMVar token
    ref <- grab @TokenCache
    atomicModifyIORef' ref $
        \cache -> (HM.insert fullRepo tokenVar cache, ())

deleteTokenImpl
    :: (MonadReader env m, Has TokenCache env, MonadIO m)
    => FullRepo
    -> m ()
deleteTokenImpl fullRepo = do
    ref <- grab @TokenCache
    atomicModifyIORef' ref $ \cache -> (HM.delete fullRepo cache, ())

lookupTokenImpl
    :: (MonadReader env m, Has TokenCache env, MonadIO m)
    => FullRepo
    -> m (Maybe (MVar InstallationToken))
lookupTokenImpl fullRepo = do
    ref <- grab @TokenCache
    cache <- readIORef ref
    pure $ HM.lookup fullRepo cache
