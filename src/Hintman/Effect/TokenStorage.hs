{- | Monad to manipulate storage tokens:

1. Add new key-value pairs.
2. Delete key-value pairs.
-}

module Hintman.Effect.TokenStorage
       ( MonadTokenStorage (..)
       , acquireInstallationToken
       ) where

import UnliftIO (MonadUnliftIO)
import UnliftIO.MVar (modifyMVar)

import Hintman.App (App, AppErrorType (..), Has, TokenCache, WithError, grab, throwError)
import Hintman.Core.PrInfo (Owner, Repo)
import Hintman.Core.Token (AppInfo, InstallationToken)
import Hintman.Installation (renewToken)

import qualified Data.HashMap.Strict as HM


class Monad m => MonadTokenStorage m where
    insertToken :: Owner -> Repo -> InstallationToken -> m ()
    deleteToken :: Owner -> Repo -> m ()
    lookupToken :: Owner -> Repo -> m (Maybe (MVar InstallationToken))

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
    => Owner
    -> Repo
    -> m InstallationToken
acquireInstallationToken owner repo = lookupToken owner repo >>= \case
    Nothing -> throwError $ ServerError $
        "Can't find token for: " <> show owner <> "/" <> show repo
    Just tokenVar -> modifyMVar tokenVar $ \oldToken -> do
        newToken <- renewToken oldToken
        pure (newToken, newToken)

----------------------------------------------------------------------------
-- Internals
----------------------------------------------------------------------------

insertTokenImpl
    :: (MonadReader env m, Has TokenCache env, MonadIO m)
    => Owner
    -> Repo
    -> InstallationToken
    -> m ()
insertTokenImpl owner repo token = do
    tokenVar <- newMVar token
    ref <- grab @TokenCache
    atomicModifyIORef' ref $
        \cache -> (HM.insert (owner, repo) tokenVar cache, ())

deleteTokenImpl
    :: (MonadReader env m, Has TokenCache env, MonadIO m)
    => Owner
    -> Repo
    -> m ()
deleteTokenImpl owner repo = do
    ref <- grab @TokenCache
    atomicModifyIORef' ref $ \cache -> (HM.delete (owner, repo) cache, ())

lookupTokenImpl
    :: (MonadReader env m, Has TokenCache env, MonadIO m)
    => Owner
    -> Repo
    -> m (Maybe (MVar InstallationToken))
lookupTokenImpl owner repo = do
    ref <- grab @TokenCache
    cache <- readIORef ref
    pure $ HM.lookup (owner, repo) cache
