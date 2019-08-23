{- | Monad to manipulate storage tokens:

1. Add new key-value pairs.
2. Delete key-value pairs.
-}

module Hintman.Effect.TokenStorage
       ( MonadTokenStorage (..)
       ) where

import Hintman.App (App, Has, TokenCache, grab)
import Hintman.Core.PrInfo (Owner, Repo)
import Hintman.Core.Token (InstallationToken)

import qualified Data.HashMap.Strict as HM


class Monad m => MonadTokenStorage m where
    insertToken :: Owner -> Repo -> InstallationToken -> m ()
    deleteToken :: Owner -> Repo -> m ()
    lookupToken :: Owner -> Repo -> m (Maybe InstallationToken)

instance MonadTokenStorage App where
    insertToken = insertTokenImpl
    deleteToken = deleteTokenImpl
    lookupToken = lookupTokenImpl

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
    ref <- grab @TokenCache
    atomicModifyIORef' ref $ \cache -> (HM.insert (owner, repo) token cache, ())

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
    -> m (Maybe InstallationToken)
lookupTokenImpl owner repo = do
    ref <- grab @TokenCache
    cache <- readIORef ref
    pure $ HM.lookup (owner, repo) cache
