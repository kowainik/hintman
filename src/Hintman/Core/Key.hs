{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE Rank2Types #-}

{- | This module contains the 'GitHubKey' data type which is a workaround for
the type from the @servant-github-webhook@ library which doesn't allow to handle
events of different types.

See the following issue for more details:

* https://github.com/tsani/servant-github-webhook/issues/13
-}

module Hintman.Core.Key
       ( GitHubKey
       , gitHubKey
       ) where

import Servant.Server (Context ((:.)), HasContextEntry (..))

import qualified Servant.GitHub.Webhook as Webhook (GitHubKey, gitHubKey)


{- | Wrapper around 'Webhook.GitHubKey' to hide type parameter.
-}
newtype GitHubKey = GitHubKey (forall result . Webhook.GitHubKey result)

{- | Smart constructor for 'GitHubKey'.
-}
gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Webhook.gitHubKey k)

instance HasContextEntry '[GitHubKey] (Webhook.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
