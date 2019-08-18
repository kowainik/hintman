-- | 'ReaderT' environment for main application monad.

module Hintman.App.Env
       ( Env (..)
       , TokenCache

       , Has (..)
       , grab
       ) where

import Hintman.Config (HintmanConfig)
import Hintman.Core.Key (GitHubKey)
import Hintman.Core.PrInfo (Owner, Repo)
import Hintman.Core.Token (AppInfo, GitHubToken)


type TokenCache = IORef (HashMap (Owner, Repo) GitHubToken)

data Env = Env
    { envConfig     :: !HintmanConfig
    , envGitHubKey  :: !GitHubKey
    , envAppInfo    :: !AppInfo
    , envTokenCache :: !TokenCache
    }

class Has field env where
    obtain :: env -> field

instance Has AppInfo    Env where obtain = envAppInfo
instance Has TokenCache Env where obtain = envTokenCache

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
