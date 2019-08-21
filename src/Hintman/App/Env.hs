-- | 'ReaderT' environment for main application monad.

module Hintman.App.Env
       ( Env (..)
       , TokenCache

       , Has (..)
       , grab
       ) where

import Colog (HasLog (..), LogAction, Message)

import Hintman.Config (HintmanConfig)
import Hintman.Core.Key (GitHubKey)
import Hintman.Core.PrInfo (Owner, Repo)
import Hintman.Core.Token (AppInfo, GitHubToken)


type TokenCache = IORef (HashMap (Owner, Repo) GitHubToken)

data Env m = Env
    { envConfig     :: !HintmanConfig
    , envGitHubKey  :: !GitHubKey
    , envAppInfo    :: !AppInfo
    , envTokenCache :: !TokenCache
    , envLogAction  :: !(LogAction m Message)
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env { envLogAction = newAction }

class Has field env where
    obtain :: env -> field

instance Has AppInfo    (Env m) where obtain = envAppInfo
instance Has TokenCache (Env m) where obtain = envTokenCache

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
