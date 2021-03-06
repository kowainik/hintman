-- | 'ReaderT' environment for main application monad.

module Hintman.App.Env
       ( Env (..)
       , TokenCache

       , Has (..)
       , grab
       ) where

import Colog (HasLog (..), LogAction, Message)

import Hintman.Core.Key (GitHubKey)
import Hintman.Core.PrInfo (Owner)
import Hintman.Core.Token (AppInfo, InstallationToken)


{- | A mapping from 'Owner' to a 'MVar' that stores
'InstallationToken'. 'MVar' is required to block on token so concurrent accesses
to the token won't renew it twice making one of the tokens invalid.
-}
type TokenCache = IORef (HashMap Owner (MVar InstallationToken))

data Env m = Env
    { envGitHubKey  :: !GitHubKey
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
