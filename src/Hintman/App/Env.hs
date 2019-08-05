-- | 'ReaderT' environment for main application monad.

module Hintman.App.Env
       ( Env (..)
       ) where

import GitHub.Data.Webhooks.Events (PullRequestEvent)
import Servant.GitHub.Webhook (GitHubKey)

import Hintman.Config (HintmanConfig)


data Env = Env
    { envConfig    :: !HintmanConfig
    , envGitHubKey :: !(GitHubKey PullRequestEvent)
    }
