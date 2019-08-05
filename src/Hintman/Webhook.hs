{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hintman.Webhook
       ( hintmanApp
       ) where

import GitHub.Data.Webhooks.Events (PullRequestEvent (..))
import Servant ((:>), Application, Context ((:.), EmptyContext), JSON, Post, Server,
                hoistServerWithContext, serveWithContext)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.GitHub.Webhook (GitHubEvent, GitHubKey, GitHubSignedReqBody, RepoWebhookEvent (..))
import Servant.Server.Generic (AsServerT)

import Hintman.App (App, Env (..), runAppAsHandler)


newtype HintmanSite route = HintmanSite
    { hintmanListenRoute :: route
        :- GitHubEvent '[ 'WebhookPullRequestEvent ]
        :> GitHubSignedReqBody '[JSON] PullRequestEvent
        :> Post '[JSON] ()
    } deriving stock (Generic)

type HintmanAPI = ToServantApi HintmanSite

hintmanServer :: HintmanSite (AsServerT App)
hintmanServer = HintmanSite
    { hintmanListenRoute = issueCommentHook
    }

issueCommentHook :: MonadIO m => RepoWebhookEvent -> ((), PullRequestEvent) -> m ()
issueCommentHook _ ev = print $ evPullReqPayload $ snd ev

server :: Env -> Server HintmanAPI
server env@Env{..} = hoistServerWithContext
    (Proxy @HintmanAPI)
    (Proxy @'[GitHubKey PullRequestEvent])
    (runAppAsHandler env)
    (toServant hintmanServer)

hintmanApp :: Env -> Application
hintmanApp env@Env{..} = serveWithContext
    (Proxy @HintmanAPI)
    (envGitHubKey :. EmptyContext)
    (server env)
