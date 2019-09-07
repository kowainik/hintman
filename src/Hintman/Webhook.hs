{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hintman.Webhook
       ( hintmanApp
       ) where

import GitHub.Data.Webhooks.Events (InstallationEvent (..), InstallationEventAction (..),
                                    InstallationRepoEventAction (..),
                                    InstallationRepositoriesEvent (..), PullRequestEvent (..))
import GitHub.Data.Webhooks.Payload (HookInstallation (..), HookRepositorySimple (..),
                                     HookUser (..))
import Servant (Application, Context ((:.), EmptyContext), Server, hoistServerWithContext,
                serveWithContext)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))
import Servant.Server.Generic (AsServerT)

import Hintman.App (App, AppEnv, Env (..), Has, WithError, runAppAsHandler)
import Hintman.Core.Key (GitHubKey)
import Hintman.Core.PrInfo (Owner (..))
import Hintman.Core.Token (AppInfo, InstallationId (..), InstallationToken (..))
import Hintman.Effect.TokenStorage (MonadTokenStorage (..))
import Hintman.Installation (createInstallationToken, mkJwtToken)


data HintmanSite route = HintmanSite
    { -- | Main endpoint that listens PR events.
      hintmanListenRoute :: route
        :- GitHubEvent '[ 'WebhookPullRequestEvent ]
        :> GitHubSignedReqBody '[JSON] PullRequestEvent
        :> Post '[JSON] ()

      -- | Triggered when Hintman has been installed or uninstalled.
    , hintmanInstallAppRoute :: route
        :- GitHubEvent '[ 'WebhookInstallationEvent ]
        :> GitHubSignedReqBody '[JSON] InstallationEvent
        :> Post '[JSON] ()

      -- | Triggered when a repository is added or removed from an installation.
    , hintmanInstallRepoRoute :: route
        :- GitHubEvent '[ 'WebhookInstallationRepositoriesEvent ]
        :> GitHubSignedReqBody '[JSON] InstallationRepositoriesEvent
        :> Post '[JSON] ()
    } deriving stock (Generic)

type HintmanAPI = ToServantApi HintmanSite

hintmanServer :: HintmanSite (AsServerT App)
hintmanServer = HintmanSite
    { hintmanListenRoute = issueCommentHook
    , hintmanInstallAppRoute = appInstalledHook
    , hintmanInstallRepoRoute = repoInstalledHook
    }

issueCommentHook :: MonadIO m => RepoWebhookEvent -> ((), PullRequestEvent) -> m ()
issueCommentHook _ ev = print $ evPullReqPayload $ snd ev

appInstalledHook
    :: (MonadIO m, MonadTokenStorage m, Has AppInfo env, WithError m, WithLog env m)
    => RepoWebhookEvent
    -> ((), InstallationEvent)
    -> m ()
appInstalledHook _ ((), ev) = do
    log I "'InstallationEvent' triggered"
    let installationId = InstallationId $ whInstallationId $ evInstallationInfo ev
    let owner = whInstallationAccount $ evInstallationInfo ev

    jwtToken <- mkJwtToken
    case evInstallationAction ev of
        InstallationCreatedAction -> do
            log D "InstallationCreatedAction"
            token <- createInstallationToken jwtToken installationId
            for_ (evInstallationRepos ev) (cacheRepo token owner)
        InstallationDeletedAction -> log I "App deleted" -- TODO: delete all
        InstallationActionOther _ -> error "Unknown action"  -- TODO: proper error with logging

repoInstalledHook
    :: (MonadIO m, MonadTokenStorage m, Has AppInfo env, WithError m, WithLog env m)
    => RepoWebhookEvent
    -> ((), InstallationRepositoriesEvent)
    -> m ()
repoInstalledHook _ ((), ev) = do
    log I "'InstallationRepositoriesEvent' triggered"
    let installationId = InstallationId $ whInstallationId $ evInstallationRepoInfo ev
    let owner = whInstallationAccount $ evInstallationRepoInfo ev

    jwtToken <- mkJwtToken
    case evInstallationRepoAction ev of
        InstallationRepoCreatedAction -> do
            log D "InstallationRepoCreatedAction"
            token <- createInstallationToken jwtToken installationId
            for_ (evInstallationReposAdd ev) (cacheRepo token owner)
        InstallationRepoRemovedAction -> log I "Repo deleted"
        InstallationRepoActionOther _ -> error "Unknown action"  -- TODO: proper error with logging

cacheRepo
    :: (MonadTokenStorage m, WithLog env m)
    => InstallationToken
    -> HookUser
    -> HookRepositorySimple
    -> m ()
cacheRepo token hookUser _hookRepo = do
    let owner  = Owner $ whUserLogin hookUser
    log D $ "Inserting token for: " <> unOwner owner
    insertToken owner token

server :: AppEnv -> Server HintmanAPI
server env@Env{..} = hoistServerWithContext
    (Proxy @HintmanAPI)
    (Proxy @'[ GitHubKey ])
    (runAppAsHandler env)
    (toServant hintmanServer)

hintmanApp :: AppEnv -> Application
hintmanApp env@Env{..} = serveWithContext
    (Proxy @HintmanAPI)
    (envGitHubKey :. EmptyContext)
    (server env)
