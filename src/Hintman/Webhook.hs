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

import Hintman.App (App, Env (..), Has, grab, runAppAsHandler)
import Hintman.Core.Key (GitHubKey)
import Hintman.Core.PrInfo (Owner (..), Repo (..))
import Hintman.Core.Token (AppInfo, InstallationAccessToken (..), InstallationId (..))
import Hintman.Effect.TokenStorage (MonadTokenStorage (..))
import Hintman.Installation (createAccessToken)


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
    :: (MonadIO m, MonadTokenStorage m, MonadReader env m, Has AppInfo env)
    => RepoWebhookEvent
    -> ((), InstallationEvent)
    -> m ()
appInstalledHook _ ((), ev) = do
    putTextLn "'InstallationEvent' triggered"
    let installationId = InstallationId $ show $ whInstallationId $ evInstallationInfo ev
    let owner = whInstallationAccount $ evInstallationInfo ev
    case evInstallationAction ev of
        InstallationCreatedAction -> do
            putTextLn "InstallationCreatedAction"
            appInfo <- grab @AppInfo
            liftIO (createAccessToken installationId appInfo) >>= \case
                Nothing -> putTextLn "Failed to create access token"
                Just token -> for_ (evInstallationRepos ev) (cacheRepo token owner)
        InstallationDeletedAction -> putTextLn "App deleted" -- TODO: delete all
        InstallationActionOther _ -> error "Unknown action"  -- TODO: proper error with logging

repoInstalledHook
    :: (MonadIO m, MonadTokenStorage m, MonadReader env m, Has AppInfo env)
    => RepoWebhookEvent
    -> ((), InstallationRepositoriesEvent)
    -> m ()
repoInstalledHook _ ((), ev) = do
    putTextLn "'InstallationRepositoriesEvent' triggered"
    let installationId = InstallationId $ show $ whInstallationId $ evInstallationRepoInfo ev
    let owner = whInstallationAccount $ evInstallationRepoInfo ev
    case evInstallationRepoAction ev of
        InstallationRepoCreatedAction -> do
            putTextLn "InstallationRepoCreatedAction"
            appInfo <- grab @AppInfo
            liftIO (createAccessToken installationId appInfo) >>= \case
                Nothing -> putTextLn "Failed to create access token"
                Just token -> for_ (evInstallationReposAdd ev) (cacheRepo token owner)
        InstallationRepoRemovedAction -> putTextLn "Repo deleted"
        InstallationRepoActionOther _ -> error "Unknown action"  -- TODO: proper error with logging

cacheRepo
    :: MonadTokenStorage m
    => InstallationAccessToken
    -> HookUser
    -> HookRepositorySimple
    -> m ()
cacheRepo token owner repo = insertToken
    (Owner $ whUserLogin owner)
    (Repo $ whSimplRepoName repo)
    (iatToken token)

server :: Env -> Server HintmanAPI
server env@Env{..} = hoistServerWithContext
    (Proxy @HintmanAPI)
    (Proxy @'[ GitHubKey ])
    (runAppAsHandler env)
    (toServant hintmanServer)

hintmanApp :: Env -> Application
hintmanApp env@Env{..} = serveWithContext
    (Proxy @HintmanAPI)
    (envGitHubKey :. EmptyContext)
    (server env)
