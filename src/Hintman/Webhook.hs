{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hintman.Webhook
       ( hintmanApp
       ) where

import GitHub.Data.Webhooks.Events (InstallationEvent (..), InstallationEventAction (..),
                                    InstallationRepoEventAction (..),
                                    InstallationRepositoriesEvent (..), PullRequestEvent (..),
                                    PullRequestEventAction (..))
import GitHub.Data.Webhooks.Payload (HookInstallation (..), HookPullRequest (..),
                                     HookRepository (..), HookSimpleUser (..), HookUser (..),
                                     PullRequestTarget (..))
import Servant (Application, Context ((:.), EmptyContext), Server, hoistServerWithContext,
                serveWithContext)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))
import Servant.Server.Generic (AsServerT)

import Hintman.App (App, AppEnv, Env (..), Has, WithError, runAppAsHandler)
import Hintman.Comment (submitReview)
import Hintman.Core.Key (GitHubKey)
import Hintman.Core.PrInfo (Owner (..), PrInfo (..), PrNumber (..), Repo (..), Sha (..))
import Hintman.Core.Token (AppInfo, InstallationId (..), InstallationToken (..))
import Hintman.Diff (fetchGitHubDiff)
import Hintman.Effect.TokenStorage (MonadTokenStorage (..), acquireInstallationToken)
import Hintman.HLint (runHLint)
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

issueCommentHook
    :: forall env m .
       ( MonadTokenStorage m
       , MonadUnliftIO m
       , WithError m
       , WithLog env m
       , Has AppInfo env
       )
    => RepoWebhookEvent
    -> ((), PullRequestEvent)
    -> m ()
issueCommentHook _ ((), ev) = case evPullReqAction ev of
    PullRequestOpenedAction -> hintmanAction
    _                       -> pass
  where
    hintmanAction :: m ()
    hintmanAction = do
        let repo = evPullReqRepo ev
        let prInfoOwner = Owner $ getRepoOwner $ whRepoOwner repo
        let prInfoRepo = Repo $ whRepoName repo
        let prInfoHead = Sha $ whPullReqTargetSha $ whPullReqHead $ evPullReqPayload ev
        let prInfoNumber = PrNumber $ evPullReqNumber ev

        fetchGitHubDiff prInfoOwner prInfoRepo prInfoNumber >>= \case
            Left err -> log E $ "Failed to fetch PR deltas: " <> toText err
            Right prInfoDelta -> do
                let prInfo = PrInfo{..}
                comments <- runHLint prInfo
                token <- itToken <$> acquireInstallationToken prInfoOwner
                submitReview token prInfo comments

    getRepoOwner :: Either HookSimpleUser HookUser -> Text
    getRepoOwner = either whSimplUserName whUserLogin

appInstalledHook
    :: (MonadIO m, MonadTokenStorage m, Has AppInfo env, WithError m, WithLog env m)
    => RepoWebhookEvent
    -> ((), InstallationEvent)
    -> m ()
appInstalledHook _ ((), ev) = do
    log D "'InstallationEvent' triggered"
    let installationId = InstallationId $ whInstallationId $ evInstallationInfo ev
    let owner = Owner $ whUserLogin $ whInstallationAccount $ evInstallationInfo ev

    case evInstallationAction ev of
        InstallationCreatedAction -> do
            log I $ "Installing app for: " <> unOwner owner
            cacheInstallation owner installationId
        InstallationDeletedAction -> do
            log I $ "Deleting app for: " <> unOwner owner
            deleteToken owner
        InstallationActionOther msg ->
            log W $ "Other installation action: " <> msg

repoInstalledHook
    :: (MonadIO m, MonadTokenStorage m, Has AppInfo env, WithError m, WithLog env m)
    => RepoWebhookEvent
    -> ((), InstallationRepositoriesEvent)
    -> m ()
repoInstalledHook _ ((), ev) = do
    log D "'InstallationRepositoriesEvent' triggered"
    let installationId = InstallationId $ whInstallationId $ evInstallationRepoInfo ev
    let owner = Owner $ whUserLogin $ whInstallationAccount $ evInstallationRepoInfo ev

    case evInstallationRepoAction ev of
        InstallationRepoCreatedAction -> do
            log D $ "Installing app for repos and user: " <> unOwner owner
            cacheInstallation owner installationId
        InstallationRepoRemovedAction ->
            log I $ "Some repo(s) deleted for user: " <> unOwner owner
        InstallationRepoActionOther msg ->
            log W $ "Other repo installation action: " <> msg

cacheInstallation
    :: ( MonadTokenStorage m
       , MonadIO m
       , Has AppInfo env
       , WithError m
       , WithLog env m
       )
    => Owner
    -> InstallationId
    -> m ()
cacheInstallation owner installationId = do
    jwtToken <- mkJwtToken
    token <- createInstallationToken jwtToken installationId
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
