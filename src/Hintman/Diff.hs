{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{- | This modules contains function to fetch and work with git diffs.
-}

module Hintman.Diff
       ( fetchGitHubDiff
       ) where

import Network.HTTP.Client.TLS (newTlsManager)
import Servant (PlainText)
import Servant.Client (BaseUrl (..), ClientM, Scheme (Https), client, mkClientEnv, runClientM)
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types (FileDeltas)

import Hintman.Core.PrInfo (Owner, PrNumber, Repo)


{- | Endpoint that represents the following @curl@ command:

@
curl https://patch-diff.githubusercontent.com/raw/kowainik/hintman/pull/61.diff
@
-}
type GetPullRequestDiffApi =
    "raw"
    :> Capture "owner" Owner
    :> Capture "repo" Repo
    :> "pull"
    :> Capture "number" PrNumber
    :> Get '[PlainText] Text

-- | Client for 'GetPullRequestDiffApi'.
getPullRequestDiffClient :: Owner -> Repo -> PrNumber -> ClientM Text
getPullRequestDiffClient = client (Proxy @GetPullRequestDiffApi)

{- | Return parsed result of the PR diff.
-}
fetchGitHubDiff
    :: MonadIO m
    => Owner
    -> Repo
    -> PrNumber
    -> m (Either String FileDeltas)
fetchGitHubDiff owner repo prNum = liftIO $ do
    manager <- newTlsManager

    res <- runClientM
        (getPullRequestDiffClient owner repo prNum)
        (mkClientEnv manager $ BaseUrl Https "patch-diff.githubusercontent.com" 443 "")

    case res of
        Left err       -> pure $ Left $ show err
        Right response -> pure $ parseDiff response
