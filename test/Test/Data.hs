module Test.Data
       ( Prs (..)
       , fetchPrs
       , owner
       , repo

       , runLog
       ) where

import Colog (LoggerT, richMessageAction, usingLoggerT)

import Hintman.Core.PrInfo (Owner (..), PrInfo (..), PrNumber (..), Repo (..), Sha (..))
import Hintman.Diff (fetchGitHubDiff)


data Prs = Prs
    { pr1  :: !PrInfo
    , pr2  :: !PrInfo
    , pr3  :: !PrInfo
    , pr24 :: !PrInfo
    , pr30 :: !PrInfo
    }

owner :: Owner
owner = Owner "kowainik"

repo :: Repo
repo = Repo "hintman-target"

fetchPrs :: IO Prs
fetchPrs = usingLoggerT richMessageAction $ do
    pr1  <- makePr (PrNumber 2) (Sha "vrom911-patch-1")
    pr2  <- makePr (PrNumber 3) (Sha "vrom911-patch-2")
    pr3  <- makePr (PrNumber 4) (Sha "chshersh/parse-error")
    pr24 <- makePr (PrNumber 24) (Sha "chshersh/change-one-in-big")
    pr30 <- makePr (PrNumber 30) (Sha "chshersh/test-hintman")
    pure Prs{..}

makePr :: (WithLog env m, MonadIO m) => PrNumber -> Sha -> m PrInfo
makePr num branch = do
    log D $ "Fetching PR: " <> show (unPrNumber num)
    liftIO (fetchGitHubDiff owner repo num) >>= \case
        Left e -> error $ "Failed to fetch PR deltas: " <> toText e
        Right deltas -> pure PrInfo
            { prInfoOwner  = owner
            , prInfoRepo   = repo
            , prInfoHead   = branch
            , prInfoNumber = num
            , prInfoDelta  = deltas
            }

-- | Helper function to run tests.
runLog :: LoggerT Message IO a -> IO a
runLog = usingLoggerT mempty
