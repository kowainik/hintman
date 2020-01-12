module Test.Data
       ( Prs (..)
       , fetchPrs
       , kowainik
       , repo

       , runLog
       , runWithFilter
       ) where

import Colog (LoggerT, richMessageAction, usingLoggerT)

import Hintman.Core.Hint (Hint (..), HintType)
import Hintman.Core.PrInfo (Owner (..), PrInfo (..), PrNumber (..), Repo (..), Sha (..))
import Hintman.Core.Review (Comment (..))
import Hintman.Diff (fetchGitHubDiff)
import Hintman.Hint (getAllComments)


data Prs = Prs
    { pr2  :: !PrInfo
    , pr3  :: !PrInfo
    , pr4  :: !PrInfo
    , pr24 :: !PrInfo
    , pr30 :: !PrInfo
    , pr36 :: !PrInfo
    , pr41 :: !PrInfo
    }

kowainik :: Owner
kowainik = Owner "kowainik"

repo :: Repo
repo = Repo "hintman-target"

fetchPrs :: IO Prs
fetchPrs = usingLoggerT richMessageAction $ do
    pr2  <- makePr (PrNumber 2) (Sha "vrom911-patch-1")
    pr3  <- makePr (PrNumber 3) (Sha "vrom911-patch-2")
    pr4  <- makePr (PrNumber 4) (Sha "chshersh/parse-error")
    pr24 <- makePr (PrNumber 24) (Sha "chshersh/change-one-in-big")
    pr30 <- makePr (PrNumber 30) (Sha "chshersh/test-hintman")
    pr36 <- makePr (PrNumber 36) (Sha "chshersh-patch-1")
    pr41 <- makePr (PrNumber 41) (Sha "no-newline-at-file-end")
    pure Prs{..}

makePr :: (WithLog env m, MonadIO m) => PrNumber -> Sha -> m PrInfo
makePr num branch = do
    log D $ "Fetching PR: " <> show (unPrNumber num)
    liftIO (fetchGitHubDiff kowainik repo num) >>= \case
        Left e -> error $ "Failed to fetch PR deltas: " <> toText e
        Right deltas -> pure PrInfo
            { prInfoOwner  = kowainik
            , prInfoRepo   = repo
            , prInfoHead   = branch
            , prInfoNumber = num
            , prInfoDeltas = deltas
            }

-- | Helper function to run tests.
runLog :: LoggerT Message IO a -> IO a
runLog = usingLoggerT mempty

runWithFilter :: HintType -> PrInfo -> IO [Comment]
runWithFilter ht prInfo = filter ((==) ht . hintType . commentHint)
    <$> runLog (getAllComments prInfo)
