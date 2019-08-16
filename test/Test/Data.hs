module Test.Data
       ( owner
       , repo
       , pr1
       , pr2
       ) where

import Hintman.Core.PrInfo (Branch (..), Owner (..), PrInfo (..), PrNumber (..), Repo (..))
import Hintman.Diff (fetchGitHubDiff)


owner :: Owner
owner = Owner "kowainik"

repo :: Repo
repo = Repo "hintman-target"

pr1, pr2 :: MonadIO m => m PrInfo
pr1 = makePr (PrNumber 2) (Branch "vrom911-patch-1")
pr2 = makePr (PrNumber 3) (Branch "vrom911-patch-2")

makePr :: MonadIO m => PrNumber -> Branch -> m PrInfo
makePr num branch = liftIO (fetchGitHubDiff owner repo num) >>= \case
    Left e -> error $ "Failed to fetch PR deltas: " <> toText e
    Right deltas -> pure PrInfo
        { prInfoOwner  = owner
        , prInfoRepo   = repo
        , prInfoBranch = branch
        , prInfoNumber = num
        , prInfoDelta  = deltas
        }
