module Test.Data
       ( owner
       , repo
       , pr1
       , pr2
       , pr3
       , pr24
       ) where

import Hintman.Core.PrInfo (Owner (..), PrInfo (..), PrNumber (..), Repo (..), Sha (..))
import Hintman.Diff (fetchGitHubDiff)


owner :: Owner
owner = Owner "kowainik"

repo :: Repo
repo = Repo "hintman-target"

pr1, pr2, pr3, pr24 :: MonadIO m => m PrInfo
pr1 = makePr (PrNumber 2) (Sha "vrom911-patch-1")
pr2 = makePr (PrNumber 3) (Sha "vrom911-patch-2")
pr3 = makePr (PrNumber 4) (Sha "chshersh/parse-error")
pr24 = makePr (PrNumber 24) (Sha "chshersh/change-one-in-big")

makePr :: MonadIO m => PrNumber -> Sha -> m PrInfo
makePr num branch = liftIO (fetchGitHubDiff owner repo num) >>= \case
    Left e -> error $ "Failed to fetch PR deltas: " <> toText e
    Right deltas -> pure PrInfo
        { prInfoOwner  = owner
        , prInfoRepo   = repo
        , prInfoHead   = branch
        , prInfoNumber = num
        , prInfoDelta  = deltas
        }
