{- | This module contains subset of all possible diff deltas that can
be handled by Hintman. Hintman processes the deltas that satisfy the
following criteria:

* 'Created' or 'Modified'
* Non-'Binary' files
-}

module Hintman.Core.Delta
       ( Delta (..)
       , fromFileDelta
       ) where

import Text.Diff.Parse.Types (Content (..), FileDelta (..), FileStatus (..), Hunk)


{- | Wrapper around relevant fields of 'FileDelta'.
-}
data Delta = Delta
    { deltaDestFile :: !Text
    , deltaHunks    :: ![Hunk]
    } deriving stock (Show)

{- | Converts 'FileDelta' from the @diff-parse@ package to 'Delta'
-}
fromFileDelta :: FileDelta -> Maybe Delta
fromFileDelta FileDelta{..} = do
    guard $ fileDeltaStatus == Created || fileDeltaStatus == Modified
    Hunks hunks <- Just fileDeltaContent
    pure Delta
        { deltaDestFile = fileDeltaDestFile
        , deltaHunks    = hunks
        }
