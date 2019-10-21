{- | This module contains function that creates all comments for the PR review.

Currently supported features:

* HLint suggestions
* Trailing spaces removal
-}

module Hintman.Hint
       ( getAllComments
       ) where

import Text.Diff.Parse.Types (FileDelta (..), FileStatus (..))

import Hintman.Core.PrInfo (ModifiedFile (..), Owner (..), PrInfo (..), Repo (..), Sha (..))
import Hintman.Core.Review (Comment)
import Hintman.Download (downloadFile)
import Hintman.Hint.HLint (getHLintHints)
import Hintman.Hint.TrailingNewlines (getTrailingNewlinesComments)
import Hintman.Hint.TrailingSpaces (getTrailingSpacesComments)

import qualified Data.Text as T


{- | Get all review comments of all supported types for the PR.
-}
getAllComments :: (MonadIO m, WithLog env m) => PrInfo -> m [Comment]
getAllComments prInfo = do
    -- 1. Get all modified files
    modFiles <- getModifiedFiles prInfo

    -- 2. Get all suggestions
    let trailingSpaces = getTrailingSpacesComments modFiles
    let trailingNewlines = getTrailingNewlinesComments modFiles
    hlintHints <- getHLintHints modFiles

    -- 3. return combined result
    pure $ trailingSpaces ++ trailingNewlines ++ hlintHints


-- | Get all modified files in the PR.
getModifiedFiles
    :: forall m env . (MonadIO m, WithLog env m)
    => PrInfo
    -> m [ModifiedFile]
getModifiedFiles prInfo@PrInfo{..} = traverse toModifiedFile $ filter ((/=) Deleted . fileDeltaStatus) prInfoDelta
  where
    toModifiedFile :: FileDelta -> m ModifiedFile
    toModifiedFile mfDelta@FileDelta{..} = do
        let mfPath = toString fileDeltaDestFile
        -- TODO: store 'ByteString' here instead of 'Maybe ByteString'
        -- if we can't retrieve file content, can as well just ignore it
        mfContent <- downloadFile $ createFileDownloadUrl prInfo mfPath
        pure ModifiedFile{..}

{- | Create an URL for downloading file content. The URL is in the following
form:

@
https://raw.githubusercontent.com/owner/repo/sha/FILE_NAME
@
-}
createFileDownloadUrl
    :: PrInfo
    -> FilePath  -- ^ File name
    -> Text  -- ^ Resulting URL
createFileDownloadUrl PrInfo{..} (toText -> file) = T.intercalate "/"
    [ "https://raw.githubusercontent.com"
    , unOwner prInfoOwner
    , unRepo prInfoRepo
    , unSha prInfoHead
    , file
    ]
