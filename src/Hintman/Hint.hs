{- | This module contains function that creates all comments for the PR review.

Currently supported features:

* HLint suggestions
* Trailing spaces removal
-}

module Hintman.Hint
       ( getAllComments
       ) where

import Text.Diff.Parse.Types (Content (..), FileDelta (..), FileStatus (..))

import Hintman.Core.Hint (toLines)
import Hintman.Core.PrInfo (ModifiedFile (..), Owner (..), PrInfo (..), Repo (..), Sha (..))
import Hintman.Core.Review (Comment)
import Hintman.Download (downloadFile)
import Hintman.Hint.HLint (getHLintHints)
import Hintman.Hint.NoNewlineAtFileEnd (getNoNewlineAtFileEndComments)
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
    let noNewlinesAtFileEnd = getNoNewlineAtFileEndComments modFiles
    hlintHints <- getHLintHints modFiles

    -- 3. return combined result
    pure $ trailingSpaces ++ trailingNewlines ++ noNewlinesAtFileEnd ++ hlintHints


-- | Get all modified files in the PR.
getModifiedFiles
    :: forall m env . (MonadIO m, WithLog env m)
    => PrInfo
    -> m [ModifiedFile]
getModifiedFiles prInfo@PrInfo{..} = fmap catMaybes $ traverse toModifiedFile $ filesForReview prInfoDelta
  where
    -- | Sifts diffs, removing binary files and files that were removed.
    filesForReview :: [FileDelta] -> [FileDelta]
    filesForReview = filter (\file -> (fileDeltaStatus file /= Deleted) && (fileDeltaContent file /= Binary))

    toModifiedFile :: FileDelta -> m (Maybe ModifiedFile)
    toModifiedFile mfDelta@FileDelta{..} = do
        let mfPath = toString fileDeltaDestFile
        maybeContent <- downloadFile $ createFileDownloadUrl prInfo mfPath
        case maybeContent of
            Nothing        -> Nothing <$ log I ("Content is empty: " <> fileDeltaDestFile)
            Just byteContent -> do
                let mfContent = decodeUtf8 byteContent
                let mfLines = fromList $ toLines mfContent
                pure $ Just ModifiedFile{..}

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
