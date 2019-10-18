{- | This module contains function that creates all comments for the PR review.

Currently supported features:

* HLint suggestions
* Trailing spaces removal
-}

module Hintman.Hint
       ( getAllComments
       ) where

import Network.HTTP.Client (Response (..), httpLbs)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))
import Text.Diff.Parse.Types (FileDelta (..), FileStatus (..))

import Hintman.Core.PrInfo (ModifiedFile (..), Owner (..), PrInfo (..), Repo (..), Sha (..))
import Hintman.Core.Review (Comment)
import Hintman.Hint.HLint (getHLintHints)
import Hintman.Hint.TrailingSpaces (getTrailingSpacesComments)

import qualified Data.Text as T


{- | Get all review comments of all supported types for the PR.
-}
getAllComments :: (MonadIO m, WithLog env m) => PrInfo -> m [Comment]
getAllComments prInfo = do
    modFiles <- getModifiedFiles prInfo
    (getTrailingSpacesComments modFiles ++) <$> getHLintHints modFiles


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
        mfContent <- downloadFile $ createFileDownloadUrl prInfo mfPath
        pure ModifiedFile{..}


-- | In-memory file download from GitHub PR sources by given URL.
downloadFile :: (MonadIO m, WithLog env m) => Text -> m (Maybe ByteString)
downloadFile url = do
    -- TODO: put in the context to not create each time
    man <- newTlsManager
    let req = fromString $ toString url
    log I $ "Attempting to download file from " <> url <> " ..."
    response <- liftIO $ httpLbs req man
    let status = statusCode $ responseStatus response
    let body = responseBody response
    log D $ "Recieved a status code of " <> show status <> " from " <> url
    case status of
        200 -> do
            log I $ "Successfully downloaded file from " <> url
            pure $ Just $ toStrict body
        _   -> do
            log E $ "Couldn't download file from " <> url
            pure Nothing


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
