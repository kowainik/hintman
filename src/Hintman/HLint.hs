{- | This module introduces function to work with @HLint@ tool at modified files
in PRs.
-}

module Hintman.HLint
       ( runHLint
       , createComment
       ) where

import Language.Haskell.HLint4 (Idea (..), Note (..), Severity (..), applyHints, autoSettings,
                                parseModuleEx)
import Network.HTTP.Client (Response (..), httpLbs)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))
import Relude.Extra.Tuple (traverseToSnd)
import System.FilePath (takeExtension)
import Text.Diff.Parse.Types (FileDelta (..), FileDeltas, FileStatus (..))

import Hintman.Core.PrInfo (Branch (..), Owner (..), PrInfo (..), Repo (..))

import qualified Data.Text as T


{- | Run @HLint@ on all modified files.

For this we need:

1. Filter all modified files.
2. Filter only `*.hs` extension files.
3. Get the content of all modified files.
4. Run @Hlint@ tool on the content.
5. Return the list of all 'Idea's.

-}
runHLint :: (MonadIO m, WithLog env m) => PrInfo -> m [Idea]
runHLint prInfo@PrInfo{..} = do
    let modFiles = filter ((==) ".hs" . takeExtension) $ getModifiedFiles prInfoDelta
    let getContent = downloadFile . createFileDownloadUrl prInfo
    fileContent <- traverse (traverseToSnd getContent) modFiles
    foldMapM getFileHLintSuggestions fileContent

-- | Get all modified files in the PR.
getModifiedFiles :: FileDeltas -> [FilePath]
getModifiedFiles = map (toString . fileDeltaDestFile) . filter ((/=) Deleted . fileDeltaStatus)


getFileHLintSuggestions :: (MonadIO m, WithLog env m) => (FilePath, Maybe ByteString) -> m [Idea]
getFileHLintSuggestions (fileName, content) = case content of
    Nothing -> [] <$ log I ("Content is empty: " <> toText fileName)
    Just (decodeUtf8 -> c) -> do
        (flags, classify, hint) <- liftIO autoSettings
        liftIO (parseModuleEx flags fileName (Just c)) >>= \case
            Right m -> pure $ applyHints classify hint [m]
            Left _err -> [] <$ log E "Hlint failed with some error" -- TODO: log err

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
https://raw.githubusercontent.com/owner/repo/branch/FILE_NAME
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
    , unBranch prInfoBranch
    , file
    ]

{- | Creates the comment text from the HLint 'Idea's.
-}
createComment :: Idea -> Maybe Text
createComment Idea{..} = ideaTo >>= \to ->
    if ideaSeverity == Ignore
    then Nothing
    else Just $ unlines $
        (if ideaHint == "" then "" else show ideaSeverity <> ": " <> toText ideaHint)
        : mkSuggestion to
       ++ [ "Note: " <> n | let n = showNotes ideaNote, n /= ""]
  where
    mkSuggestion :: String -> [Text]
    mkSuggestion to =
        [ "```suggestion"
        , toText to
        , "```"
        ]

    -- This function is copy-pasted from HLint sources.
    showNotes :: [Note] -> Text
    showNotes = T.intercalate ", " . map show . filter use
      where
        use :: Note -> Bool
        use ValidInstance{} = False -- Not important enough to tell an end user
        use _               = True
