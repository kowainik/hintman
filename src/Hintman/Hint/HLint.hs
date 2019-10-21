{- | This module introduces function to work with @HLint@ tool at modified files
in PRs.
-}

module Hintman.Hint.HLint
       ( getHLintHints

         -- * Internals
       , createComment
       , createCommentText
       ) where

import Language.Haskell.Exts.SrcLoc (srcSpanEnd, srcSpanStart)
import Language.Haskell.HLint4 (Classify, Idea (..), Note (..), ParseFlags, Severity (..),
                                applyHints, autoSettings, defaultParseFlags, findSettings,
                                getHLintDataDir, parseFlagsAddFixities, parseModuleEx,
                                readSettingsFile, resolveHints)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeExtension, (</>))
import Text.Diff.Parse.Types (FileDelta)

import Hintman.Core.Hint (Hint (..), HintType (HLint))
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..))
import Hintman.Download (downloadFile)
import Hintman.Hint.Position (getTargetCommentPosition, (!!?))

import qualified Data.Text as T
import qualified Language.Haskell.HLint4 as HLint (Hint)


{- | Run @HLint@ on all modified files.

For this we need:

1. Filter only `*.hs` extension files.
2. Run @Hlint@ tool on the content.
3. Return the list of all 'Idea's.
4. Create 'Comment's out of those 'Idea's.

-}
getHLintHints :: (MonadIO m, WithLog env m) => [ModifiedFile] -> m [Comment]
getHLintHints mfs = do
    let modFiles = filter ((==) ".hs" . takeExtension . mfPath) mfs
    foldMapM getFileHLintComments modFiles

-- | Create 'Comment's out of the file content
getFileHLintComments
    :: (MonadIO m, WithLog env m)
    => ModifiedFile
    -> m [Comment]
getFileHLintComments ModifiedFile{..} = let content = decodeUtf8 mfContent in do
    ideas <- getFileHLintSuggestions mfPath content
    pure $ mapMaybe (createComment mfDelta mfPath content) ideas


getFileHLintSuggestions :: (MonadIO m, WithLog env m) => FilePath -> Text -> m [Idea]
getFileHLintSuggestions fileName content = do
    (flags, classify, hint) <- customHLintSettings
    liftIO (parseModuleEx flags fileName (Just $ toString content)) >>= \case
        Right m -> pure $ applyHints classify hint [m]
        Left _err -> [] <$ log E "Hlint failed with some error" -- TODO: log err

-- | Create a 'Comment' from all necessary data.
createComment :: FileDelta -> FilePath -> Text -> Idea -> Maybe Comment
createComment fd (toText -> commentPath) content idea = do
    commentHint <- createCommentText content idea
    commentPosition <- getTargetCommentPosition fd commentPos
    Just Comment{..}
  where
    -- Real line number in the modified file.
    commentPos :: Int
    commentPos = fst $ srcSpanEnd $ ideaSpan idea


{- | Creates the comment text from the HLint 'Idea's and file content.
-}
createCommentText :: Text -> Idea -> Maybe Hint
createCommentText content Idea{..} = do
    to <- ideaTo
    guard $ ideaSeverity /= Ignore
    newLine <- buildWholeLine to
    Just $ Hint
        { hintHeader = if ideaHint == "" then "" else bold ideaSeverity <> ": " <> toText ideaHint
        , hintBody = newLine
        , hintIsSuggestion = isOneLiner
        , hintNote = showNotes ideaNote
        , hintType = HLint
        }
  where
    startLine, startP, endLine, endP :: Int
    (startLine, startP) = srcSpanStart ideaSpan
    (endLine, endP) = srcSpanEnd ideaSpan

    isOneLiner :: Bool
    isOneLiner = startLine == endLine

    bold :: Show a => a -> Text
    bold a = "**" <> show a <> "**"

    -- Replace only relevant part of the line.
    buildWholeLine :: String -> Maybe Text
    buildWholeLine (toText -> replacement) =
        if isOneLiner
        then lines content !!? (startLine - 1) >>= \original ->
            Just $ T.take (startP - 1) original <> replacement <> T.drop (endP - 1) original
        else Just replacement

    -- This function is copy-pasted from HLint sources.
    showNotes :: [Note] -> Text
    showNotes = T.intercalate ", " . map show . filter use
      where
        use :: Note -> Bool
        use ValidInstance{} = False -- Not important enough to tell an end user
        use _               = True

{- | These custom settings are required to make Hintman work on both deployed
Heroku build and locally. This function does the following:

1. First, uses default HLint location to find file (to work locally as before).
2. If file not found, uses it from the specified location (to work on prod).
-}
customHLintSettings
    :: forall m env .
       (MonadIO m, WithLog env m)
    => m (ParseFlags, [Classify], HLint.Hint)
customHLintSettings = do
    hlintDataDir <- liftIO getHLintDataDir
    let hlintDataFile = hlintDataDir </> "hlint.yaml"
    doesDataFileExist <- liftIO $ doesFileExist hlintDataFile
    if doesDataFileExist
        then liftIO autoSettings
        else downloadHints >> liftIO customSettings
  where
    -- datadir with hlint.yaml file on produnction
    prodDataDir :: FilePath
    prodDataDir = ".hlint-v2.2.2"

    -- download default hlint.yaml file to .hlint-data/hlint.yaml
    downloadHints :: m ()
    downloadHints = do
        liftIO $ createDirectoryIfMissing False prodDataDir
        let hlintUrl = "https://raw.githubusercontent.com/ndmitchell/hlint/v2.2.2/data/hlint.yaml"
        let hlintPath = prodDataDir </> "hlint.yaml"
        unlessM (liftIO $ doesFileExist hlintPath) $ downloadFile hlintUrl >>= \case
            Nothing   -> log E "Couldn't download hlint.yaml"
            Just file -> writeFileBS hlintPath file

    -- copy-pasted from HLint as recommended in the documentation
    customSettings :: IO (ParseFlags, [Classify], HLint.Hint)
    customSettings = do
        (fixities, classify, hints) <- findSettings (readSettingsFile $ Just prodDataDir) Nothing
        pure (parseFlagsAddFixities fixities defaultParseFlags, classify, resolveHints hints)
