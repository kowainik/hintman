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
import Language.Haskell.HLint4 (Idea (..), Note (..), Severity (..), applyHints, autoSettings,
                                parseModuleEx)
import System.FilePath (takeExtension)
import Text.Diff.Parse.Types (FileDelta (..))

import Hintman.Core.Hint (HintType (HLint))
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..))
import Hintman.Hint.Position (getTargetCommentPosition, (!!?))

import qualified Data.Text as T


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
getFileHLintComments ModifiedFile{..} = case mfContent of
    Nothing -> [] <$ log I ("Content is empty: " <> fileNameText)
    Just (decodeUtf8 -> content) -> do
        ideas <- getFileHLintSuggestions mfPath content
        pure $ mapMaybe (createComment mfDelta mfPath content) ideas
  where
    fileNameText :: Text
    fileNameText = toText mfPath


getFileHLintSuggestions :: (MonadIO m, WithLog env m) => FilePath -> Text -> m [Idea]
getFileHLintSuggestions fileName content = do
    (flags, classify, hint) <- liftIO autoSettings
    liftIO (parseModuleEx flags fileName (Just $ toString content)) >>= \case
        Right m -> pure $ applyHints classify hint [m]
        Left _err -> [] <$ log E "Hlint failed with some error" -- TODO: log err

-- | Create a 'Comment' from all necessary data.
createComment :: FileDelta -> FilePath -> Text -> Idea -> Maybe Comment
createComment fd (toText -> commentPath) content idea = do
    commentBody <- createCommentText content idea
    commentPosition <- getTargetCommentPosition fd commentPos
    let commentHintType = HLint
    Just Comment{..}
  where
    -- Real line number in the modified file.
    commentPos :: Int
    commentPos = fst $ srcSpanEnd $ ideaSpan idea


{- | Creates the comment text from the HLint 'Idea's and file content.
-}
createCommentText :: Text -> Idea -> Maybe Text
createCommentText content Idea{..} = do
    to <- ideaTo
    guard $ ideaSeverity /= Ignore
    newLine <- buildWholeLine to
    Just $ unlines $
        (if ideaHint == "" then "" else show ideaSeverity <> ": " <> toText ideaHint)
        : mkSuggestion newLine
       ++ [ "Note: " <> n | let n = showNotes ideaNote, n /= ""]
  where
    startLine, startP, endLine, endP :: Int
    (startLine, startP) = srcSpanStart ideaSpan
    (endLine, endP) = srcSpanEnd ideaSpan

    isOneLiner :: Bool
    isOneLiner = startLine == endLine

    mkSuggestion :: Text -> [Text]
    mkSuggestion to =
        [ "```" <> if isOneLiner then "suggestion" else ""
        , to
        , "```"
        ]

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
