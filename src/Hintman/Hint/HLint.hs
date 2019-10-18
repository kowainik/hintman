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
import Text.Diff.Parse.Types (Annotation (..), Content (..), FileDelta (..), Hunk (..), Line (..),
                              Range (..))

import Hintman.Core.Hint (HintType (HLint))
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..))

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

{- | Line numbers in the file (modified by PR) and in the 'FileDelta' are
are different as they have different meaning.

HLint is working with the whole file and gives us the information about
corresponding line in the file, while @git-diff@ only could show the modified
chunks of lines. To leave a comment we can only apply them to the modified
lines, so we need to know there number in the @git-diff@.

This function is making this translation for you. Given a 'FileDelta' and the
'Idea's last comment line number it returns the corresponding number of
'FileDelta' which matches the last line of the HLint comment.

Note that we only will leave comments on 'Added' lines, not 'Removed' or 'Context'.

If we have this modification in the file:

@
myFileDelta = FileDelta
    { fileDeltaStatus = Modified
    , fileDeltaSourceFile = "file.example"
    , fileDeltaDestFile = "file.example"
    , fileDeltaContent = Hunks
        [ Hunk
            { hunkSourceRange = Range
                { rangeStartingLineNumber = 3
                , rangeNumberOfLines = 2
                }
            , hunkDestRange = Range
                { rangeStartingLineNumber = 3
                , rangeNumberOfLines = 2
                }
            , hunkLines =
                [ Line Added "Line number 3"
                , Line Context "Line number 4"
                ]
            ...
@

then it could only add HLint comment to lines 3 and 4 but not any other.

>>> getTargetCommentPosition myFileDelta 3
Just 0
>>> getTargetCommentPosition myFileDelta 4
Nothing
>>> getTargetCommentPosition myFileDelta 5
Nothing
-}
getTargetCommentPosition :: FileDelta -> Int -> Maybe Int
getTargetCommentPosition FileDelta{..} commentPos = case fileDeltaContent of
    Binary   -> Nothing
    Hunks hs -> goHunks hs
  where
    -- Find the position number i the hunk.
    -- Stops on the first satisfying.
    goHunks :: [Hunk] -> Maybe Int
    goHunks [] = Nothing
    goHunks (Hunk{..}:hs) = case inRange hunkDestRange of
        Just diffLine -> hunkLines !!? diffLine >>= \line ->
            -- Only 'Added' lines.
            if lineAnnotation line == Added
            then Just (diffLine + 1)
            else Nothing
        Nothing -> goHunks hs
      where
        -- | Check is the desired line number belongs to this Hunk
        -- and return the corresponding number in the Hunk.
        inRange :: Range -> Maybe Int
        inRange Range{..} =
            if (rangeStartingLineNumber <= commentPos)
                && (commentPos <= (rangeStartingLineNumber + rangeNumberOfLines))
            then Just $ commentPos - rangeStartingLineNumber
            else Nothing

-- | Safe @at@ function.
(!!?) :: [a] -> Int -> Maybe a
l !!? index
    | index < 0 = Nothing
    | otherwise = f index l
  where
    f :: Int -> [a] -> Maybe a
    f 0 (x:_)  = Just x
    f i (_:xs) = f (i - 1) xs
    f _ []     = Nothing
