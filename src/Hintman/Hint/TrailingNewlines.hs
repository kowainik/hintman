module Hintman.Hint.TrailingNewlines
       ( getTrailingNewlinesComments
       ) where

import Hintman.Core.Hint (HintType (TrailingNewlines), Line (..), toLines)
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..))
import Hintman.Hint.Position (getTargetCommentPosition)

import qualified Data.Text as T


-- | Creates 'Comment's on removing trailing newlines in all modified files.
getTrailingNewlinesComments :: [ModifiedFile] -> [Comment]
getTrailingNewlinesComments = foldMap getFileTrailingNewlineComments

getFileTrailingNewlineComments :: ModifiedFile -> [Comment]
getFileTrailingNewlineComments ModifiedFile{..} = maybeToMonoid $
    mapMaybe createComment . getTrailingSpaceLines . toLines . decodeUtf8
    <$> mfContent
  where
    -- TODO: remove duplication with 'getTrailingSpacesComments'
    -- | Create a 'Comment' from all necessary data.
    createComment :: Line -> Maybe Comment
    createComment Line{..} = do
        let commentPath = fileNameText
        commentPosition <- getTargetCommentPosition mfDelta lineNumber
        let commentBody = unlines [ "```suggestion", "```" ]
        let commentHintType = TrailingNewlines
        Just Comment{..}

    fileNameText :: Text
    fileNameText = toText mfPath

-- | Take only empty lines from the end of the file
getTrailingSpaceLines :: [Line] -> [Line]
getTrailingSpaceLines = takeWhile isEmptyLine . reverse
  where
    isEmptyLine :: Line -> Bool
    isEmptyLine = T.null . T.strip . lineBody

