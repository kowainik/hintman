module Hintman.Hint.TrailingNewlines
       ( getTrailingNewlinesComments
       ) where

import Hintman.Core.Hint (Hint, HintType (TrailingNewlines), Line (..), simpleSuggestion, toLines)
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..), createComment)

import qualified Data.Text as T


-- | Creates 'Comment's on removing trailing newlines in all modified files.
getTrailingNewlinesComments :: [ModifiedFile] -> [Comment]
getTrailingNewlinesComments = foldMap getFileTrailingNewlineComments

getFileTrailingNewlineComments :: ModifiedFile -> [Comment]
getFileTrailingNewlineComments mf@ModifiedFile{..} = mapMaybe spawnComment $
    getTrailingSpaceLines $ toLines $ decodeUtf8 mfContent
  where
    -- | Spawns a "TrailingNewlines" comment on a given line.
    spawnComment :: Line -> Maybe Comment
    spawnComment line = createComment mf line hint

    hint :: Hint
    hint = simpleSuggestion TrailingNewlines "Trailing newline" ""

-- | Take only empty lines from the end of the file
getTrailingSpaceLines :: [Line] -> [Line]
getTrailingSpaceLines = takeWhile isEmptyLine . reverse
  where
    isEmptyLine :: Line -> Bool
    isEmptyLine = T.null . T.strip . lineBody
