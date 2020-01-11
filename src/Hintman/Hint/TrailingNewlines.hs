module Hintman.Hint.TrailingNewlines
       ( getTrailingNewlinesComments
       ) where

import Data.Vector (Vector)

import Hintman.Core.Hint (Hint, HintType (TrailingNewlines), Line (..), simpleSuggestion)
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..), createComment)

import qualified Data.Text as T
import qualified Data.Vector as V


-- | Creates 'Comment's on removing trailing newlines in all modified files.
getTrailingNewlinesComments :: [ModifiedFile] -> [Comment]
getTrailingNewlinesComments = foldMap getFileTrailingNewlineComments

getFileTrailingNewlineComments :: ModifiedFile -> [Comment]
getFileTrailingNewlineComments mf@ModifiedFile{..} = mapMaybe spawnComment $
    V.toList $ getTrailingNewlines mfLines
  where
    -- | Spawns a "TrailingNewlines" comment on a given line.
    spawnComment :: Line -> Maybe Comment
    spawnComment line = createComment mf line hint

    hint :: Hint
    hint = simpleSuggestion TrailingNewlines "Trailing newline" ""

-- | Take only empty lines from the end of the file
getTrailingNewlines :: Vector Line -> Vector Line
getTrailingNewlines = V.takeWhile isEmptyLine . V.reverse
  where
    isEmptyLine :: Line -> Bool
    isEmptyLine = T.null . T.strip . lineBody
