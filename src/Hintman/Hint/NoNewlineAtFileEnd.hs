module Hintman.Hint.NoNewlineAtFileEnd
       ( getNoNewlineAtFileEndComments
       ) where

import Data.Vector (Vector)

import Hintman.Core.Hint (Hint, HintType (NoNewlineAtFileEnd), Line (..), simpleSuggestion)
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..), createComment)

import qualified Data.Text as T
import qualified Data.Vector as V


-- | Creates 'Comment's on adding newlines in all modified files.
getNoNewlineAtFileEndComments :: [ModifiedFile] -> [Comment]
getNoNewlineAtFileEndComments = mapMaybe getFileNoNewlineAtFileEndComment

{- | Takes a file and returns a comment if file doesn't end with a '\n'.
The suggestion text for the line in comment is the context of the line with '\n' appended. -}
getFileNoNewlineAtFileEndComment :: ModifiedFile -> Maybe Comment
getFileNoNewlineAtFileEndComment mf@ModifiedFile{..} = lastLine >>= spawnComment
  where
    lastLine :: Maybe Line
    lastLine = maybeGetNonEmptyLastLine mfLines mfContent

    {- | Spawns a "NoNewlineAtFileEnd" comment on a given line, suggesting the same
    line, but with '\n' appended. -}
    spawnComment :: Line -> Maybe Comment
    spawnComment line = createComment mf line $ createHint line

    createHint :: Line -> Hint
    createHint line = simpleSuggestion NoNewlineAtFileEnd "No newline at end of file" $ suggestionText line

    suggestionText :: Line -> Text
    suggestionText Line{..} = lineBody <> "\n"

{- | Checks if file ends with a newline, and if it does not, returns the last line (to spawn warning on).

More precise explanation of the logic within function:

First, we obtain the last symbol of file. If file is empty, the execution stops and 'Nothing' is returned.
Then we check if the last symbol is '\n'. If it is, we return 'Nothing'.
Otherwise, we take the last line of file and return it.

An important note here is that result of lines will trim a '\n' symbol from the last line, so
we can't just check the last symbol of line, we have to check last symbol of raw content).
-}
maybeGetNonEmptyLastLine :: Vector Line -> Text -> Maybe Line
maybeGetNonEmptyLastLine mfLines fileContent = do
    -- 1. Get the last symbol of file. If file is empty, the function will be executed no further.
    lastSymbol <- getLastSymbol fileContent

    -- 2. Return line for warning creation if the last symbol isn't '\n'.
    maybeLineForWarning lastSymbol
  where
    -- | Takes last symbol of provided 'Text' (if there is any).
    getLastSymbol :: Text -> Maybe Char
    getLastSymbol text
        | T.null text = Nothing
        | otherwise = Just $ T.last text

    -- | Returns a last line of file if the last character isn't '\n'.
    maybeLineForWarning :: Char -> Maybe Line
    maybeLineForWarning lastChar
        | lastChar == '\n' = Nothing
        | otherwise = Just $ V.last mfLines
