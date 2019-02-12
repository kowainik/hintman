{- |
Aggregator for all automatic hint we did.
Now it deletes:
 - trailing newlines
 - trailing spaces
-}

module Hintman.Suggestion.Aggregation
       ( aggregate
       ) where

import Hintman.Suggestion.Core (Line (..), Suggestion (..), toLines)

import qualified Hintman.Suggestion.TrailingNewline as STN
import qualified Hintman.Suggestion.TrailingSpaces as STS


-- | Add suggestions to given text.
aggregate :: FilePath -> Text -> [Suggestion]
aggregate path txt = deleteSuggest ++ editSuggest
  where
    -- First, aggregate deletes trailingNewLines
    deleteSuggest :: [Suggestion]
    deleteSuggest = STN.suggest path lineList

    deletedLines :: [Line]
    deletedLines = map suggestionLine deleteSuggest
    -- Second, it consumes the rest lines to suggest edit.
    toEditLines :: [Line]
    toEditLines = filter (`notElem` deletedLines) lineList

    editSuggest :: [Suggestion]
    editSuggest = STS.suggest path toEditLines

    lineList :: [Line]
    lineList = toLines txt
