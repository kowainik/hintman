module Hintman.Suggestion.TrailingNewline
       ( suggest
       ) where

import Hintman.Config (SuggestionType (..))
import Hintman.Suggestion.Core (Line (..), LineChange (..), Suggestion (..))

import qualified Data.Text as T


suggest :: FilePath -> [Line] -> [Suggestion]
suggest path = generateResult . trailingNewLines . reverse
  where
    generateResult :: [Line] -> [Suggestion]
    generateResult = map
        (\l -> Suggestion path l (LineChange T.empty Nothing TrailingNewlines))

    trailingNewLines :: [Line] -> [Line]
    trailingNewLines = takeWhile isNewline

    isNewline :: Line -> Bool
    isNewline = T.null . T.strip . lineBody
