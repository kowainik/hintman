module Hintman.Suggestion.TrailingSpaces
       ( suggest
       ) where

import Data.Text (stripEnd)
import Hintman.Suggestion.Core (Line (..), LineChange (..), Suggestion (..))
import Hintman.Config (SuggestionType(..))


suggest :: FilePath -> [Line] -> [Suggestion]
suggest path = mapMaybe mSuggest
  where
    mSuggest :: Line -> Maybe Suggestion
    mSuggest l@Line{..} = if lineBody == stripped then Nothing
        else Just $ Suggestion path l (LineChange stripped Nothing TrailingSpaces)
      where
        stripped :: Text
        stripped = stripEnd lineBody
