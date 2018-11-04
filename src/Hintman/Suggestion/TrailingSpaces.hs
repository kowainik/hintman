module Hintman.Suggestion.TrailingSpaces
       ( suggest
       ) where

import Data.Text (stripEnd)
import Hintman.Suggestion.Core (ChangeType (..), Line (..), LineChange (..), Suggestion (..))


suggest :: FilePath -> [Line] -> [Suggestion]
suggest path = mapMaybe mSuggest
  where
    mSuggest :: Line -> Maybe Suggestion
    mSuggest l@Line{..} = if lineBody == stripped then Nothing
        else Just $ Suggestion path l (LineChange stripped Nothing Edit)
      where
        stripped :: Text
        stripped = stripEnd lineBody
