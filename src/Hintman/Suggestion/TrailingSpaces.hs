module Hintman.Suggestion.TrailingSpaces
       ( suggest
       ) where

import Data.Text (stripEnd)
import Hintman.Suggestion.Core (Line (..), Suggest (..))


suggest :: FilePath -> [Line] -> [Suggest]
suggest path = mapMaybe mSuggest
  where
    mSuggest :: Line -> Maybe Suggest
    mSuggest Line{..} = if lineBody == stripped then Nothing
        else Just Suggest {suggestFile = path, suggestNew = stripped, suggestComment = Nothing}
      where
        stripped :: Text
        stripped = stripEnd lineBody
