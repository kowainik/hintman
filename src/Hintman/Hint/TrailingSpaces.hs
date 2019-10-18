module Hintman.Hint.TrailingSpaces
       ( getTrailingSpacesComments
       , suggest
       ) where

import Data.Text (stripEnd)
import Hintman.Core.Hint (HintType (TrailingSpaces), Line (..), LineChange (..), Suggestion (..))
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..))


getTrailingSpacesComments :: [ModifiedFile] -> [Comment]
getTrailingSpacesComments _ = undefined

suggest :: FilePath -> [Line] -> [Suggestion]
suggest path = mapMaybe mSuggest
  where
    mSuggest :: Line -> Maybe Suggestion
    mSuggest l@Line{..} = if lineBody == stripped then Nothing
        else Just $ Suggestion path l (LineChange stripped Nothing TrailingSpaces)
      where
        stripped :: Text
        stripped = stripEnd lineBody
