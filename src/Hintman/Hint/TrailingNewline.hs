module Hintman.Hint.TrailingNewline
       ( getTrailingNewlineComments

       , suggest
       ) where

import Hintman.Core.Hint (HintType (TrailingNewlines), Line (..), LineChange (..), Suggestion (..))
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..))

import qualified Data.Text as T


getTrailingNewlineComments :: [ModifiedFile] -> [Comment]
getTrailingNewlineComments _ = undefined

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
