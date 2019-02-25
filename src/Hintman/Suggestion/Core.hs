module Hintman.Suggestion.Core
       ( Line (..)
       , LineChange (..)
       , Suggestion (..)
       , toLines
       ) where

import Hintman.Config (SuggestionType (..))


data Line = Line
    { lineRow  :: Int
    , lineBody :: Text
    } deriving (Eq, Show)

data Suggestion = Suggestion
    { suggestionFile   :: FilePath
    , suggestionLine   :: Line
    , suggestionChange :: LineChange
    } deriving (Eq, Show)

data LineChange = LineChange
    { lineChangeNew     :: Text
    , lineChangeComment :: Maybe Text
    , lineChangeType    :: SuggestionType
    } deriving (Eq, Show)

toLines :: Text -> [Line]
toLines txt = map (\(lineRow, lineBody) -> Line{..}) numberedLines
  where
    numberedLines :: [(Int, Text)]
    numberedLines = zip [1..] (lines txt)

