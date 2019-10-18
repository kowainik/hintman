module Hintman.Core.Hint
       ( HintType (..)
       , showHintType
       , parseHintType


       , Line (..)
       , LineChange (..)
       , Suggestion (..)
       , toLines
       ) where

import Relude.Extra.Enum (inverseMap)


data HintType
    = TrailingSpaces
    | TrailingNewlines
    | HLint
    deriving (Eq, Read, Show, Bounded, Enum)

showHintType :: HintType -> Text
showHintType = \case
    TrailingSpaces   -> "trailing-spaces"
    TrailingNewlines -> "trailing-newlines"
    HLint            -> "hlint"

parseHintType :: Text -> Maybe HintType
parseHintType = inverseMap showHintType



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
    , lineChangeType    :: HintType
    } deriving (Eq, Show)

toLines :: Text -> [Line]
toLines = zipWith Line [1..] . lines
