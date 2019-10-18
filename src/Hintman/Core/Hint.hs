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
    = HLint
    | TrailingSpaces
    | TrailingNewlines
    deriving (Eq, Read, Show, Bounded, Enum)

showHintType :: HintType -> Text
showHintType = \case
    HLint            -> "hlint"
    TrailingSpaces   -> "trailing-spaces"
    TrailingNewlines -> "trailing-newlines"
{-# INLINE showHintType #-}

parseHintType :: Text -> Maybe HintType
parseHintType = inverseMap showHintType
{-# INLINE parseHintType #-}



data Line = Line
    { lineNumber :: !Int
    , lineBody   :: !Text
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
