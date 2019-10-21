module Hintman.Core.Hint
       ( HintType (..)
       , showHintType
       , parseHintType

       , Line (..)
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

-- | Convert multiline text to the list of lines.
toLines :: Text -> [Line]
toLines = zipWith Line [1..] . lines
