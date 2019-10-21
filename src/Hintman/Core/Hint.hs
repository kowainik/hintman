{- | This module contains data types and helpers for work with different types
of hints.
-}
module Hintman.Core.Hint
       ( Hint (..)
       , formatHint
       , HintType (..)
       , showHintType
       , parseHintType

       , Line (..)
       , toLines
       ) where

import Relude.Extra.Enum (inverseMap)


-- | Data type to represent the information for comment creation.
data Hint = Hint
    { hintHeader       :: !Text  -- ^ Hint text
    , hintBody         :: !Text  -- ^ New line body
    , hintIsSuggestion :: !Bool  -- ^ Is this GitHub @suggestion@?
    , hintNote         :: !Text  -- ^ Additional information
    , hintType         :: !HintType  -- ^ Type of the hint
    } deriving stock (Show, Eq)

-- | Formats 'Hint' in the compatible with @GitHub@ way.
formatHint :: Hint -> Text
formatHint Hint{..} = unlines $ concat
    [ [ hintHeader | hintHeader /= ""]
    , ["```" <> if hintIsSuggestion then "suggestion" else ""]
    , [ hintBody | hintType /= TrailingNewlines ]
    , [ "```"]
    , [ "__Note:__ " <> hintNote | hintNote /= "" ]
    ]


-- | Supported types of the hints.
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
