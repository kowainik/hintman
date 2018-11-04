module Hintman.Suggestion.Core
       ( Line (..)
       , Suggest (..)
       , toLines
       ) where


data Line = Line
    { lineRow  :: Int
    , lineBody :: Text
    } deriving (Eq, Show)

data Suggest = Suggest
    { suggestFile    :: FilePath
    , suggestNew     :: Text
    , suggestComment :: Maybe Text  -- ^ Optional comment to the suggestion
    } deriving (Eq, Show)

toLines :: Text -> [Line]
toLines txt = map (\(lineRow, lineBody) -> Line{..}) numberedLines
  where
    numberedLines :: [(Int, Text)]
    numberedLines = zip [1..] (lines txt)

