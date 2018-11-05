module Hintman.Suggestion.Core
       ( ChangeType (..)
       , Line (..)
       , LineChange (..)
       , Suggestion (..)
       , toLines
       ) where


data Line = Line
    { lineRow  :: Int
    , lineBody :: Text
    } deriving (Eq, Show)

data ChangeType = Edit | Delete
  deriving (Eq, Show)

data Suggestion = Suggestion
    { suggestionFile   :: FilePath
    , suggestionLine   :: Line
    , suggestionChange :: LineChange
    } deriving (Eq, Show)

data LineChange = LineChange
    { lineChangeNew     :: Text
    , lineChangeComment :: Maybe Text
    , lineChangeType    :: ChangeType
    } deriving (Eq, Show)

toLines :: Text -> [Line]
toLines txt = map (\(lineRow, lineBody) -> Line{..}) numberedLines
  where
    numberedLines :: [(Int, Text)]
    numberedLines = zip [1..] (lines txt)

