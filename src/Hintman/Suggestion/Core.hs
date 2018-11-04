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

data Suggestion = Suggestion
    { suggestionFile   :: FilePath
    , suggestionLine   :: Line
    , suggestionChange :: LineChange
    }

data LineChange = LineChange
    { lineChangeNew     :: Text
    , lineChangeComment :: Maybe Text
    , lineChangeType    :: ChangeType
    }

toLines :: Text -> [Line]
toLines txt = map (\(lineRow, lineBody) -> Line{..}) numberedLines
  where
    numberedLines :: [(Int, Text)]
    numberedLines = zip [1..] (lines txt)

