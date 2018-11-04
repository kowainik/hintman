module Hintman.Config
       ( HintmanConfig(..)
       , loadFileConfig
       ) where

import Control.Exception (catch)
import Relude.Extra.Enum (inverseMap)
import Toml              (AnyValue (..), BiMap (..), BiToml, (.=), LoadTomlException)

import qualified Toml


data SuggestionType
    = TrailingSpaces
    | TrailingNewlines
    deriving (Eq, Read, Show, Bounded, Enum)

newtype HintmanConfig = HintmanConfig
    { hintmanConfigSuggestionTypes :: [SuggestionType]
    } deriving (Show)

defaultHintmanConfig :: HintmanConfig
defaultHintmanConfig = HintmanConfig [TrailingSpaces, TrailingNewlines]

showSuggestionType :: SuggestionType -> Text
showSuggestionType  = \case
    TrailingSpaces   -> "trailing-spaces"
    TrailingNewlines -> "trailing-newlines"

parseSuggestionType :: Text -> Maybe SuggestionType
parseSuggestionType = inverseMap showSuggestionType

hintmanConfiguationT :: BiToml HintmanConfig
hintmanConfiguationT = HintmanConfig
    <$> Toml.arrayOf _SuggestionType "checks" .= hintmanConfigSuggestionTypes
  where
    _SuggestionType :: BiMap AnyValue SuggestionType
    _SuggestionType = BiMap
         { forward = \(AnyValue t) -> Toml.matchText t >>= parseSuggestionType
         , backward = Just . AnyValue . Toml.Text . showSuggestionType
         }

loadFileConfig :: FilePath -> IO HintmanConfig
loadFileConfig path = Toml.decodeFile hintmanConfiguationT path `catch` catchLoad
    where
        catchLoad :: LoadTomlException -> IO HintmanConfig
        catchLoad _ = do
              print $ "Hintman failed to load config from " <> path
              pure defaultHintmanConfig
