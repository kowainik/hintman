-- | Configuration for @hintman@ with TOML codec.

module Hintman.Config
       ( HintmanConfig(..)
       , SuggestionType(..)
       , loadFileConfig
       ) where

import Control.Exception (IOException, catch)
import Relude.Extra.Enum (inverseMap)
import Toml (AnyValue (..), LoadTomlException, TomlBiMap, TomlCodec, (.=))

import qualified Toml


data SuggestionType
    = TrailingSpaces
    | TrailingNewlines
    deriving (Eq, Read, Show, Bounded, Enum)

newtype HintmanConfig = HintmanConfig
    { hintmanConfigSuggestionTypes :: [SuggestionType]
    } deriving (Eq, Show)

defaultHintmanConfig :: HintmanConfig
defaultHintmanConfig = HintmanConfig [TrailingSpaces, TrailingNewlines]

showSuggestionType :: SuggestionType -> Text
showSuggestionType  = \case
    TrailingSpaces   -> "trailing-spaces"
    TrailingNewlines -> "trailing-newlines"

parseSuggestionType :: Text -> Maybe SuggestionType
parseSuggestionType = inverseMap showSuggestionType

hintmanConfiguationCodec :: TomlCodec HintmanConfig
hintmanConfiguationCodec = HintmanConfig
    <$> Toml.arrayOf _SuggestionType "checks" .= hintmanConfigSuggestionTypes
  where
    _SuggestionType :: TomlBiMap SuggestionType AnyValue
    _SuggestionType = Toml._TextBy
        showSuggestionType
        (maybeToRight "Couldn't parsse 'SuggestionType'" . parseSuggestionType)

loadFileConfig :: FilePath -> IO HintmanConfig
loadFileConfig path =
    Toml.decodeFile hintmanConfiguationCodec path
        `catch` catchExcAndReturnDefaultConfig
  where
    catchExcAndReturnDefaultConfig :: SomeException -> IO HintmanConfig
    catchExcAndReturnDefaultConfig e = defaultHintmanConfig <$ case e of
        Exc (_ :: LoadTomlException) -> putTextLn ("Encountered LoadTomlException while trying to load config from " <> toText path)
        Exc (_ :: IOException) -> putTextLn ("Config '" <> toText path <> "' does not exist! Using default configuration")
        _ -> putTextLn ("Error while trying to load config from " <> toText path <> ": " <> show e)
