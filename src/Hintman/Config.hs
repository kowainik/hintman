-- | Configuration for @hintman@ with TOML codec.

module Hintman.Config
       ( HintmanConfig(..)
       , loadFileConfig
       ) where

import Control.Exception (IOException, catch)
import Toml (AnyValue (..), LoadTomlException, TomlBiMap, TomlCodec, (.=))

import Hintman.Core.Hint (HintType (..), parseHintType, showHintType)

import qualified Toml


newtype HintmanConfig = HintmanConfig
    { hintmanConfigHintTypes :: [HintType]
    } deriving (Eq, Show)

defaultHintmanConfig :: HintmanConfig
defaultHintmanConfig = HintmanConfig [TrailingSpaces, TrailingNewlines]

hintmanConfiguationCodec :: TomlCodec HintmanConfig
hintmanConfiguationCodec = HintmanConfig
    <$> Toml.arrayOf _HintType "checks" .= hintmanConfigHintTypes
  where
    _HintType :: TomlBiMap HintType AnyValue
    _HintType = Toml._TextBy
        showHintType
        (maybeToRight "Couldn't parse 'HintType'" . parseHintType)

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
