module Test.Config
       ( configSpec
       ) where

import System.IO.Silently (capture)
import System.IO.Temp (writeSystemTempFile)
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldReturn)

import Hintman.Config (HintmanConfig (..), SuggestionType (..), loadFileConfig)


configSpec :: Spec
configSpec = describe "loadFileConfig" $ do
    let defaultHintmanConfig = HintmanConfig [TrailingSpaces, TrailingNewlines]
    it "returns a HintmanConfig from a config file" $ do
        configFile <- writeSystemTempFile "hintman" "checks=[\"trailing-spaces\"]"
        loadFileConfig configFile `shouldReturn` HintmanConfig [TrailingSpaces]
    context "When config file is malformed" $
        it "returns defaultHintmanConfig and informs user" $ do
            configFile <- writeSystemTempFile "hintman" "checks=["
            (msg, result) <- capture (loadFileConfig configFile)
            result `shouldBe` defaultHintmanConfig
            msg `shouldBe` ("Encountered LoadTomlException while trying to load config from " <> configFile <> "\n")
    context "When config file is missing" $
        it "returns defaultHintmanConfig and informs user" $ do
            (msg, result) <- capture (loadFileConfig "")
            result `shouldBe` defaultHintmanConfig
            msg `shouldBe` "Config '' does not exist! Using default configuration\n"
