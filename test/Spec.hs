module Main (main) where

import System.IO (hSetEncoding, utf8)
import Test.Hspec (hspec)

import Hintman (mkTestAppEnv)
import Test.Config (configSpec)
import Test.HLint (hlintSpec)
import Test.Suggestion (suggestionSpec, suggestionTests)

main :: IO ()
main = mkTestAppEnv >>= \env -> do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hspec $ do
        configSpec
        suggestionSpec
        hlintSpec env
    ifM suggestionTests exitSuccess exitFailure
