module Main (main) where

import System.IO (hSetEncoding, utf8)
import Test.Hspec (hspec)

import qualified Test.Config as Config
import qualified Test.Suggestion as Suggestion

main :: IO ()
main = do
  hspec Config.spec
  hspec Suggestion.spec
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  ifM Suggestion.tests exitSuccess exitFailure
