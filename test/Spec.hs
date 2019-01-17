module Main (main) where

import System.IO (hSetEncoding, utf8)
import Test.Hspec (hspec)

import Test.Suggestion (spec, tests)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hspec spec
  ifM tests exitSuccess exitFailure
