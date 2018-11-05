module Main (main) where

import System.IO (hSetEncoding, utf8)

import Test.Suggestion (tests)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  ifM tests exitSuccess exitFailure
