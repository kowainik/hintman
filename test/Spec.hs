module Main (main) where

import Hedgehog (checkParallel)
import System.IO (hSetEncoding, utf8)
import Test.Hspec (hspec)

import Test.HLint (hlintSpec)
import Test.Suggestion (hintProperties)


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec hlintSpec
    ifM (checkParallel hintProperties) exitSuccess exitFailure
