module Main (main) where

import System.IO (hSetEncoding, utf8)
import Test.Hspec (hspec)

import Test.Hint (hintSpec)


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec hintSpec
