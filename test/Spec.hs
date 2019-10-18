module Main (main) where

import System.IO (hSetEncoding, utf8)
import Test.Hspec (hspec)

import Test.HLint (hlintSpec)
import Test.TrailingSpaces (trailingSpacesSpec)


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec $ do
        hlintSpec
        trailingSpacesSpec
