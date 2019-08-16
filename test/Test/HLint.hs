module Test.HLint
       ( hlintSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hintman.HLint (runHLint)

import Test.Data (pr1, pr2)


hlintSpec :: Spec
hlintSpec = describe "HLint works on opened PRs" $ do
    it "works with non-code PRs" $
        pr1 >>= runHLint >>= shouldBe []
    it "works on code PRs" $
        pr2 >>= runHLint >>= shouldBe [etaReduce] . map show
  where
    etaReduce :: Text
    etaReduce = unlines
        [ "Main.hs:8:1: Warning: Eta reduce"
        , "Found:"
        , "  greet x = (++) \"Hello \" x"
        , "Perhaps:"
        , "  greet = (++) \"Hello \""
        ]
