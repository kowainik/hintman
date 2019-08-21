module Test.HLint
       ( hlintSpec
       ) where

import Colog (LoggerT, usingLoggerT)
import Test.Hspec (Spec, describe, it, shouldBe)

import Hintman.HLint (runHLint)

import Test.Data (pr1, pr2)

runLog :: LoggerT Message IO a -> IO a
runLog = usingLoggerT mempty

hlintSpec :: Spec
hlintSpec = describe "HLint works on opened PRs" $ do
    it "works with non-code PRs" $
        pr1 >>= runLog . runHLint >>= shouldBe []
    it "works on code PRs" $
        pr2 >>= runLog . runHLint >>= shouldBe [etaReduce] . map show
  where
    etaReduce :: Text
    etaReduce = unlines
        [ "Main.hs:8:1: Warning: Eta reduce"
        , "Found:"
        , "  greet x = (++) \"Hello \" x"
        , "Perhaps:"
        , "  greet = (++) \"Hello \""
        ]
