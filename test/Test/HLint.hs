module Test.HLint
       ( hlintSpec
       ) where

import Colog (LoggerT, usingLoggerT)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Review (Comment (..))
import Hintman.HLint (runHLint)

import Test.Data (pr1, pr2)

runLog :: LoggerT Message IO a -> IO a
runLog = usingLoggerT mempty

hlintSpec :: Spec
hlintSpec = describe "HLint works on opened PRs" $ do
    it "works with non-code PRs" $
        pr1 >>= runLog . runHLint >>= shouldBe []
    it "creates correct eta-reduce comment for PR 2" $
        pr2 >>= runLog . runHLint >>= (`shouldSatisfy` (etaComment `elem`))
    it "creates correct part line comment for PR 2" $
        pr2 >>= runLog . runHLint >>= (`shouldSatisfy` (avoidLambdaComment `elem`))
    it "creates remove line comment for PR 2" $
        pr2 >>= runLog . runHLint >>= (`shouldSatisfy` (removePragmaComment `elem`))
    it "creates multiline comment for PR 2" $
        pr2 >>= runLog . runHLint >>= (`shouldSatisfy` (multilineComment `elem`))

  where
    mkComment :: Int -> Text -> Comment
    mkComment pos txt = Comment
        { commentPath = "Main.hs"
        , commentPosition = pos
        , commentBody = txt
        }

    etaComment :: Comment
    etaComment = mkComment 9 $ unlines
        [ "Warning: Eta reduce"
        , "```suggestion"
        , "greet = (++) \"Hello \""
        , "```"
        ]

    avoidLambdaComment :: Comment
    avoidLambdaComment = mkComment 12 $ unlines
        [ "Warning: Avoid lambda"
        , "```suggestion"
        , "foo a b = (succ) a + b"
        , "```"
        ]

    removePragmaComment :: Comment
    removePragmaComment = mkComment 0 $ unlines
        [ "Warning: Unused LANGUAGE pragma"
        , "```suggestion"
        , ""
        , "```"
        ]

    multilineComment :: Comment
    multilineComment = mkComment 17 $ unlines
        [ "Warning: Eta reduce"
        , "```"
        , "multiline = putStrLn"
        , "```"
        ]
