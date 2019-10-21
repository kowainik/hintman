{- | Tests for the HLint hints.
-}

module Test.Hint.HLint
       ( hlintSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (HintType (HLint))
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))
import Hintman.Hint (getAllComments)

import Test.Data (Prs (..), runLog)


hlintSpec :: Prs -> Spec
hlintSpec Prs{..} = describe "HLint works on opened PRs" $ do
    it "works with non-code PRs for PR 1" $
        runHLint pr1 >>= shouldBe []
    it "ignores parse errors for PR 3" $
        runHLint pr3 >>= shouldBe []
    it "creates correct eta-reduce comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (etaComment `elem`))
    it "creates correct part line comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (avoidLambdaComment `elem`))
    it "creates remove line comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (removePragmaComment `elem`))
    it "creates multiline comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (multilineComment `elem`))
    it "creates redundant parens comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (redundantParenComment `elem`))
    it "creates redundant do comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (redundantDoComment `elem`))
    it "creates redundant $ comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (redundantDollarComment `elem`))
    it "creates <$> over fmap comment for PR 2" $
        runHLint pr2 >>= (`shouldSatisfy` (fmapComment `elem`))
    it "redundant () in one line diff for PR 24" $
        runHLint pr24 >>= (`shouldBe` removeParensComment)
  where
    runHLint :: PrInfo -> IO [Comment]
    runHLint prInfo = filter ((==) HLint . commentHintType)
        <$> runLog (getAllComments prInfo)

    mkComment :: Text -> Int -> Text -> Comment
    mkComment fileName pos txt = Comment
        { commentPath = fileName
        , commentPosition = pos
        , commentBody = txt
        , commentHintType = HLint
        }

    mkMainComment :: Int -> Text -> Comment
    mkMainComment = mkComment "Main.hs"

    mkBigExampleComment :: Int -> Text -> Comment
    mkBigExampleComment = mkComment "BigExample.hs"

    etaComment :: Comment
    etaComment = mkMainComment 10 $ unlines
        [ "Warning: Eta reduce"
        , "```suggestion"
        , "greet = (++) \"Hello \""
        , "```"
        ]

    avoidLambdaComment :: Comment
    avoidLambdaComment = mkMainComment 13 $ unlines
        [ "Warning: Avoid lambda"
        , "```suggestion"
        , "foo a b = (succ) a + b"
        , "```"
        ]

    removePragmaComment :: Comment
    removePragmaComment = mkMainComment 1 $ unlines
        [ "Warning: Unused LANGUAGE pragma"
        , "```suggestion"
        , ""
        , "```"
        ]

    multilineComment :: Comment
    multilineComment = mkMainComment 18 $ unlines
        [ "Warning: Eta reduce"
        , "```"
        , "multiline = putStrLn"
        , "```"
        ]

    redundantParenComment :: Comment
    redundantParenComment = mkMainComment 21 $ unlines
        [ "Warning: Redundant bracket"
        , "```suggestion"
        , "redundantParen x = succ $ x - 1"
        , "```"
        ]

    redundantDoComment :: Comment
    redundantDoComment = mkMainComment 24 $ unlines
        [ "Warning: Redundant do"
        , "```suggestion"
        , "redundantDo = putStrLn \"Hello\""
        , "```"
        ]

    redundantDollarComment :: Comment
    redundantDollarComment = mkMainComment 27 $ unlines
        [ "Suggestion: Redundant $"
        , "```suggestion"
        , "redundantDollar = putStrLn \"<- What is this dollar about?\""
        , "```"
        ]

    fmapComment :: Comment
    fmapComment = mkMainComment 30 $ unlines
        [ "Suggestion: Use <$>"
        , "```suggestion"
        , "fmapWarn f = f Control.Applicative.<$> foo bar"
        , "```"
        ]

    removeParensComment :: [Comment]
    removeParensComment = one $ mkBigExampleComment 5 $ unlines
        [ "Suggestion: Redundant bracket"
        , "```suggestion"
        , "        pr1 >>= runLog . runHLint >>= shouldBe []"
        , "```"
        ]

