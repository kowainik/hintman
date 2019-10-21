{- | Tests for the HLint hints.
-}

module Test.Hint.HLint
       ( hlintSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (Hint (..), HintType (HLint))
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))

import Test.Data (Prs (..), runWithFilter)


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
    runHLint = runWithFilter HLint

    mkComment :: Text -> Int -> Hint -> Comment
    mkComment fileName pos hint = Comment
        { commentPath = fileName
        , commentPosition = pos
        , commentHint = hint
        }

    mkMainComment :: Int -> Hint -> Comment
    mkMainComment = mkComment "Main.hs"

    mkBigExampleComment :: Int -> Hint -> Comment
    mkBigExampleComment = mkComment "BigExample.hs"

    hintType :: HintType
    hintType = HLint

    hintNote :: Text
    hintNote = ""

    etaComment :: Comment
    etaComment = mkMainComment 10 Hint
        { hintHeader = "Warning: Eta reduce"
        , hintBody = "greet = (++) \"Hello \""
        , hintIsSuggestion = True
        , ..
        }

    avoidLambdaComment :: Comment
    avoidLambdaComment = mkMainComment 13 Hint
        { hintHeader = "Warning: Avoid lambda"
        , hintBody = "foo a b = (succ) a + b"
        , hintIsSuggestion = True
        , ..
        }

    removePragmaComment :: Comment
    removePragmaComment = mkMainComment 1 Hint
        { hintHeader = "Warning: Unused LANGUAGE pragma"
        , hintBody = ""
        , hintIsSuggestion = True
        , ..
        }

    multilineComment :: Comment
    multilineComment = mkMainComment 18 Hint
        { hintHeader = "Warning: Eta reduce"
        , hintBody = "multiline = putStrLn"
        , hintIsSuggestion = False
        , ..
        }

    redundantParenComment :: Comment
    redundantParenComment = mkMainComment 21 Hint
        { hintHeader = "Warning: Redundant bracket"
        , hintBody = "redundantParen x = succ $ x - 1"
        , hintIsSuggestion = True
        , ..
        }

    redundantDoComment :: Comment
    redundantDoComment = mkMainComment 24 Hint
        { hintHeader = "Warning: Redundant do"
        , hintBody = "redundantDo = putStrLn \"Hello\""
        , hintIsSuggestion = True
        , ..
        }

    redundantDollarComment :: Comment
    redundantDollarComment = mkMainComment 27 Hint
        { hintHeader = "Suggestion: Redundant $"
        , hintBody = "redundantDollar = putStrLn \"<- What is this dollar about?\""
        , hintIsSuggestion = True
        , ..
        }

    fmapComment :: Comment
    fmapComment = mkMainComment 30 Hint
        { hintHeader = "Suggestion: Use <$>"
        , hintBody = "fmapWarn f = f Control.Applicative.<$> foo bar"
        , hintIsSuggestion = True
        , ..
        }

    removeParensComment :: [Comment]
    removeParensComment = one $ mkBigExampleComment 5 Hint
        { hintHeader = "Suggestion: Redundant bracket"
        , hintBody = "        pr1 >>= runLog . runHLint >>= shouldBe []"
        , hintIsSuggestion = True
        , ..
        }
