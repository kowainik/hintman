module Test.Hint
       ( hintSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hintman.Core.Hint (Hint (..), HintType (..), formatHint)

import Test.Data (Prs)
import Test.Hint.HLint (hlintSpec)
import Test.Hint.NoNewlineAtFileEnd (noNewlineAtFileEndSpec)
import Test.Hint.TrailingNewlines (trailingNewlinesSpec)
import Test.Hint.TrailingSpaces (trailingSpacesSpec)


hintSpec :: Prs -> Spec
hintSpec prs = describe "Hintman hints" $ do
    hintFormatSpec
    hlintSpec prs
    trailingNewlinesSpec prs
    trailingSpacesSpec prs
    noNewlineAtFileEndSpec prs


hintFormatSpec :: Spec
hintFormatSpec = describe "Hint formatted correctly" $ do
    it "formats suggestion correctly" $
        formatHint h1 `shouldBe` h1Txt
    it "formats non-suggestion correctly" $
        formatHint h2 `shouldBe` h2Txt
    it "adds note" $
        formatHint h3 `shouldBe` h3Txt
    it "handles TrailingNewlines correctly" $
        formatHint h4 `shouldBe` h4Txt
  where
    h1, h2, h3 :: Hint
    h1 = Hint
        { hintHeader = "Warning: Eta reduce"
        , hintBody = "foo = bar"
        , hintIsSuggestion = True
        , hintNote = ""
        , hintType = HLint
        }

    h2 = Hint
        { hintHeader = "Warning: Eta reduce"
        , hintBody = "foo = bar"
        , hintIsSuggestion = False
        , hintNote = ""
        , hintType = HLint
        }

    h3 = Hint
        { hintHeader = "Warning: Eta reduce"
        , hintBody = "foo = bar"
        , hintIsSuggestion = True
        , hintNote = "This is a note"
        , hintType = HLint
        }

    h4 = Hint
        { hintHeader = "Trailing newline"
        , hintBody = ""
        , hintIsSuggestion = True
        , hintNote = ""
        , hintType = TrailingNewlines
        }

    h1Txt, h2Txt, h3Txt :: Text
    h1Txt = unlines
        [ "Warning: Eta reduce"
        , "```suggestion"
        , "foo = bar"
        , "```"
        ]

    h2Txt = unlines
        [ "Warning: Eta reduce"
        , "```"
        , "foo = bar"
        , "```"
        ]

    h3Txt = unlines
        [ "Warning: Eta reduce"
        , "```suggestion"
        , "foo = bar"
        , "```"
        , "__Note:__ This is a note"
        ]

    h4Txt = unlines
        [ "Trailing newline"
        , "```suggestion"
        , "```"
        ]
