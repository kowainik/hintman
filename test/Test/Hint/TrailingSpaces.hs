{- | Tests for trailing spaces hints
-}

module Test.Hint.TrailingSpaces
       ( trailingSpacesSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (HintType (TrailingSpaces), simpleSuggestion)
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))

import Test.Data (Prs (..), runWithFilter)


trailingSpacesSpec :: Prs -> Spec
trailingSpacesSpec Prs{..} = describe "Trailing Spaces are removed on opened PRs" $ do
    it "outlines trailing spaces in non-Haskell files" $
        run pr2 >>= (`shouldSatisfy` (readmeComment `elem`))
    it "outlines trailing spaces in Haskell files" $
        run pr3 >>= (`shouldSatisfy` (mainComment `elem`))
    it "doesn't produce comment on other PRs" $
        run pr4 >>= shouldBe []
 where
    run :: PrInfo -> IO [Comment]
    run = runWithFilter TrailingSpaces

    mkComment :: Text -> Int -> Text -> Comment
    mkComment file pos txt = Comment
        { commentPath = file
        , commentPosition = pos
        , commentHint = simpleSuggestion TrailingSpaces "Trailing spaces" txt
        }

    readmeComment :: Comment
    readmeComment = mkComment "README.md" 3 ":)"

    mainComment :: Comment
    mainComment = mkComment "Main.hs" 2 ""
