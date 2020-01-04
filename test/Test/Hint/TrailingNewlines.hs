module Test.Hint.TrailingNewlines
       ( trailingNewlinesSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (HintType (TrailingNewlines), simpleSuggestion)
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))

import Test.Data (Prs (..), runWithFilter)


trailingNewlinesSpec :: Prs -> Spec
trailingNewlinesSpec Prs{..} = describe "Trailing Newlines are removed on opened PRs" $ do
    it "removes last trailing newline with spaces" $
        run pr3 >>= (`shouldSatisfy` (lastLineComment `elem`))
    it "removes pred-last trailing newline" $
        run pr3 >>= (`shouldSatisfy` (predLastLineComment `elem`))
    it "doesn't produce comment on other PRs" $
        run pr4 >>= shouldBe []
    it "removes trailing newline not from the first hunk in PR 30" $
        run pr30 >>= (`shouldSatisfy` (secondHunkComment `elem`))
 where
    run :: PrInfo -> IO [Comment]
    run = runWithFilter TrailingNewlines

    mkComment :: Text -> Int -> Comment
    mkComment file pos = Comment
        { commentPath = file
        , commentPosition = pos
        , commentHint = simpleSuggestion TrailingNewlines "Trailing newline" ""
        }

    lastLineComment :: Comment
    lastLineComment = mkComment "Main.hs" 32

    predLastLineComment :: Comment
    predLastLineComment = mkComment "Main.hs" 31

    secondHunkComment :: Comment
    secondHunkComment = mkComment "BigExample.hs" 13
