module Test.Hint.TrailingNewlines
       ( trailingNewlinesSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (HintType (TrailingNewlines))
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))
import Hintman.Hint (getAllComments)

import Test.Data (pr2, pr3, runLog)


trailingNewlinesSpec :: Spec
trailingNewlinesSpec = describe "Trailing Newlines are removed on opened PRs" $ do
    it "removes last trailing newline with spaces" $
        pr2 >>= runLog . run >>= (`shouldSatisfy` (lastLineComment `elem`))
    it "removes pred-last trailing newline" $
        pr2 >>= runLog . run >>= (`shouldSatisfy` (predLastLineComment `elem`))
    it "doesn't produce comment on other PRs" $
        pr3 >>= runLog . run >>= shouldBe []
 where
    run :: (MonadIO m, WithLog env m) => PrInfo -> m [Comment]
    run prInfo = filter ((==) TrailingNewlines . commentHintType) <$> getAllComments prInfo

    mkComment :: Text -> Int -> Comment
    mkComment file pos = Comment
        { commentPath = file
        , commentPosition = pos
        , commentBody = unlines
            [ "```suggestion"
            , "```"
            ]
        , commentHintType = TrailingNewlines
        }

    lastLineComment :: Comment
    lastLineComment = mkComment "Main.hs" 32

    predLastLineComment :: Comment
    predLastLineComment = mkComment "Main.hs" 31
