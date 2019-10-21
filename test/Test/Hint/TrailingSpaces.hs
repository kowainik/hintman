{- | Tests for trailing spaces hints
-}

module Test.Hint.TrailingSpaces
       ( trailingSpacesSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (HintType (TrailingSpaces))
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))
import Hintman.Hint (getAllComments)

import Test.Data (Prs (..), runLog)


trailingSpacesSpec :: Prs -> Spec
trailingSpacesSpec Prs{..} = describe "Trailing Spaces are removed on opened PRs" $ do
    it "outlines trailing spaces in non-Haskell files" $
        run pr1 >>= (`shouldSatisfy` (readmeComment `elem`))
    it "outlines trailing spaces in Haskell files" $
        run pr2 >>= (`shouldSatisfy` (mainComment `elem`))
    it "doesn't produce comment on other PRs" $
        run pr3 >>= shouldBe []
 where
    run :: PrInfo -> IO [Comment]
    run prInfo = filter ((==) TrailingSpaces . commentHintType)
        <$> runLog (getAllComments prInfo)

    mkComment :: Text -> Int -> Text -> Comment
    mkComment file pos txt = Comment
        { commentPath = file
        , commentPosition = pos
        , commentBody = unlines
            [ "```suggestion"
            , txt
            , "```"
            ]
        , commentHintType = TrailingSpaces
        }

    readmeComment :: Comment
    readmeComment = mkComment "README.md" 3 ":)"

    mainComment :: Comment
    mainComment = mkComment "Main.hs" 2 ""
