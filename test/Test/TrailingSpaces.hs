-- TODO: rename to
-- Test.Hint
--   Test.Hint.HLint
--   Test.Hint.TrailingSpaces

module Test.TrailingSpaces
       ( trailingSpacesSpec
       ) where

import Colog (LoggerT, usingLoggerT)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (HintType (TrailingSpaces))
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))
import Hintman.Hint (getAllComments)

import Test.Data (pr1, pr2, pr3)



runLog :: LoggerT Message IO a -> IO a
runLog = usingLoggerT mempty

trailingSpacesSpec :: Spec
trailingSpacesSpec = describe "Trailing Spaces are removed on opened PRs" $ do
    it "outlines trailing spaces in non-Haskell files" $
        pr1 >>= runLog . run >>= (`shouldSatisfy` (readmeComment `elem`))
    it "outlines trailing spaces in Haskell files" $
        pr2 >>= runLog . run >>= (`shouldSatisfy` (mainComment `elem`))
    it "doesn't produce comment on other PRs" $
        pr3 >>= runLog . run >>= shouldBe []
 where
    run :: (MonadIO m, WithLog env m) => PrInfo -> m [Comment]
    run prInfo = filter ((==) TrailingSpaces . commentHintType) <$> getAllComments prInfo

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
