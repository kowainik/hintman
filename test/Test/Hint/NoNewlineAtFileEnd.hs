module Test.Hint.NoNewlineAtFileEnd
       ( noNewlineAtFileEndSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Hintman.Core.Hint (HintType (NoNewlineAtFileEnd), simpleSuggestion)
import Hintman.Core.PrInfo (PrInfo)
import Hintman.Core.Review (Comment (..))

import Test.Data (Prs (..), runWithFilter)


noNewlineAtFileEndSpec :: Prs -> Spec
noNewlineAtFileEndSpec Prs{..} = describe "Comments are spawned for files without newline at file end" $ do
    it "Spawns comment for README.md" $
        run pr41 >>= (`shouldSatisfy` (readmeComment `elem`))
    it "Spawns comment for BigExample.hs" $
        run pr41 >>= (`shouldSatisfy` (bigExampleHsComment `elem`))
    it "Does not spawn comments for an empty file" $
        run pr41 >>= (`shouldSatisfy` noWarningForEmptyFile)
    it "doesn't produce comment on other PRs" $
        run pr4 >>= shouldBe []
 where
    run :: PrInfo -> IO [Comment]
    run = runWithFilter NoNewlineAtFileEnd

    mkComment :: Text -> Int -> Text -> Comment
    mkComment file pos suggestion = Comment
        { commentPath = file
        , commentPosition = pos
        , commentHint = simpleSuggestion NoNewlineAtFileEnd "No newline at end of file" suggestion
        }

    readmeComment :: Comment
    readmeComment = mkComment "README.md" 3 "Target dummy for hintman\n"
    {- -----------------------------------^
    Note that line number corresponds to the line in diff, not to the line in file.
    See 'Hintman.Hint.Position.getTargetCommentPosition' for details. -}

    bigExampleHsComment :: Comment
    bigExampleHsComment = mkComment "BigExample.hs" 5 "        ]\n"

    noWarningForEmptyFile :: [Comment] -> Bool
    noWarningForEmptyFile = not . any (\c -> commentPath c == "EmptyFile.hs")
