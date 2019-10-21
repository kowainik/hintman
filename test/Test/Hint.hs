module Test.Hint
       ( hintSpec
       ) where

import Test.Hspec (Spec, describe)

import Test.Data (Prs)
import Test.Hint.HLint (hlintSpec)
import Test.Hint.TrailingNewlines (trailingNewlinesSpec)
import Test.Hint.TrailingSpaces (trailingSpacesSpec)


hintSpec :: Prs -> Spec
hintSpec prs = describe "Hintman hints" $ do
    hlintSpec prs
    trailingNewlinesSpec prs
    trailingSpacesSpec prs
