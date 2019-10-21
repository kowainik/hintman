module Test.Hint
       ( hintSpec
       ) where

import Test.Hspec (Spec, describe)

import Test.Hint.HLint (hlintSpec)
import Test.Hint.TrailingNewlines (trailingNewlinesSpec)
import Test.Hint.TrailingSpaces (trailingSpacesSpec)


hintSpec :: Spec
hintSpec = describe "Hintmant hints" $ do
    hlintSpec
    trailingNewlinesSpec
    trailingSpacesSpec
