{-# LANGUAGE OverloadedStrings #-}

module Test.Suggestion (spec, tests) where

import qualified Data.Text as T
import Hedgehog (Gen, Group (..), MonadGen, Property, PropertyName, checkParallel, forAll, property,
                 (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Hintman.Config (SuggestionType (..))
import Hintman.Suggestion.Core (Line (..), LineChange (..), Suggestion (..), toLines)
import qualified Hintman.Suggestion.TrailingNewline as N
import qualified Hintman.Suggestion.TrailingSpaces as S

newtype LineNumber = MkLineNumber Int
  deriving (Eq, Show)

data TestLine
  = Plain Text
  | Trailing Text
  deriving (Show)

space :: MonadGen m => m Char
space = pure ' '

genSpaces :: Gen Text
genSpaces = Gen.text (Range.linear 1 20) space

genText :: Gen Text
genText = unwords <$> Gen.list (Range.linear 0 20) (Gen.text (Range.linear 5 100) Gen.alpha)

genPlain :: Gen TestLine
genPlain = Plain <$> genText

genTrailing :: Gen TestLine
genTrailing = Trailing <$> Gen.subterm2 genText genSpaces T.append

genLines :: Gen [TestLine]
genLines = Gen.list (Range.linear 0 120) $ Gen.choice
  [ genPlain
  , genTrailing
  ]

linesToText :: [TestLine] -> Text
linesToText = unlines . map unLine
  where
    unLine :: TestLine -> Text
    unLine (Plain s)    = s
    unLine (Trailing s) = s

trailingLines :: [TestLine] -> [LineNumber]
trailingLines xs = map (MkLineNumber . fst) (filter isTrailing numberedLines)
  where
    numberedLines :: [(Int, TestLine)]
    numberedLines = zip [1..] xs

    isTrailing :: (Int, TestLine) -> Bool
    isTrailing (_, Trailing _) = True
    isTrailing _               = False

testSuggestTrailing :: Property
testSuggestTrailing = property $ do
  xs <- forAll genLines
  let ys = map (lineRow . suggestionLine) (S.suggest "foo" (toLines . linesToText $ xs))
      ns = map coerce (trailingLines xs)
  ys === ns

tests :: IO Bool
tests = checkParallel (Group "Test.Suggest" props)
  where
    props :: [(PropertyName, Property)]
    props =
      [ ("suggest edits when trailing spaces", testSuggestTrailing) ]

spec :: Spec
spec =
  describe "Trailing newlines removal" $ do
    context "When file has trailing newlines" $
      it "suggests removal" $ do
        let given = toLines "foo bar\n\nbaz\n\n"
            expected =
                [ Suggestion "foo" (Line 4 T.empty) (LineChange T.empty Nothing TrailingNewlines) ]
        N.suggest "foo" given `shouldBe` expected
    context "When file has no trailing newlines" $
      it "suggests nothing" $ do
        let given = toLines "foo bar\nbaz\n"
            expected = []
        N.suggest "foo" given `shouldBe` expected
