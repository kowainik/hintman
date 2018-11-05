{-# LANGUAGE OverloadedStrings #-}

module Test.Suggestion (tests) where

import qualified Data.Text as T
import Hedgehog (Gen, Group (..), MonadGen, Property, PropertyName, (===), checkParallel, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hintman.Suggestion.Core (lineRow, suggestionLine, toLines)
import Hintman.Suggestion.TrailingSpaces (suggest)

newtype LineNumber = MkLineNumber Int
  deriving (Eq, Show)

data Line
  = Plain Text
  | Trailing Text
  deriving (Show)

space :: MonadGen m => m Char
space = pure ' '

genSpaces :: Gen Text
genSpaces = Gen.text (Range.linear 1 20) space

genText :: Gen Text
genText = unwords <$> Gen.list (Range.linear 0 20) (Gen.text (Range.linear 5 100) Gen.alpha)

genPlain :: Gen Line
genPlain = Plain <$> genText

genTrailing :: Gen Line
genTrailing = Trailing <$> Gen.subterm2 genText genSpaces T.append

genLines :: Gen [Line]
genLines = Gen.list (Range.linear 0 120) $ Gen.choice
  [ genPlain
  , genTrailing
  ]

linesToText :: [Line] -> Text
linesToText = unlines . map unLine
  where
    unLine :: Line -> Text
    unLine (Plain s) = s
    unLine (Trailing s) = s

trailingLines :: [Line] -> [LineNumber]
trailingLines xs = map (MkLineNumber . fst) (filter isTrailing numberedLines)
  where
    numberedLines :: [(Int, Line)]
    numberedLines = zip [1..] xs

    isTrailing :: (Int, Line) -> Bool
    isTrailing (_, Trailing _) = True
    isTrailing _ = False

testSuggestTrailing :: Property
testSuggestTrailing = property $ do
  xs <- forAll genLines
  let ys = map (lineRow . suggestionLine) (suggest "foo" (toLines . linesToText $ xs))
      ns = map coerce (trailingLines xs)
  ys === ns

tests :: IO Bool
tests = checkParallel (Group "Test.Suggest" props)
  where
    props :: [(PropertyName, Property)]
    props =
      [ ("suggest edits when trailing spaces", testSuggestTrailing) ]
