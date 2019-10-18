-- TODO: rename to
-- Test.Hint
--   Test.Hint.HLint
--   Test.Hint.TrailingSpaces
module Test.Suggestion
       ( hintProperties
       ) where

import Hedgehog (Gen, Group (..), MonadGen, Property, PropertyName, forAll, property, (===))

import Hintman.Core.Hint (Line (..), Suggestion (..), toLines)

import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Hintman.Hint.TrailingSpaces as S


hintProperties :: Group
hintProperties = Group "Test.Hint" props
  where
    props :: [(PropertyName, Property)]
    props = [ ("suggest edits when trailing spaces", testSuggestTrailing) ]


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
