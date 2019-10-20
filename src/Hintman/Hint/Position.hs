module Hintman.Hint.Position
       ( getTargetCommentPosition
       , (!!?)
       ) where

import Text.Diff.Parse.Types (Annotation (..), Content (..), FileDelta (..), Hunk (..), Line (..),
                              Range (..))


{- | Line numbers in the file (modified by PR) and in the 'FileDelta' are
are different as they have different meaning.

HLint and other hint-generators is working with the whole file and gives us the
information about corresponding line in the file, while @git-diff@ only could
show the modified chunks of lines. To leave a comment we can only apply them to
the modified lines, so we need to know there number in the @git-diff@.

This function is making this translation for you. Given a 'FileDelta' and the
file's line number it returns the corresponding number of
'FileDelta' which matches the last line of the HLint comment.

Note that we only will leave comments on 'Added' lines, not 'Removed' or 'Context'.

If we have this modification in the file:

@
myFileDelta = FileDelta
    { fileDeltaStatus = Modified
    , fileDeltaSourceFile = "file.example"
    , fileDeltaDestFile = "file.example"
    , fileDeltaContent = Hunks
        [ Hunk
            { hunkSourceRange = Range
                { rangeStartingLineNumber = 3
                , rangeNumberOfLines = 2
                }
            , hunkDestRange = Range
                { rangeStartingLineNumber = 3
                , rangeNumberOfLines = 2
                }
            , hunkLines =
                [ Line Added "Line number 3"
                , Line Context "Line number 4"
                ]
            ...
@

then it could only add HLint comment to lines 3 and 4 but not any other.

>>> getTargetCommentPosition myFileDelta 3
Just 0
>>> getTargetCommentPosition myFileDelta 4
Nothing
>>> getTargetCommentPosition myFileDelta 5
Nothing
-}
getTargetCommentPosition :: FileDelta -> Int -> Maybe Int
getTargetCommentPosition FileDelta{..} commentPos = case fileDeltaContent of
    Binary   -> Nothing
    Hunks hs -> goHunks hs
  where
    -- Find the position number i the hunk.
    -- Stops on the first satisfying.
    goHunks :: [Hunk] -> Maybe Int
    goHunks [] = Nothing
    goHunks (Hunk{..}:hs) = case inRange hunkDestRange of
        Nothing -> goHunks hs
        Just diffLine -> do
            (pos, line) <- diffLinePos diffLine hunkLines
            -- Only 'Added' lines.
            guard $ lineAnnotation line == Added
            Just pos
      where
        -- | Check is the desired line number belongs to this Hunk
        -- and return the corresponding number in the Hunk.
        inRange :: Range -> Maybe Int
        inRange Range{..} =
            if (rangeStartingLineNumber <= commentPos)
                && (commentPos <= (rangeStartingLineNumber + rangeNumberOfLines))
            then Just $ commentPos - rangeStartingLineNumber
            else Nothing

{- | Returns the relative position of diff line in the given list, ignoring all
'Removed' lines. For example, if you have the following diff (written with indexes):

@
0 -> (1, Context "foo")
1 -> (2, Context "bar")
2 -> (3, Context "xxx")
3 -> (4, Removed "yyy")
4 -> (5, Added   "zzz")
@

Then the resulting file has only 4 lines, but the diff has 5 elements. So, when
given number 3 to the argument of 'diffLinePos', it should return

@
(5, Added "    zzz")
@
-}
diffLinePos :: Int -> [Line] -> Maybe (Int, Line)
diffLinePos index =
      (!!? index)
    . filter ((/=) Removed . lineAnnotation . snd)
    . zip [1..]

-- | Safe @at@ function.
(!!?) :: [a] -> Int -> Maybe a
l !!? index
    | index < 0 = Nothing
    | otherwise = f index l
  where
    f :: Int -> [a] -> Maybe a
    f 0 (x:_)  = Just x
    f i (_:xs) = f (i - 1) xs
    f _ []     = Nothing
