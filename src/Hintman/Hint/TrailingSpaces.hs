{-| Contains function to catch trailing spaces in all modified files and create
@GitHub@ suggestion through the comments for removing that.
-}

module Hintman.Hint.TrailingSpaces
       ( getTrailingSpacesComments
       ) where

import Data.Text (stripEnd)
import Hintman.Core.Hint (HintType (TrailingSpaces), Line (..), toLines)
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..))
import Hintman.Hint.Position (getTargetCommentPosition)


-- | Creates 'Comment's on removing trailing spaces in all modified files.
getTrailingSpacesComments :: [ModifiedFile] -> [Comment]
getTrailingSpacesComments = foldMap getFileTrailingSpaceComments

getFileTrailingSpaceComments :: ModifiedFile -> [Comment]
getFileTrailingSpaceComments ModifiedFile{..} = maybeToMonoid $
    mapMaybe createComment . getTrailingSpaceLines . toLines . decodeUtf8
    <$> mfContent
  where
    -- | Create a 'Comment' from all necessary data.
    createComment :: Line -> Maybe Comment
    createComment Line{..} = do
        let commentPath = fileNameText
        commentPosition <- getTargetCommentPosition mfDelta lineNumber
        let commentBody = unlines [ "```suggestion", lineBody, "```" ]
        let commentHintType = TrailingSpaces
        Just Comment{..}

    fileNameText :: Text
    fileNameText = toText mfPath

-- | Take only lines which had trailing spaces with the stripped body.
getTrailingSpaceLines :: [Line] -> [Line]
getTrailingSpaceLines [] = []
getTrailingSpaceLines (Line{..}:rest) = let newBody = stripEnd lineBody in
    if newBody /= lineBody
    then Line lineNumber newBody : getTrailingSpaceLines rest
    else getTrailingSpaceLines rest
