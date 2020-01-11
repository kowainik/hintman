{-| Contains function to catch trailing spaces in all modified files and create
@GitHub@ suggestion through the comments for removing that.
-}

module Hintman.Hint.TrailingSpaces
       ( getTrailingSpacesComments
       ) where

import Data.Text (stripEnd)
import Data.Vector (Vector)

import Hintman.Core.Hint (Hint, HintType (TrailingSpaces), Line (..), simpleSuggestion)
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Core.Review (Comment (..), createComment)

import qualified Data.Vector as V

-- | Creates 'Comment's on removing trailing spaces in all modified files.
getTrailingSpacesComments :: [ModifiedFile] -> [Comment]
getTrailingSpacesComments = foldMap getFileTrailingSpaceComments

getFileTrailingSpaceComments :: ModifiedFile -> [Comment]
getFileTrailingSpaceComments mf@ModifiedFile{..} = mapMaybe spawnComment $
    V.toList $ getTrailingSpaceLines mfLines
  where
    -- | Spawns a "TrailingSpaces" comment on a given line.
    spawnComment :: Line -> Maybe Comment
    spawnComment line@Line{..} = createComment mf line $ hint lineBody

    -- | Creates a "TrailingSpaces" hint with given line body.
    hint :: Text -> Hint
    hint = simpleSuggestion TrailingSpaces "Trailing spaces"

-- | Take only lines which had trailing spaces with the stripped body.
getTrailingSpaceLines :: Vector Line -> Vector Line
getTrailingSpaceLines = V.mapMaybe trimSpaces
  where
    trimSpaces :: Line -> Maybe Line
    trimSpaces Line{..} = let newBody = stripEnd lineBody in
        if newBody /= lineBody
        then Just $ Line lineNumber newBody
        else Nothing
