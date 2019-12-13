{- | Data types and instances for PR review.
-}

module Hintman.Core.Review
       ( PullRequestReview (..)
       , ReviewEvent (..)
       , Comment (..)
       , createComment
       ) where

import Data.Aeson (object, (.=))

import Hintman.Core.Hint (Hint, Line (..), formatHint)
import Hintman.Core.PrInfo (ModifiedFile (..))
import Hintman.Hint.Position (getTargetCommentPosition)


data PullRequestReview = PullRequestReview
    { prrBody     :: !Text
    , prrEvent    :: !ReviewEvent
    , prrComments :: ![Comment]
    }

instance ToJSON PullRequestReview where
    toJSON PullRequestReview{..} = object
        [ "body"     .= prrBody
        , "event"    .= prrEvent
        , "comments" .= prrComments
        ]

data ReviewEvent
    = Approved
    | ChangesRequested
    | Commented

instance ToJSON ReviewEvent where
    toJSON = \case
        Approved         -> "APPROVE"
        ChangesRequested -> "REQUEST_CHANGES"
        Commented        -> "COMMENT"

data Comment = Comment
    { -- | Path to file
      commentPath     :: !Text

      -- | The position in the diff where you want to add a review comment. Note
      -- this value is not the same as the line number in the file.
    , commentPosition :: !Int

      -- | Text of the review comment
    , commentHint     :: !Hint
    } deriving (Show, Eq)

instance ToJSON Comment where
    toJSON Comment{..} = object
        [ "path"     .= commentPath
        , "position" .= commentPosition
        , "body"     .= formatHint commentHint
        ]

-- | Create a 'Comment' from all necessary data.
createComment :: ModifiedFile -> Line -> Hint -> Maybe Comment
createComment ModifiedFile{..} Line{..} commentHint = do
    let commentPath = fileNameText
    commentPosition <- getTargetCommentPosition mfDelta lineNumber
    Just Comment{..}
  where
    fileNameText :: Text
    fileNameText = toText mfPath
