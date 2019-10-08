{-# LANGUAGE DataKinds #-}

{- | This module contains functions to construct and submit comments.
-}

module Hintman.Comment
       ( submitReview
       ) where

import Data.Aeson (Value, encode)
import GitHub.Auth (Auth (..))
import GitHub.Data.Request (CommandMethod (Post), RW (RW), Request, command)
import GitHub.Request (executeRequest)

import Hintman.Core.PrInfo (Owner (..), PrInfo (..), PrNumber (..), Repo (..))
import Hintman.Core.Review (Comment (..), PullRequestReview (..), ReviewEvent (..))
import Hintman.Core.Token (GitHubToken (..))


{- | This function submits PR review according to the following API endpoint:

* https://developer.github.com/v3/pulls/reviews/#create-a-pull-request-review
-}
submitReview
    :: (MonadIO m, WithLog env m)
    => GitHubToken
    -> PrInfo
    -> [Comment]
    -> m ()
submitReview (GitHubToken token) PrInfo{..} comments = do
    response <- liftIO $ executeRequest (OAuth token) reviewCommand
    case response of
        Left err -> log E $ "Error submitting review: " <> show err
        Right _  -> log I "Successfully submitted review!"
  where
    reviewCommand :: Request 'RW Value
    reviewCommand = command
        Post
        [ "repos"
        , unOwner prInfoOwner
        , unRepo prInfoRepo
        , "pulls"
        , show $ unPrNumber prInfoNumber
        , "reviews"
        ]
        (encode pullRequestReview)

    pullRequestReview :: PullRequestReview
    pullRequestReview = PullRequestReview
        { prrBody     = "Review by Hintman"  -- TODO: think about the text
        , prrEvent    = Commented  -- TODO: this can be configurable
        , prrComments = comments
        }
