-- | Core types related to pull request.

module Hintman.Core.PrInfo
       ( PrInfo (..)
       , Owner (..)
       , Repo (..)
       , Sha (..)
       , PrNumber (..)

         -- * Helper types for JSON interaction
       , FullRepo (..)
       , Repositories (..)
       , displayFullRepo

         -- * Files
       , ModifiedFile (..)
       ) where

import Data.Aeson (withObject, (.:))
import Servant (ToHttpApiData (..))
import Text.Diff.Parse.Types (FileDelta, FileDeltas)

import qualified Data.Text as T


-- | Data type that represents the current pull request.
data PrInfo = PrInfo
    { prInfoOwner  :: !Owner
    , prInfoRepo   :: !Repo
    , prInfoHead   :: !Sha  -- ^ Sha of the HEAD, instead of branch name
    , prInfoNumber :: !PrNumber
    , prInfoDelta  :: !FileDeltas
    }

newtype Owner = Owner
    { unOwner :: Text
    } deriving newtype (Show, Eq, Hashable, ToHttpApiData)

newtype Repo = Repo
    { unRepo :: Text
    } deriving newtype (Show, Eq, Hashable, ToHttpApiData)

-- | Full repo name in the form: @onwer/repo@
-- TODO: is there better name?...
data FullRepo = FullRepo
    { frOwner :: !Owner
    , frRepo  :: !Repo
    } deriving stock (Eq, Generic)
      deriving anyclass (Hashable)

displayFullRepo :: FullRepo -> Text
displayFullRepo FullRepo{..} = unOwner frOwner <> "/" <> unRepo frRepo

{- | Parser from the following endpoint:

* https://developer.github.com/v3/apps/installations/#list-repositories
-}
instance FromJSON FullRepo where
    parseJSON = withObject "FullRepo" $ \o -> do
        fullName <- o .: "full_name"
        case T.splitOn "/" fullName of
            [Owner -> frOwner, Repo -> frRepo] -> pure FullRepo{..}
            _ -> fail $ "Expected owner/repo but got: " <> toString fullName

newtype Repositories = Repositories
    { unRepositories :: [FullRepo]
    }

{- | Parser from the following endpoint:

* https://developer.github.com/v3/apps/installations/#list-repositories
-}

instance FromJSON Repositories where
    parseJSON = withObject "Repositories" $ \o -> do
        repositories <- o .: "repositories"
        pure $ Repositories repositories

-- | Commit hash.
newtype Sha = Sha
    { unSha :: Text
    }

newtype PrNumber = PrNumber
    { unPrNumber :: Int
    }

instance ToHttpApiData PrNumber where
    toUrlPiece :: PrNumber -> Text
    toUrlPiece (PrNumber num) = show num <> ".diff"

-- | Information about modified file.
data ModifiedFile = ModifiedFile
    { mfDelta   :: !FileDelta
    , mfPath    :: !FilePath  -- ^ Path to the destination file
    , mfContent :: !(Maybe ByteString) -- ^ File content (if retrieved)
    } deriving (Show)
