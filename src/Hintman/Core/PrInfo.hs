-- | Core types related to pull request.

module Hintman.Core.PrInfo
       ( PrInfo (..)
       , Owner (..)
       , Repo (..)
       , Branch (..)
       , PrNumber (..)
       ) where

import Servant (ToHttpApiData (..))
import Text.Diff.Parse.Types (FileDeltas)


-- | Data type that represents the current pull request.
data PrInfo = PrInfo
    { prInfoOwner  :: !Owner
    , prInfoRepo   :: !Repo
    , prInfoBranch :: !Branch
    , prInfoNumber :: !PrNumber
    , prInfoDelta  :: !FileDeltas
    }

newtype Owner = Owner
    { unOwner :: Text
    } deriving newtype (Show, Eq, Hashable, ToHttpApiData)

newtype Repo = Repo
    { unRepo :: Text
    } deriving newtype (Show, Eq, Hashable, ToHttpApiData)

newtype Branch = Branch
    { unBranch :: Text
    }

newtype PrNumber = PrNumber
    { unPrNumber :: Int
    }

instance ToHttpApiData PrNumber where
    toUrlPiece :: PrNumber -> Text
    toUrlPiece (PrNumber num) = show num <> ".diff"
