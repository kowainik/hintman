-- | Core types related to pull request.

module Hintman.Core.PrInfo
       ( Repo (..)
       , Owner (..)
       , PrNumber (..)
       ) where

import Servant (ToHttpApiData (..))


newtype Repo = Repo
    { unRepo :: Text
    } deriving newtype (ToHttpApiData)

newtype Owner = Owner
    { unOwner :: Text
    } deriving newtype (ToHttpApiData)

newtype PrNumber = PrNumber
    { unPrNumber :: Int
    }

instance ToHttpApiData PrNumber where
    toUrlPiece :: PrNumber -> Text
    toUrlPiece (PrNumber num) = show num <> ".diff"
