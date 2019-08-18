-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , module Json
       , module Time
       , module Web
       ) where

import Relude
import Relude.Extra.Newtype as Relude (un)

import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Time.Clock as Time (UTCTime)
import Servant.API as Web ((:>), Capture, Get, JSON, NoContent (NoContent), Post, ReqBody)
