{-# LANGUAGE PatternSynonyms #-}

-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , module Colog
       , module Json
       , module Time
       , module Web
       , module UnliftIO

       , WithLog
       ) where

import Relude
import Relude.Extra.Newtype as Relude (un)

import Colog (pattern D, pattern E, pattern I, LogAction (..), Message, Severity (..), pattern W,
              log)
import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Time.Clock as Time (UTCTime)
import Servant.API as Web ((:>), Capture, Get, JSON, NoContent (NoContent), Post, ReqBody)
import UnliftIO (MonadUnliftIO)

-- Internal
import qualified Colog (WithLog)


-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Message m
