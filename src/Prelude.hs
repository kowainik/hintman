-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , module Web
       ) where

import Relude
import Relude.Extra.Newtype as Relude (un)

import Servant.API as Web ((:>), Capture, Get, JSON, NoContent (NoContent), Post, ReqBody)
