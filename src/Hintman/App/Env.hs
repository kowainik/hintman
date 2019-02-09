-- | 'ReaderT' environment for main application monad.

module Hintman.App.Env
       ( Env (..)
       ) where

import Hintman.Config (HintmanConfig)


newtype Env = Env
    { envConfig :: HintmanConfig
    }
