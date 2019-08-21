-- | Application monad for the server.

module Hintman.App.Monad
       ( App (..)
       , AppEnv
       , runAppAsHandler
       ) where

import Control.Exception (try)
import Servant (Handler (..))

import Hintman.App.Env (Env)

type AppEnv = Env App

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving (Functor, Applicative, Monad, MonadReader AppEnv, MonadIO)

-- | Running 'App' as 'Handler'.
runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env = Handler . ExceptT . try . usingReaderT env . unApp
