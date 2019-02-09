-- | Application monad for the server.

module Hintman.App.Monad
       ( App (..)
       , runAppAsHandler
       ) where

import Servant (Handler)

import Hintman.App.Env (Env)


-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT Env Handler a
    } deriving (Functor, Applicative, Monad, MonadReader Env)

-- | Running 'App' as 'Handler'.
runAppAsHandler :: Env -> App a -> Handler a
runAppAsHandler env = usingReaderT env . unApp
