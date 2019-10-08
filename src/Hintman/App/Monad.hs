-- | Application monad for the server.

module Hintman.App.Monad
       ( App (..)
       , AppEnv

         -- * Runners
       , runAppAsHandler
       , runAppLogIO
       , runAppLogIO_

         -- * Internal helpers
       , runAppAsIO
       ) where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..), liftEither)
import Relude.Extra.Bifunctor (firstF)
import Servant (Handler (..))

import Hintman.App.Env (Env)
import Hintman.App.Error (AppError, AppException (..), toHttpError)


-- | 'Env' data type parameterized by 'App' monad
type AppEnv = Env App

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving newtype ( Functor, Applicative, Monad, MonadReader AppEnv
                       , MonadIO, MonadUnliftIO)

instance MonadError AppError App where
    throwError :: AppError -> App a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError :: App a -> (AppError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

{- | Helper for running route handlers in IO. Catches exception of type
'AppException' and unwraps 'AppError' from it. Do not use this function to run
the application.
-}
runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env

{- | Helper for running 'App'. Do not use this function to run the application.
-}
runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp

----------------------------------------------------------------------------
-- Application runners with logging
----------------------------------------------------------------------------

-- | Runs application as servant 'Handler'.
runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runAppLogIO env app
    liftEither $ first toHttpError res

-- | Runs application like 'runAppAsIO' but also logs error.
runAppLogIO :: AppEnv -> App a -> IO (Either AppError a)
runAppLogIO env app = do
    appRes <- runAppAsIO env app
    logRes <- whenLeft (Right ()) appRes (logErrorIO env)
    pure $ appRes <* logRes

-- | Like 'runAppAsIO' but discards result.
runAppLogIO_ :: AppEnv -> App a -> IO ()
runAppLogIO_ env app = void $ runAppLogIO env app

----------------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------------

logErrorIO :: AppEnv -> AppError -> IO (Either AppError ())
logErrorIO env err = runAppAsIO env $ log E $ show err
