module Hintman.App.Error
       ( AppError (..)
       , AppErrorType (..)
       , AppException (..)
       , WithError
       , throwError
       , catchError
       , toHttpError

         -- * Error throwing helpers
       , throwOnNothing
       , throwOnNothingM
       , notFoundOnNothing
       , notFoundOnNothingM
       ) where

import Control.Monad.Except (MonadError)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))
import Servant.Server (err401, err404, err417, err500, errBody)

import qualified Control.Monad.Except as E (catchError, throwError)
import qualified Servant.Server as Servant (ServerError)


-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'.
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)
{-# INLINE throwError #-}

-- | Specialized version of 'E.catchError'.
catchError :: WithError m => m a -> (AppErrorType -> m a) -> m a
catchError action handler = action `E.catchError` (handler . appErrorType)
{-# INLINE catchError #-}

newtype SourcePosition = SourcePosition Text
    deriving newtype (Show, Eq)

-- | Display 'CallStack' as 'SourcePosition' in a format: @Module.function#line_number@.
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} =
        toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

{- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
'AppError' as 'Exception'.
-}
newtype AppException = AppException
    { unAppException :: AppError
    } deriving (Show)
      deriving anyclass (Exception)

-- | 'AppErrorType' with the corresponding 'CallStack'.
data AppError = AppError
    { appErrorCallStack :: !SourcePosition
    , appErrorType      :: !AppErrorType
    } deriving (Show, Eq)

-- | App errors type.
data AppErrorType
    {- | General not found. -}
    = NotFound
    {- | Some exceptional circumstance has happened stop execution and return.
    Optional text to provide some context in server logs.
    -}
    | ServerError Text
    {- | A required permission level was not met. Optional text to provide some context. -}
    | NotAllowed Text
    {- | Given inputs do not conform to the expected format or shape. Optional
    text to provide some context in server logs.
    -}
    | Invalid Text
    deriving (Show, Eq)

-- | Map 'AppError' into a HTTP error code.
toHttpError :: AppError -> Servant.ServerError
toHttpError AppError{..} = case appErrorType of
    NotFound        -> err404
    ServerError msg -> err500 { errBody = encodeUtf8 msg }
    NotAllowed msg  -> err401 { errBody = encodeUtf8 msg }
    Invalid msg     -> err417 { errBody = encodeUtf8 msg }

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure

-- | Extract the value from a 'Maybe' in @m@, throwing the given 'AppError' if
-- the value does not exist
throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err

-- | Similar to 'throwOnNothing' but throws a 'NotFound' if the value does not exist
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing = withFrozenCallStack . throwOnNothing NotFound

-- | Similar to 'throwOnNothingM' but throws a 'NotFound' if the value does not exist
notFoundOnNothingM :: WithError m => m (Maybe a) -> m a
notFoundOnNothingM = withFrozenCallStack . throwOnNothingM NotFound
