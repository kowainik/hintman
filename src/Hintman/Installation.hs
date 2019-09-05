{- | Functions for acquiring information about GitHub application installation.
-}

module Hintman.Installation
       ( -- * Main base API
         createInstallationToken
       , renewToken

         -- * Helper and more general functions
       , AuthType (..)
       , mkJwtToken
       , mkInstallationsRequest
       , mkRepositoriesRequest
       , performRequest
       ) where

import Prelude hiding (exp)

import Data.Aeson (decode, withObject, (.:))
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (Request (..), Response (..), httpLbs, parseUrlThrow)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Method, Status (..))
import Web.JWT (JWTClaimsSet (..), Signer (RSAPrivateKey), encodeSigned, numericDate, stringOrURI)

import Hintman.App (AppErrorType (..), Has, WithError, grab, throwError)
import Hintman.Core.Token (AppInfo (..), GitHubToken (..), InstallationId (..),
                           InstallationToken (..))

import qualified Data.Text as T


{- | Internal data type representing the result of 'encodeSigned' function.
-}
newtype JwtToken = JwtToken Text
    deriving (Show)

{- | Expiration time of the JWT token. Set to 10 minutes (max allowed by
GitHub).
-}
jwtExpiryTime :: NominalDiffTime
jwtExpiryTime = 600

{- | Create JWT token that will be used later to issue
'InstallationAccessToken'.
-}
mkJwtToken :: (MonadIO m, MonadReader env m, Has AppInfo env) =>  m JwtToken
mkJwtToken = do
    AppInfo{..} <- grab @AppInfo
    currentTime <- liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime
    let expiresAt = currentTime + jwtExpiryTime
    let issuer = stringOrURI $ show @Text appInfoId
    let claimSet  = mempty
            { iat = numericDate currentTime
            , exp = numericDate expiresAt
            , iss = issuer
            }

    pure $ JwtToken $ encodeSigned (RSAPrivateKey appInfoPrivateKey) mempty claimSet

{- | Describes auth types for GitHub apps. There are two ways:

1. Using 'JwtToken': only for some non-sensitive endpoints
2. Using 'GitHubToken': for most of the endpoints
-}
data AuthType
    = Bearer JwtToken
    | Token GitHubToken

renderAuthType :: AuthType -> ByteString
renderAuthType = \case
    Bearer (JwtToken token) -> "Bearer " <> encodeUtf8 token
    Token (GitHubToken token) -> "token " <> token

{- | Create HTTP request for Hintman to query information that requires
'JwtToken'. This function sets all necessary headers of the request.
-}
mkAuthRequest
    :: Method  -- ^ REST method, like "POST" or "GET"
    -> [Text]  -- ^ Path pieces of the request URL; provide without "/"
    -> AuthType  -- ^ Auth token; generate using 'mkJwtToken'
    -> IO Request
mkAuthRequest apiMethod urlParts authType = do
    request <- parseUrlThrow $ toString $ T.intercalate "/"
        $ "https://api.github.com" : urlParts

    pure request
        { method = apiMethod
        , requestHeaders =
            [ ("Authorization", renderAuthType authType)
            , ("Accept", "application/vnd.github.machine-man-preview+json")
            , ("User-Agent", "kowainik/hintman")  -- TODO: use hintman version here
            ]
        }

-- | Create request which asks for all installation ids.
mkInstallationsRequest :: JwtToken -> IO Request
mkInstallationsRequest token = mkAuthRequest
    "GET"
    ["app", "installations"]
    (Bearer token)

-- | Create request which asks for 'AcessToken'.
mkAccessTokenRequest :: InstallationId -> JwtToken -> IO Request
mkAccessTokenRequest (InstallationId installationId) token = mkAuthRequest
    "POST"
    ["app", "installations", show installationId, "access_tokens"]
    (Bearer token)

-- | Create request which returns all repositories for this access token.
mkRepositoriesRequest :: GitHubToken -> IO Request
mkRepositoriesRequest token = mkAuthRequest
    "GET"
    ["installation", "repositories"]
    (Token token)

{- | Internal data type for parsing GitHub response containing access token for
Hintman.
-}
data AccessToken = AccessToken
    { atToken     :: !GitHubToken
    , atExpiresAt :: !UTCTime
    }

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \o -> do
        atToken     <- o .: "token"
        atExpiresAt <- o .: "expires_at"
        pure AccessToken{..}

{- | Query GitHub to ask access token.
-}
createInstallationToken
    :: (MonadIO m, WithError m, WithLog env m)
    => JwtToken
    -> InstallationId
    -> m InstallationToken
createInstallationToken jwtToken installationId = do
    AccessToken{..} <- performRequest @AccessToken
        $ mkAccessTokenRequest installationId jwtToken

    pure InstallationToken
        { itToken = atToken
        , itExpiresAt = atExpiresAt
        , itInstallationId = installationId
        }

{- | Perform 'Request' constructed with IO action.
-}
performRequest
    :: forall a m env .
       (FromJSON a, MonadIO m, WithError m, WithLog env m)
    => IO Request  -- ^ Action that constructs and returns request
    -> m a         -- ^ JSON result of the request
performRequest requestAction = do
    request <- liftIO requestAction

    -- TODO: put in the context to not create each time
    manager  <- newTlsManager
    response <- liftIO $ httpLbs request manager

    -- TODO: remove code duplication with @downloadFile@
    let status = statusCode $ responseStatus response
    let body = responseBody response
    log D $ "Recieved a status code: " <> show status

    if status `elem` [200, 201]
        then do
            log D "Successfully performed request!"
            case decode @a body of
                Just a  -> pure a
                Nothing -> throwError $ ServerError $
                    "Error decoding request from: " <> decodeUtf8 body
        else do
            log E "Couldn't perform request :("
            throwError $ ServerError $ "Invalid status " <> show status

{- | This function takes 'InstallationAccessToken' and either returns itself if
it can live long enough or creates a new token by calling 'createAccessToken'
function. 'InstallationToken' contains enough information to be renewed.
-}
renewToken
    :: (MonadIO m, Has AppInfo env, WithError m, WithLog env m)
    => InstallationToken
    -> m InstallationToken
renewToken token@InstallationToken{..} = do
    jwtToken <- mkJwtToken
    now <- liftIO getCurrentTime

    -- if less than 10 seconds left for token expiration, create new one
    if now `diffUTCTime` itExpiresAt <= 10
        then createInstallationToken jwtToken itInstallationId
        else pure token
