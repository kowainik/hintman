{- | Functions for acquiring information about GitHub application installation.
-}

module Hintman.Installation
       ( createInstallationToken
       , renewToken
       ) where

import Prelude hiding (exp)

import Data.Aeson (decode, withObject, (.:))
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (Request (..), Response (..), httpLbs, parseUrlThrow)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))
import Web.JWT (JWTClaimsSet (..), Signer (RSAPrivateKey), encodeSigned, numericDate, stringOrURI)

import Hintman.App (AppErrorType (..), Has, WithError, grab, throwError)
import Hintman.Core.Token (AppInfo (..), GitHubToken, InstallationId (..), InstallationToken (..))


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
mkAuthToken :: MonadIO m => AppInfo -> m JwtToken
mkAuthToken AppInfo{..} = do
    currentTime <- liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime
    let expiresAt = currentTime + jwtExpiryTime
    let issuer = stringOrURI $ show @Text appInfoId
    let claimSet  = mempty
            { iat = numericDate currentTime
            , exp = numericDate expiresAt
            , iss = issuer
            }

    pure $ JwtToken $ encodeSigned (RSAPrivateKey appInfoPrivateKey) mempty claimSet

{- | Create HTTP request from the generated token
-}
mkTokenRequest :: MonadIO m => InstallationId -> JwtToken -> m Request
mkTokenRequest (InstallationId installationId) (JwtToken token) = do
    request <- liftIO $ parseUrlThrow $ toString $ mconcat
        [ "https://api.github.com/installations/"
        , installationId
        , "/access_tokens"
        ]

    pure request
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 token)
            , ("Accept", "application/vnd.github.machine-man-preview+json")
            , ("User-Agent", "kowainik/hintman")
            ]
        }

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
    :: (MonadIO m, Has AppInfo env, WithError m, WithLog env m)
    => InstallationId
    -> m InstallationToken
createInstallationToken installationId = do
    appInfo  <- grab @AppInfo
    jwtToken <- mkAuthToken appInfo
    request  <- mkTokenRequest installationId jwtToken

    -- TODO: put in the context to not create each time
    manager  <- newTlsManager
    response <- liftIO $ httpLbs request manager

    -- TODO: remove code duplication with @downloadFile@
    let status = statusCode $ responseStatus response
    let body = responseBody response
    log D $ "Recieved a status code of " <> show status <> " from " <> show installationId
    if status `elem` [200, 201]
        then do
            log I $ "Successfully created token from: " <> show installationId
            case decode @AccessToken body of
                Nothing -> throwError $ ServerError $
                    "Error decoding token from: " <> decodeUtf8 body
                Just AccessToken{..} -> pure InstallationToken
                    { itToken = atToken
                    , itExpiresAt = atExpiresAt
                    , itInstallationId = installationId
                    }
        else do
            log E $ "Couldn't create token for: " <> show installationId
            throwError $ ServerError $
                "Invalid status " <> show status <> " for " <> show installationId

{- | This function takes 'InstallationAccessToken' and either returns itself if
it can live long enough or creates a new token by calling 'createAccessToken'
function. 'InstallationToken' contains enough information to be renewed.
-}
renewToken
    :: (MonadIO m, Has AppInfo env, WithError m, WithLog env m)
    => InstallationToken
    -> m InstallationToken
renewToken token@InstallationToken{..} = do
    now <- liftIO getCurrentTime

    -- if less than 10 seconds left for token expiration, create new one
    if now `diffUTCTime` itExpiresAt <= 10
        then createInstallationToken itInstallationId
        else pure token
