{- | Functions for acquiring information about GitHub application installation.
-}

module Hintman.Installation
       ( createAccessToken
       ) where

import Prelude hiding (exp)

import Data.Aeson (decode)
import Data.Time (NominalDiffTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (Request (..), Response (..), httpLbs, parseUrlThrow)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))
import Web.JWT (JWTClaimsSet (..), Signer (RSAPrivateKey), encodeSigned, numericDate, stringOrURI)

import Hintman.Core.Token (AppInfo (..), InstallationAccessToken, InstallationId (..))


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
mkAuthToken :: AppInfo -> IO JwtToken
mkAuthToken AppInfo{..} = do
    currentTime <- utcTimeToPOSIXSeconds <$> getCurrentTime
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
mkTokenRequest :: InstallationId -> JwtToken -> IO Request
mkTokenRequest (InstallationId installationId) (JwtToken token) = do
    request <- parseUrlThrow $ toString $ mconcat
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

{- | Query GitHub to ask access token.
-}
createAccessToken :: InstallationId -> AppInfo -> IO (Maybe InstallationAccessToken)
createAccessToken installationId appInfo = do
    jwtToken <- mkAuthToken appInfo
    request  <- mkTokenRequest installationId jwtToken

    -- TODO: put in the context to not create each time
    manager  <- newTlsManager
    response <- httpLbs request manager

    -- TODO: remove code duplication with @downloadFile@
    let status = statusCode $ responseStatus response
    let body = responseBody response
    -- log D $ "Recieved a status code of " <> show status <> " from " <> url
    if status `elem` [200, 201]
        then
            -- log I $ "Successfully downloaded file from " <> url
            pure $ decode body
        else
            -- log E $ "Couldn't download file from " <> url
            pure Nothing
