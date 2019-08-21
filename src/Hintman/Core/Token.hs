{- | Data types to store information about webhook installations.
-}

module Hintman.Core.Token
       ( AppInfo (..)
       , GitHubToken (..)
       , InstallationId (..)
       , InstallationAccessToken (..)
       ) where

import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (withObject, withText, (.:))


{- | Information about GitHub application required for authentication.

* app id can be taken from the corresponding GitHub page
* private key is stored in the file and should be read upon configuration
-}
data AppInfo = AppInfo
    { appInfoId         :: !Int
    , appInfoPrivateKey :: !PrivateKey
    }

{- | Id of the GitHub app installation action. Used to issue access tokens.
-}
newtype InstallationId = InstallationId
    { unInstallationId :: Text
    } deriving stock (Show)

{- | GitHub access token.
-}
newtype GitHubToken = GitHubToken
    { unGitHubToken :: ByteString
    }

instance FromJSON GitHubToken where
    parseJSON = withText "GitHubToken" $ pure . GitHubToken . encodeUtf8

{- | Token that can be used to authenticate requests from the GitHub app. The
value of 'iatToken' can be used with 'OAuth' constructor from the @github@
library to sign requests.
-}
data InstallationAccessToken = InstallationAccessToken
    { iatToken     :: !GitHubToken
    , iatExpiresAt :: !UTCTime
    }

instance FromJSON InstallationAccessToken where
    parseJSON = withObject "InstallationAccessToken" $ \o -> do
        iatToken     <- o .: "token"
        iatExpiresAt <- o .: "expires_at"
        pure InstallationAccessToken{..}
