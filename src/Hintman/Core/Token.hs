{- | Data types to store information about webhook installations.
-}

module Hintman.Core.Token
       ( AppInfo (..)
       , GitHubToken (..)
       , InstallationId (..)
       , InstallationToken (..)
       ) where

import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (withText)


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
library to sign requests. Contains enough information to be renewed.
-}
data InstallationToken = InstallationToken
    { itToken          :: !GitHubToken
    , itExpiresAt      :: !UTCTime
    , itInstallationId :: !InstallationId
    }
