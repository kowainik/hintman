{- | Functions for authenticating GitHub app. -}

module Hintman.GitHub.Auth
       ( JwtToken (..)
       , GithubWebhookSecret (..)
       , mkGitHubAuth
       ) where

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Environment (getEnv)

import qualified Web.JWT as Jwt


{- | Notice that the private key must be in PEM format, but the newlines should
be stripped and replaced with the literal `\n`. This can be done in the terminal
as such:

@
export GITHUB_PRIVATE_KEY=`awk '{printf "%s\\n", $0}' private-key.pem`
@

Env variable: GITHUB_PRIVATE_KEY
-}
newtype GithubPrivateKey = GithubPrivateKey Text

{- | You set the webhook secret when you create your app. This verifies that the
webhook is really coming from GH.

Env variable: GITHUB_WEBHOOK_SECRET
-}
newtype GithubWebhookSecret = GithubWebhookSecret Text

{- | Get the app identifier—an integer—from your app page after you create your
app. This isn't actually a secret, but it is something easier to configure at
runtime.

Env variable: GITHUB_APP_IDENTIFIER
-}
newtype GithubAppIdentifier = GithubAppIdentifier Text

{- | Before each request to our app, we want to instantiate an Octokit client. Doing so requires that we construct a JWT.
* HTTP://jwt.io/introduction/

We have to also sign that JWT with our private key, so GitHub can be sure that
 a) it came from us
 b) it hasn't been altered by a malicious third party
-}
data AuthPayload = AuthPayload
    { -- | The time that this JWT was issued, _i.e._ now.
      authPayloadIssuedAt :: !POSIXTime

      {- | How long is the JWT good for (in seconds)?
      Let's say it can be used for 10 minutes before it needs to be refreshed.
      TODO: we don't actually cache this token, we regenerate a new one every time!
      -}
    , authPayloadExpiry   :: !POSIXTime

      -- | Your GitHub App's identifier number, so GitHub knows who issued the
      -- JWT, and know what permissions this token has.
    , authPayloadId       :: !GithubAppIdentifier
    }

newtype JwtToken = JwtToken
    { unJwtToken :: Text
    }

authPayloadToJwt :: AuthPayload -> Jwt.JWTClaimsSet
authPayloadToJwt AuthPayload{..} = Jwt.def
    { Jwt.iat = Jwt.numericDate authPayloadIssuedAt
    , Jwt.exp = Jwt.numericDate authPayloadExpiry
    , Jwt.iss = Jwt.stringOrURI $ un authPayloadId
    }

mkGitHubAuth :: IO JwtToken
mkGitHubAuth = do
    privateKey    <- GithubPrivateKey    . toText <$> getEnv "GITHUB_PRIVATE_KEY"
    authPayloadId <- GithubAppIdentifier . toText <$> getEnv "GITHUB_APP_IDENTIFIER"

    authPayloadIssuedAt <- liftIO getPOSIXTime
    let authPayloadExpiry = authPayloadIssuedAt + 10 * 60

    -- TODO: use RS256 after switching to jwt-0.8
    pure $ JwtToken $ Jwt.encodeSigned Jwt.HS256 (Jwt.secret $ un privateKey) $ authPayloadToJwt AuthPayload{..}
