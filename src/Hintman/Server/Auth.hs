{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Authentication for incoming requests from GitHub.

module Hintman.Server.Auth
       ( HintmanAuthAPI
       , HintmanAuthContextHandlers
       , hintmanAuthServerContext
       ) where

import Crypto.Hash (Digest, digestFromByteString)
import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.ByteString (breakSubstring)
import Data.List (lookup)
import Network.Wai (Request (..))
import Servant (Context ((:.), EmptyContext), throwError)
import Servant.API (AuthProtect)
import Servant.Server (Handler, ServantErr (..), err400, err401)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import System.Environment (getEnv)

import Hintman.GitHub.Auth (GithubWebhookSecret (..))


type HintmanAuthAPI = AuthProtect "GitHub"
type instance AuthServerData (AuthProtect "GitHub") = ()

type HintmanAuthContextHandlers = '[AuthHandler Request ()]
type HintmanAuthContext = Context HintmanAuthContextHandlers

verifySignature :: Request -> Handler ()
verifySignature req = do
    body <- liftIO $ requestBody req
    verifySignature' body header
  where
    header :: Maybe ByteString
    header = lookup "x-hub-signature" (requestHeaders req)

verifySignature' :: ByteString -> Maybe ByteString -> Handler ()
verifySignature' _ Nothing = throwError $ err401 { errBody = "Empty signature." }
verifySignature' payload (Just signature) = do
    GithubWebhookSecret key <- liftIO $  GithubWebhookSecret . toText <$> getEnv "GITHUB_WEBHOOK_SECRET"
    let ourDigest = sha1sum (encodeUtf8 key) payload
    case theirDigest of
      Nothing -> throwError $ err400 { errBody = "Invalid signature." }
      Just d  -> unless (d == ourDigest) $ throwError $ err401 { errBody = "Wrong signature."}
  where
    theirDigest :: Maybe (Digest SHA1)
    theirDigest = digestFromByteString $ snd $ breakSubstring signature "="

    sha1sum :: ByteString -> ByteString -> Digest SHA1
    sha1sum key msg = hmacGetDigest $ hmac key msg

hintmanAuthServerContext :: HintmanAuthContext
hintmanAuthServerContext = hintmanAuthHandler :. EmptyContext

hintmanAuthHandler :: AuthHandler Request ()
hintmanAuthHandler = mkAuthHandler handler
  where
    handler :: Request -> Handler ()
    handler req = do
        verifySignature req
        print req
