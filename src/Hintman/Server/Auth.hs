{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Authentication for incoming requests from GitHub.

module Hintman.Server.Auth
       ( HintmanAuthAPI
       , hintmanAuthServerContext
       ) where

import Network.Wai (Request)
import Servant (Context ((:.), EmptyContext))
import Servant.API (AuthProtect)
import Servant.Server (Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)


type HintmanAuthAPI = AuthProtect "GitHub"
type instance AuthServerData (AuthProtect "GitHub") = ()

type HintmanAuthContextHandlers = '[AuthHandler Request ()]
type HintmanAuthContext = Context HintmanAuthContextHandlers

hintmanAuthServerContext :: HintmanAuthContext
hintmanAuthServerContext = hintmanAuthHandler :. EmptyContext

hintmanAuthHandler :: AuthHandler Request ()
hintmanAuthHandler = mkAuthHandler handler
  where
    handler :: Request -> Handler ()
    handler = liftIO . print
