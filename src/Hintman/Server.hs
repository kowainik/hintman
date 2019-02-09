{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hintman.Server
       ( hintmanApp
       ) where

import Servant ((:>), Application, Get, JSON, NoContent (..), Post, Server, hoistServerWithContext,
                serveWithContext)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.Server.Generic (AsServerT)

import Hintman.App (App, Env, runAppAsHandler)
import Hintman.Server.Auth (HintmanAuthAPI, HintmanAuthContextHandlers, hintmanAuthServerContext)


data HintmanSite route = HintmanSite
    { hintmanTheAnswerRoute :: route
        :- "api"
        :> "the-answer"
        :> Get '[JSON] Int

    , hintmanAuthRoute :: route
        :- HintmanAuthAPI
        :> Post '[JSON] NoContent
    } deriving (Generic)

type HintmanAPI = ToServantApi HintmanSite

hintmanServer :: HintmanSite (AsServerT App)
hintmanServer = HintmanSite
    { hintmanTheAnswerRoute = pure 42
    , hintmanAuthRoute = \() -> pure NoContent
    }

server :: Env -> Server HintmanAPI
server env = hoistServerWithContext
    (Proxy @HintmanAPI)
    (Proxy @HintmanAuthContextHandlers)
    (runAppAsHandler env)
    (toServant hintmanServer)

hintmanApp :: Env -> Application
hintmanApp env = serveWithContext
    (Proxy @HintmanAPI)
    hintmanAuthServerContext
    (server env)
