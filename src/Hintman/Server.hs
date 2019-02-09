{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hintman.Server
       ( HintmanEnv (..)
       , hintmanApp
       ) where

import Servant ((:>), Application, Get, Handler, JSON, NoContent (..), Post, Server,
                hoistServerWithContext, serveWithContext)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.Server.Generic (AsServerT)

import Hintman.Config (HintmanConfig)
import Hintman.Server.Auth (HintmanAuthAPI, HintmanAuthContextHandlers, hintmanAuthServerContext)


newtype HintmanEnv = HintmanEnv
    { hintmanEnvConfig :: HintmanConfig
    }

type HintmanAppM = ReaderT HintmanEnv Handler

runAppAsHandler :: HintmanEnv -> HintmanAppM a -> Handler a
runAppAsHandler = usingReaderT


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

hintmanServer :: HintmanSite (AsServerT HintmanAppM)
hintmanServer = HintmanSite
    { hintmanTheAnswerRoute = pure 42
    , hintmanAuthRoute = \() -> pure NoContent
    }

server :: HintmanEnv -> Server HintmanAPI
server env = hoistServerWithContext
    (Proxy @HintmanAPI)
    (Proxy @HintmanAuthContextHandlers)
    (runAppAsHandler env)
    (toServant hintmanServer)

hintmanApp :: HintmanEnv -> Application
hintmanApp env = serveWithContext
    (Proxy @HintmanAPI)
    hintmanAuthServerContext
    (server env)
