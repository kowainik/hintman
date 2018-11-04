{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hintman.Server
       ( hintmanApp
       ) where

import Servant ((:>), Application, Get, JSON, NoContent (..), Post, serveWithContext)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.Server.Generic (AsServer)

import Hintman.Server.Auth (HintmanAuthAPI, hintmanAuthServerContext)


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

hintmanServer :: HintmanSite AsServer
hintmanServer = HintmanSite
    { hintmanTheAnswerRoute = pure 42
    , hintmanAuthRoute = \() -> pure NoContent
    }

hintmanApp :: Application
hintmanApp = serveWithContext (Proxy @HintmanAPI) hintmanAuthServerContext
           $ toServant hintmanServer
