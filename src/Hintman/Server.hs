{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hintman.Server
       ( app
       ) where

import Servant ((:>), Application, Get, JSON)

import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServer, genericServe)

newtype HintmanSite route = HintmanSite {
    hintmanGetRoute :: route :- "api" :> "the-answer" :> Get '[JSON] Int
    } deriving (Generic)

hintmanServer :: HintmanSite AsServer
hintmanServer = HintmanSite
    { hintmanGetRoute = return 42
    }

app :: Application
app = genericServe hintmanServer
