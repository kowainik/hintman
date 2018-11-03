module Hintman.App
       ( runHintman
       ) where

import Network.Wai.Handler.Warp (run)

import Hintman.Server (app)


runHintman :: IO ()
runHintman = do
    print ("Starting hintman site at http://localhost:8000" :: Text)
    run 8000 app
