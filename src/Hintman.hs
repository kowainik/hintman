module Hintman
       ( runHintman
       ) where

import Network.Wai.Handler.Warp (run)

import Hintman.Cli (Context(..), cliContext)
import Hintman.Server (app)


runHintman :: IO ()
runHintman = cliContext >>= runOn


-- | Prints the logging status in this context
printLoggingStatus :: Context -> IO ()
printLoggingStatus ctx
    | contextLogging ctx = putTextLn "Logging is enabled"
    | otherwise          = putTextLn "Logging is disabled"

-- | Runs the program with a given context
runOn :: Context -> IO ()
runOn ctx = do
    printLoggingStatus ctx
    putTextLn "Starting hintman site at https://localhost:8000"
    run 8000 app
