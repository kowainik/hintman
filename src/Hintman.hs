module Hintman
       ( runHintman
       ) where

import Network.Wai.Handler.Warp (run)

import Hintman.App (Env (..))
import Hintman.Cli (Context (..), cliContext)
import Hintman.Config (loadFileConfig)
import Hintman.Server (hintmanApp)


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
    let siteString = "https://localhost:" <> show (contextPort ctx)
    putTextLn ("Starting hintman site at " <> siteString)
    config <- loadFileConfig "hintman-config.toml"
    run (contextPort ctx) (hintmanApp $ Env config)
