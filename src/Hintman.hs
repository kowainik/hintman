module Hintman
       ( runHintman
       ) where

import Network.Wai.Handler.Warp (run)

import Hintman.App (Env (..))
import Hintman.Cli (CliArguments (..), cliArguments)
import Hintman.Config (loadFileConfig)
import Hintman.Server (hintmanApp)


runHintman :: IO ()
runHintman = cliArguments >>= runOn


-- | Prints the logging status in this context
printLoggingStatus :: CliArguments -> IO ()
printLoggingStatus ctx
    | cliArgumentsLogging ctx = putTextLn "Logging is enabled"
    | otherwise               = putTextLn "Logging is disabled"

-- | Runs the program with a given context
runOn :: CliArguments -> IO ()
runOn ctx = do
    printLoggingStatus ctx
    let siteString = "https://localhost:" <> show (cliArgumentsPort ctx)
    putTextLn ("Starting hintman site at " <> siteString)
    config <- loadFileConfig "hintman-config.toml"
    run (cliArgumentsPort ctx) (hintmanApp $ Env config)
