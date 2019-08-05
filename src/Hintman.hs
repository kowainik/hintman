module Hintman
       ( runHintman
       ) where

import Network.Wai.Handler.Warp (run)
import Servant.GitHub.Webhook (gitHubKey)
import System.Environment (lookupEnv)

import Hintman.App (Env (..))
import Hintman.Cli (CliArguments (..), cliArguments)
import Hintman.Config (loadFileConfig)
import Hintman.Webhook (hintmanApp)

import qualified Data.ByteString.Char8 as C8


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
    envConfig <- loadFileConfig "hintman-config.toml"
    key <- maybe mempty C8.pack <$> lookupEnv "KEY"
    let envGitHubKey = gitHubKey $ pure key
    run (cliArgumentsPort ctx) (hintmanApp Env{..})
