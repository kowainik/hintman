module Hintman
       ( runHintman
       ) where

import Colog (richMessageAction)
import Data.X509 (PrivKey (PrivKeyRSA))
import Data.X509.File (readKeyFile)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import Hintman.App (Env (..), runAppLogIO_)
import Hintman.Cli (CliArguments (..), cliArguments)
import Hintman.Config (loadFileConfig)
import Hintman.Core.Key (gitHubKey)
import Hintman.Core.Token (AppInfo (..))
import Hintman.Effect.TokenStorage (initialiseInstallationIds)
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

    key <- maybe (error "KEY not found") C8.pack <$> lookupEnv "KEY"
    let envGitHubKey = gitHubKey $ pure key

    pkPath <- fromMaybe (error "PK_PATH not found") <$> lookupEnv "PK_PATH"
    [PrivKeyRSA pk] <- readKeyFile pkPath
    let envAppInfo = AppInfo
            { appInfoId = 20170
            , appInfoPrivateKey = pk
            }

    envTokenCache <- newIORef mempty
    let envLogAction = richMessageAction

    let env = Env{..}

    -- this function changes mutable variable 'envTokenCache'
    -- it's not dangerous but might be surprising
    runAppLogIO_ env initialiseInstallationIds

    run (cliArgumentsPort ctx) (hintmanApp env)
