{-# LANGUAGE BangPatterns #-}

module Hintman
       ( runHintman
       , mkAppEnv
       , mkTestAppEnv
       ) where

import Colog (richMessageAction)
import Data.X509 (PrivKey (PrivKeyRSA))
import Data.X509.File (readKeyFile)
import Data.X509.Memory (readKeyFileFromMemory)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import Hintman.App (Env (..), runAppLogIO_)
import Hintman.App.Monad (AppEnv)
import Hintman.Cli (CliArguments (..), cliArguments, defaultCliArguments)
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

-- | Creates 'AppEnv' from the CLI arguments.
mkAppEnv :: CliArguments -> IO AppEnv
mkAppEnv cli = do
    printLoggingStatus cli

    -- get port
    portEnv <- lookupEnv "PORT"
    let envPort = fromMaybe 8080 $ (portEnv >>= readMaybe) <|> cliArgumentsPort cli

    let siteString = "https://localhost:" <> show envPort
    putTextLn ("Starting hintman site at: " <> siteString)

    envConfig <- loadFileConfig "hintman-config.toml"

    -- get webhook secret
    !key <- maybe (error "KEY not found") C8.pack <$> lookupEnv "KEY"
    let envGitHubKey = gitHubKey $ pure key

    -- get private key
    pkValEnv <- lookupEnv "PK_VAL"
    pk <- case pkValEnv >>= readMaybe @String of
        Just pkVal -> do
            putTextLn "Using value of PK_VAL..."
            putStrLn pkVal
            let privKey = readKeyFileFromMemory $ encodeUtf8 pkVal
            print privKey
            [PrivKeyRSA pk] <- pure privKey
            pure pk
        Nothing -> do
            putTextLn "Using value of PK_PATH..."
            pkPath <- fromMaybe (error "PK_PATH not found") <$> lookupEnv "PK_PATH"
            [PrivKeyRSA pk] <- readKeyFile pkPath
            pure pk

    let envAppInfo = AppInfo
            { appInfoId = 20170
            , appInfoPrivateKey = pk
            }

    envTokenCache <- newIORef mempty
    let envLogAction = richMessageAction

    -- Http manager configuration
    envManager <- newTlsManager

    pure Env{..}

mkTestAppEnv :: IO AppEnv
mkTestAppEnv = mkAppEnv defaultCliArguments

-- | Runs the program with a given context
runOn :: CliArguments -> IO ()
runOn cli = do
    env@Env{..} <- mkAppEnv cli

    -- this function changes mutable variable 'envTokenCache'
    -- it's not dangerous but might be surprising
    runAppLogIO_ env initialiseInstallationIds

    run envPort (hintmanApp env)
