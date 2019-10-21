{-# LANGUAGE ApplicativeDo #-}

module Hintman.Cli
       ( Cli (..)
       , cli
       ) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar,
                            option, progDesc, short, showDefault, switch, value)


-- | Represents the applicaiton settings
data Cli = Cli
    { cliLogging :: !Bool  -- ^ 'True' if status logging is enabled
    , cliPort    :: !(Maybe Int)  -- ^ Port to listen
    }

parseCli :: Parser Cli
parseCli = do
    cliLogging <- parseLogging
    cliPort    <- parsePort
    pure Cli{..}
  where
    parseLogging :: Parser Bool
    parseLogging = switch $
        long "logging"
        <> short 'l'
        <> help "Enable concurrent status logging"

    parsePort :: Parser (Maybe Int)
    parsePort = optional $ option auto $
        long "port"
        <> short 'p'
        <> metavar "PORT_NUMBER"
        <> value 8080
        <> showDefault
        <> help "Configure the port to run on"

{- | Parse out the arguments from command line arguments

Using this also enabled the program to print out a helpful
description of the valid command line arguments.
-}
cli :: IO Cli
cli = execParser opts
  where
    opts = info (parseCli <**> helper)
        (  fullDesc
        <> progDesc "Run the Hintman server"
        )
