module Hintman.Cli
    ( CliArguments(..)
    , defaultCliArguments
    , cliArguments
    ) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar,
                            option, progDesc, short, showDefault, switch, value)


-- | Represents the applicaiton settings
data CliArguments = CliArguments
    { cliArgumentsLogging :: Bool
    , cliArgumentsPort    :: Maybe Int
    }

defaultCliArguments :: CliArguments
defaultCliArguments = CliArguments
    { cliArgumentsLogging = False
    , cliArgumentsPort = Nothing
    }

parseCliArguments :: Parser CliArguments
parseCliArguments = CliArguments
    <$> parseLogging
    <*> parsePort
  where
    parseLogging = switch
        (  long "logging"
        <> short 'l'
        <> help "Enable logging"
        )
    parsePort = optional $ option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORT_NUMBER"
        <> value 8080
        <> showDefault
        <> help "Configure the port to run on"
        )

{- | Parse out the arguments from command line arguments

Using this also enabled the program to print out a helpful
description of the valid command line arguments.
-}
cliArguments :: IO CliArguments
cliArguments = execParser opts
  where
    opts = info (parseCliArguments <**> helper)
        (  fullDesc
        <> progDesc "Run the Hintman server"
        )
