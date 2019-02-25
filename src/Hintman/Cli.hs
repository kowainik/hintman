module Hintman.Cli
    ( CliArguments(..)
    , cliArguments
    ) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar,
                            option, progDesc, short, showDefault, switch, value)


-- | Represents the applicaiton settings
data CliArguments = CliArguments
    { cliArgumentsLogging :: Bool
    , cliArgumentsPort    :: Int
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
    parsePort = option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORTNUMBER"
        <> value 3000
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
