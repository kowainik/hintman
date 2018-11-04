module Hintman.Cli
    ( Context(..)
    , cliContext
    ) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help,
                            helper, info, long, metavar, option, progDesc,
                            short, showDefault, switch, value)


-- | Represents the applicaiton settings
data Context = Context
    { contextLogging :: Bool
    , contextPort :: Int
    }
    
parseContext :: Parser Context
parseContext = Context
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
    
{- | Parse out the context from command line arguments

Using this also enabled the program to print out a helpful
description of the valid command line arguments.
-}
cliContext :: IO Context
cliContext = execParser opts
  where
    opts = info (parseContext <**> helper)
        (  fullDesc
        <> progDesc "Run the Hintman server"
        )
