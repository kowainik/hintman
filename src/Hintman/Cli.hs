module Hintman.Cli
    ( Context(..)
    , cliContext
    ) where

import Options.Applicative (Parser, execParser, fullDesc, help,
                            helper, info, long, progDesc, short, switch)


-- | Represents the applicaiton settings
newtype Context = Context
    { contextLogging :: Bool
    }
    
parseContext :: Parser Context
parseContext = Context
    <$> parseLogging
  where
    parseLogging = switch 
        (  long "logging"
        <> short 'l'
        <> help "Enable logging"
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
