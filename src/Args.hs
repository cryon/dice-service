module Args (Arguments
            , argPort
            , argLogLocation
            , opts
             , execParser
            ) where

import Options.Applicative

defaultPort :: Int
defaultPort = 80

data Arguments = Arguments { argPort        :: Int
                           , argLogLocation :: Maybe String
                           }

cmdParser :: Parser Arguments
cmdParser = Arguments
            <$> option (long "port"
                        <> short 'p'
                        <> metavar "PORT"
                        <> help "Port that service listens to (defaults to 80)"
                        <> value defaultPort)

            <*> optional (option (long "log-location"
                               <> short 'l'
                               <> metavar "PATH"
                               <> help "Path to logfile (if none is given, \
                                        \defaults to STDOUT)"))

opts:: ParserInfo Arguments
opts = info (helper <*> cmdParser)
       (fullDesc
        <> header "Starts a service on that listens for incoming HTTP \
                   \GET requests, parses the argument and returns the \
                   \result in either plain text, json or html"
        <> progDesc "Dice as a service (DaaS)")
