module CliOpts (execOptCliParser, CliOpts(..)) where

import Options.Applicative
import Control.Applicative ((<|>))

import Settings

data CliOpts = OptsFile FilePath | CmdLineOpts AppSettings

parseOptsFile :: Parser CliOpts
parseOptsFile = OptsFile
            <$> strOption
                ( long "settings"
               <> metavar "SETTINGS"
               <> help "Path to the database and server settings" )

parseCmdLineOpts :: Parser CliOpts
parseCmdLineOpts = CmdLineOpts <$> appSettingsParser
    where appSettingsParser = AppSettings
                          <$> option auto
                              ( long "port"
                             <> metavar "PORT"
                             <> help "The server that the server should listen to" )
                          <*> parseCmdLineDBSettings

parseCmdLineDBSettings :: Parser DBSettings
parseCmdLineDBSettings = DBSettings
                     <$> strOption
                         ( long "db-host"
                        <> metavar "DBHOST"
                        <> value "localhost"
                        <> help "Postgres database host name" )
                     <*> option auto
                         ( long "db-port"
                        <> metavar "DBPORT"
                        <> help "Postgres database port" )
                     <*> strOption
                         ( long "db-user"
                        <> metavar "DBUSER"
                        <> value "postgres"
                        <> help "Postgres database user" )
                     <*> strOption
                         ( long "db-password"
                        <> metavar "DBPASSWORD"
                        <> help "Postgres database password" )
                     <*> strOption
                         ( long "db-name"
                        <> metavar "DBNAME"
                        <> help "Postgres database name" )
                     <*> option auto
                         ( long "db-pool-size"
                        <> metavar "DBPOOLSIZE"
                        <> help "Postgres database pool size" )

parseCliOpts :: Parser CliOpts
parseCliOpts = parseOptsFile <|> parseCmdLineOpts

optsInfo :: ParserInfo CliOpts
optsInfo = info (parseCliOpts <**> helper)
    ( fullDesc
   <> progDesc "Run the Stellarium server"
   <> header "stellarium - planet trading at it's peak" )

execOptCliParser :: IO CliOpts
execOptCliParser = execParser optsInfo
