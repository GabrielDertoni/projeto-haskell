module Lib ( run ) where

-- External imports
import Prelude hiding (readFile)
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative ((<|>))
import Test.QuickCheck (arbitrary)
import Data.Text (Text)
import Data.Semigroup ((<>))
import Data.Yaml (decodeFileThrow)
import Database.Persist.Postgresql (createPostgresqlPool, runSqlPersistMPool, runMigration)
import Database.PostgreSQL.Simple (ConnectInfo(..), postgreSQLConnectionString)
import Options.Applicative

-- Internal imports
import Database.Models
import Settings

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

name :: Text
name = "Jorge"

age :: Int
age = 47

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ return $ object
               [ "name" .= name
               , "age" .= age
               ]

connStr = "host=localhost dbname=stellarium user=postgres password=12345 port=5432"

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

run :: IO ()
run = do
    opts <- execParser optsInfo
    AppSettings{..} <- case opts of
      OptsFile    path     -> decodeFileThrow path
      CmdLineOpts settings -> return settings

    let connStr = getPostgresConnStr $ dbSettings
    let poolSize = fromIntegral $ dbPoolSize $ dbSettings
    connectionPool <- runStdoutLoggingT $
        createPostgresqlPool connStr poolSize

    runSqlPersistMPool (runMigration migrateAll) connectionPool

    putStrLn $ "Launching server on port " ++ show port
    warp port HelloWorld
