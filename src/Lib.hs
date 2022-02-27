module Lib ( run ) where

-- External imports
import Yesod
import Control.Monad.Logger        (runStdoutLoggingT)
import Data.Text                   (Text)
import Data.Semigroup              ((<>))
import Data.Yaml                   (decodeFileThrow)
import Database.Persist.Postgresql (createPostgresqlPool, runSqlPersistMPool, runMigration)
import Database.PostgreSQL.Simple  (ConnectInfo(..), postgreSQLConnectionString)

-- Internal imports
import Database.Models
import Settings
import CliOpts
import Foundation

-- Handlers
import Handler.User
import Handler.Planet
import Handler.Transaction

mkYesodDispatch "StellariumApp" resourcesStellariumApp

run :: IO ()
run = do
    opts <- execOptCliParser
    AppSettings{..} <- case opts of
      OptsFile    path     -> decodeFileThrow path
      CmdLineOpts settings -> return settings

    let connStr = getPostgresConnStr $ dbSettings
    let poolSize = fromIntegral $ dbPoolSize $ dbSettings
    connectionPool <- runStdoutLoggingT $
        createPostgresqlPool connStr poolSize

    runSqlPersistMPool (runMigration migrateAll) connectionPool

    putStrLn $ "Launching server on port " ++ show port
    warp port StellariumApp{..}
