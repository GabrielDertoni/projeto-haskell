module Lib ( run ) where

import Control.Monad.Logger
import Test.QuickCheck (arbitrary)
import Yesod
import Data.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple (ConnectInfo(..), postgreSQLConnectionString)

import Database.Models

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

run :: IO ()
run = do
    let poolSize = 10
    let config = ConnectInfo { connectHost = "localhost"
                             , connectPort = 5000
                             , connectUser = "postgres"
                             , connectPassword = "1234"
                             , connectDatabase = "db"
                             }

    let connStr = postgreSQLConnectionString config
    putStrLn $ "connStr:{" ++ (BS.w2c <$> (BS.unpack $ connStr)) ++ "}"
    connectionPool <- runStdoutLoggingT $
        createPostgresqlPool (postgreSQLConnectionString config) poolSize

    runSqlPersistMPool (runMigration migrateAll) connectionPool

    warp 3000 HelloWorld
