module Settings where

import Data.Word                (Word16)
import Numeric.Natural          (Natural)
import Data.Aeson               (FromJSON(..), (.:), withObject)
import Data.List                (intersperse)
import Data.ByteString.Internal (ByteString, packChars)
import Control.Arrow            ((>>>))
import Control.Monad            (ap, join)

data DBSettings = DBSettings { dbHost :: String
                             , dbPort :: Word16
                             , dbUser :: String
                             , dbPassword :: String
                             , dbName :: String
                             , dbPoolSize :: Natural
                             }

data AppSettings = AppSettings { port :: Int
                               , dbSettings :: DBSettings
                               }

instance FromJSON DBSettings where
    parseJSON = withObject "DBSettings" $ \obj -> do
        dbHost     <- obj .: "host"
        dbPort     <- obj .: "port"
        dbUser     <- obj .: "user"
        dbPassword <- obj .: "password"
        dbName     <- obj .: "name"
        dbPoolSize <- obj .: "pool-size"
        return DBSettings {..}

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \obj -> do
        port        <- obj .: "port"
        dbSettings  <- obj .: "database"
        return AppSettings {..}

getPostgresConnStr :: DBSettings -> ByteString
getPostgresConnStr = flip ($)
                 >>> flip fmap [ ("host="     ++) . dbHost
                               , ("port="     ++) . show . dbPort
                               , ("user="     ++) . dbUser
                               , ("password=" ++) . dbPassword
                               , ("dbname="   ++) . dbName
                               ]
                 >>> intersperse " "
                 >>> join
                 >>> packChars
