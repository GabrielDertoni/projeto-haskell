module Database.Models where

import Yesod
import Control.Monad (guard)
import Database.Persist.Quasi
import Database.Persist.TH
import Data.Scientific (isInteger)
import Data.Text
import Data.Aeson (FromJSON(..), Value(..), withObject, withText, withScientific, (.:))

data Weight = Kg Float
    deriving (Show, Eq, Ord, Read)

derivePersistField "Weight"

instance FromJSON Weight where
    parseJSON = withScientific "Weight" $ return . Kg . realToFrac

instance ToJSON Weight where
    toJSON (Kg num) = Number $ realToFrac num

data Distance = Meter Float
    deriving (Show, Eq, Ord, Read)

derivePersistField "Distance"

instance FromJSON Distance where
    parseJSON = withScientific "Distance" $ return . Meter . realToFrac

instance ToJSON Distance where
    toJSON (Meter num) = Number $ realToFrac num

-- Stellarium: the currency that is going to reach the stars!
data Currency = Stellarium Float
    deriving (Show, Eq, Ord, Read)

derivePersistField "Currency"

instance FromJSON Currency where
    parseJSON = withScientific "Currency" $ return . Stellarium . realToFrac

instance ToJSON Currency where
    toJSON (Stellarium amnt) = Number $ realToFrac amnt

data Date = Date { year :: Int
                 , month :: Int
                 , day :: Int
                 }
                 deriving (Show, Eq, Read)

instance Ord Date where
    a <= b
      | year  a <= year  b = True
      | month a <= month b = True
      | day   a <= day   b = True
      | otherwise          = False

derivePersistField "Date"

instance FromJSON Date where
    parseJSON = withObject "Date" $ \obj -> do
        year  <- obj .: "year"
        month <- obj .: "month"
        day   <- obj .: "day"
        return Date{..}

instance ToJSON Date where
    toJSON Date{..} = object [ "year" .= year
                             , "month" .= month
                             , "day" .= day
                             ]

data PlanetType = Gaseous | EarthLike | Rocky
                 deriving (Show, Eq, Read)

derivePersistField "PlanetType"

instance FromJSON PlanetType where
    parseJSON = withText "PlanetType" $ \text ->
        case text of
          "gaseous"    -> return Gaseous
          "earth-like" -> return EarthLike
          "rocky"      -> return Rocky
          _            -> fail "PlanetType must be either 'gaseous', 'earth-like' or 'rocky'"

instance ToJSON PlanetType where
    toJSON Gaseous   = String "gaseous"
    toJSON EarthLike = String "earth-like"
    toJSON Rocky     = String "rocky"

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
      $(persistFileWith lowerCaseSettings "config/definitions/models")
