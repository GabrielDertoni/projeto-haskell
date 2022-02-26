module Database.Models where

import Yesod
import Database.Persist.Quasi
import Database.Persist.TH
import Data.Text

data Weight = Kg Float
    deriving (Show, Eq, Ord, Read)
derivePersistField "Weight"

data Distance = Meter Float
    deriving (Show, Eq, Ord, Read)
derivePersistField "Distance"

-- Stellarium: the currency that is going to reach the stars!
data Currency = Stellarium Integer
    deriving (Show, Eq, Ord, Read)
derivePersistField "Currency"

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

data PlanetType = Gaseous | EarthLike | Rocky
                 deriving (Show, Eq, Read)
derivePersistField "PlanetType"

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
      $(persistFileWith lowerCaseSettings "config/definitions/models")
