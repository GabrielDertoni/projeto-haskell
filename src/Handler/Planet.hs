module Handler.Planet where

import Yesod
import Data.Aeson.Types
import Data.Aeson (toJSON)
import Data.Maybe
import Data.Text (Text(..))
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Maybe (catMaybes)
import Database.Persist.Postgresql
import GHC.Generics
import Network.HTTP.Types
import Test.QuickCheck

import qualified Database.Models as DB
import Foundation
import Template.PlanetList

postPlanetR :: Handler Value
postPlanetR = do
    planet <- requireCheckJsonBody :: Handler DB.Planet
    planetId <- runDB $ insert planet

    sendStatusJSON created201
        $ object ["planetId" .= fromSqlKey planetId]

getPlanetByIdR :: DB.PlanetId -> Handler Value
getPlanetByIdR planetId = do
    planet <- runDB $ get404 planetId
    sendStatusJSON ok200 $ toJSON planet

data PlanetPatch = PlanetPatch
    { patchPlanetName   :: Maybe Text
    , patchPlanetMass   :: Maybe DB.Weight
    , patchPlanetType   :: Maybe DB.PlanetType
    , patchPlanetDist   :: Maybe DB.Distance
    , patchPlanetRadius :: Maybe DB.Distance
    } deriving (Generic, FromJSON)

patchPlanetByIdR :: DB.PlanetId -> Handler Value
patchPlanetByIdR planetId = do
    PlanetPatch {..} <- requireCheckJsonBody :: Handler PlanetPatch

    let patchData = catMaybes [ (DB.PlanetName        =.) <$> patchPlanetName
                              , (DB.PlanetMass        =.) <$> patchPlanetMass
                              , (DB.PlanetType        =.) <$> patchPlanetType
                              , (DB.PlanetDisttoearth =.) <$> patchPlanetDist
                              , (DB.PlanetRadius      =.) <$> patchPlanetRadius
                              ]
    runDB $ update planetId patchData

    sendStatusJSON noContent204 emptyObject

deletePlanetByIdR :: DB.PlanetId -> Handler Value
deletePlanetByIdR planetId = do
    runDB $ delete planetId
    sendStatusJSON noContent204 emptyObject

getDiscoverPlanet :: Handler Value
getDiscoverPlanet = do
    planet <- liftIO $ (generate arbitrary :: IO DB.Planet)
    sendStatusJSON ok200 $ toJSON planet

instance Arbitrary DB.Weight where
    arbitrary = DB.Kg <$> choose (10 ^ 6, 10 ^ 18)

instance Arbitrary DB.Distance where
    arbitrary = DB.Meter <$> choose (10 ^ 9, 10 ^ 13)

instance Arbitrary DB.PlanetType where
    arbitrary = oneof $ pure <$> [DB.Gaseous, DB.EarthLike, DB.Rocky]

newtype PlanetName = PlanetName { unPlanetName :: Text }

$(mkPlanetWordList "planetWordList" "data/planets_wordlist.txt")

toRoman :: Int -> Text
toRoman 0 = ""
toRoman n = let (letter, val) = head $ filter ((n >=) . snd) numerals
             in toRoman (n - val) <> letter
    where numerals = [("M", 1000), ("CM",  900), ("D",  500), ("CD",  400),
                      ("C",  100), ("XC",   90), ("L",   50), ("XL",   40),
                      ("X",   10), ("IX",    9), ("V",    5), ("IV",    4),
                      ("I",    1)]

instance Arbitrary PlanetName where
    arbitrary = do
        name <- oneof $ pure <$> planetWordList
        size <- getSize
        num <- choose (0, size)
        case num of
          0 -> return $ PlanetName name
          n -> return $ PlanetName $ name <> " " <> toRoman n

instance Arbitrary DB.Planet where
    arbitrary = do
        name <- unPlanetName <$> arbitrary
        mass <- arbitrary
        ty <- arbitrary
        disttoearth <- arbitrary
        radius <- arbitrary
        return DB.Planet{ DB.planetName = name
                        , DB.planetMass = mass
                        , DB.planetType = ty
                        , DB.planetDisttoearth = disttoearth
                        , DB.planetRadius = radius
                        }
