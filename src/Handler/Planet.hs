module Handler.Planet where

import Yesod
import Data.Aeson.Types
import Data.Aeson (FromJSON(..), toJSON, withObject, (.:))
import Data.Maybe
import Data.Text (Text(..))
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Maybe (runMaybeT)
import Database.Persist.Postgresql
import Network.HTTP.Types
import Test.QuickCheck

import qualified Database.Models as DB
import Foundation
import Template.PlanetList

$(mkPlanetWordList "planetWordList" "data/planets_wordlist.txt")

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

data PlanetPatch = PlanetPatch { patchPlanetName   :: Maybe Text
                               , patchPlanetMass   :: Maybe DB.Weight
                               , patchPlanetType   :: Maybe DB.PlanetType
                               , patchPlanetRadius :: Maybe DB.Distance
                               , patchPlanetDist   :: Maybe DB.Distance
                               }

instance FromJSON PlanetPatch where
    parseJSON = withObject "PlanetPatch" $ \obj -> do
        patchPlanetName   <- obj .:? "name"
        patchPlanetMass   <- obj .:? "mass"
        patchPlanetType   <- obj .:? "type"
        patchPlanetRadius <- obj .:? "radius"
        patchPlanetDist   <- obj .:? "dist"
        return PlanetPatch{..}


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

data PlanetDiscover = PlanetDiscover { planetDiscoverUserId :: DB.UserId }

instance FromJSON PlanetDiscover where
    parseJSON = withObject "PlanetDiscover" $ \obj -> do
        planetDiscoverUserId <- obj .: "userId"
        return PlanetDiscover{..}

postDiscoverPlanetR :: Handler Value
postDiscoverPlanetR = do
    PlanetDiscover {..} <- requireCheckJsonBody
    planetGen <- liftIO $ generate arbitrary
    let planet = genPlanet planetGen planetDiscoverUserId
    planetId <- runDB $ insert planet
    sendStatusJSON ok200 $ object [ "planet" .= toJSON planet
                                  , "id"     .= planetId
                                  ]

data PlanetSell = PlanetSell { sellPlanetPlanetId :: DB.PlanetId
                             , sellPlanetUserId :: DB.UserId
                             -- TODO: sellPlanetUserSignature :: String
                             }

instance FromJSON PlanetSell where
    parseJSON = withObject "PlanetSell" $ \obj -> do
        sellPlanetPlanetId <- obj .: "planetId"
        sellPlanetUserId   <- obj .: "userId"
        return PlanetSell{..}

postSellPlanetR :: Handler Value
postSellPlanetR = do
    PlanetSell{..} <- requireCheckJsonBody
    res <- runDB $ runMaybeT $ do
        Just planet <- lift $ get sellPlanetPlanetId
        Just user   <- lift $ get sellPlanetUserId
        lift $ do let balance = DB.userBalance user + DB.planetIco planet
                  update sellPlanetUserId [DB.UserBalance =. balance]
                  delete sellPlanetPlanetId

    case res of
      Nothing -> sendStatusJSON notFound404 $ object ["error" .= ("planet or user not found" :: Text)]
      Just _ -> sendStatusJSON ok200 emptyObject

instance Arbitrary DB.Weight where
    arbitrary = DB.Kg <$> choose (10 ^ 6, 10 ^ 18)

instance Arbitrary DB.Distance where
    arbitrary = DB.Meter <$> choose (10 ^ 9, 10 ^ 13)

instance Arbitrary DB.PlanetType where
    arbitrary = oneof $ pure <$> [DB.Gaseous, DB.EarthLike, DB.Rocky]

newtype PlanetName = PlanetName { unPlanetName :: Text }

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

newtype PlanetICO = PlanetICO { unPlanetICO :: DB.Currency }

pascalTri :: Int -> [Int]
pascalTri 1 = [1]
pascalTri n = [1] ++ zipWith (+) upLevel (tail upLevel) ++ [1]
    where upLevel = pascalTri (n - 1)

instance Arbitrary PlanetICO where
    arbitrary = (PlanetICO . DB.Stellarium) <$> frequency frequencies
        where -- Basically a very bad way to get a normal distribution
              frequencies = zip (pascalTri levels) [choose (i, i + range) | i <- [minICO, minICO + range .. maxICO]]
              range = (maxICO - minICO + 1) / (realToFrac levels)
              levels = 20
              minICO = 1.0
              maxICO = 10.0 ^ 3

newtype GenPlanet = GenPlanet { genPlanet :: DB.UserId -> DB.Planet }

instance Arbitrary GenPlanet where
    arbitrary = do
        name        <- unPlanetName <$> arbitrary
        mass        <- arbitrary
        ty          <- arbitrary
        disttoearth <- arbitrary
        radius      <- arbitrary
        ico         <- unPlanetICO <$> arbitrary
        return $ GenPlanet $ \owner -> DB.Planet{ DB.planetName = name
                                                , DB.planetMass = mass
                                                , DB.planetType = ty
                                                , DB.planetDisttoearth = disttoearth
                                                , DB.planetRadius = radius
                                                , DB.planetIco = ico
                                                , DB.planetOwnerId = owner
                                                }

