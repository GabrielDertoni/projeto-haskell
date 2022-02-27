module Handler.Transaction where

import Yesod
import Data.Aeson.Types
import Data.Aeson (FromJSON(..), toJSON)
import Data.Maybe
import Data.Text
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (catMaybes)
import Database.Persist.Postgresql
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (guard)
import Network.HTTP.Types

import qualified Database.Models as DB
import Foundation

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
    now <- liftIO $ getCurrentTime
    res <- runDB $ runMaybeT $ do
        Just planet <- lift $ get sellPlanetPlanetId
        -- Make sure that the user owns the planet
        guard $ DB.planetOwnerId planet == Just sellPlanetUserId
        Just user   <- lift $ get sellPlanetUserId
        lift $ do let balance = DB.userBalance user + DB.planetIco planet
                  update sellPlanetUserId [DB.UserBalance =. balance]
                  update sellPlanetPlanetId [DB.PlanetOwnerId =. Nothing]
                  -- Register the transaction
                  insert DB.Transaction { DB.transactionBuyer = Nothing
                                        , DB.transactionSeller = sellPlanetUserId
                                        , DB.transactionAmount = DB.planetIco planet
                                        , DB.transactionCreated = now
                                        , DB.transactionPlanet = sellPlanetPlanetId
                                        }

    case res of
      Nothing             -> sendStatusJSON notFound404
                                $ object ["error" .= ("not allowed" :: Text)]
      Just transactionId  -> sendStatusJSON ok200
                                $ object ["transactionId" .= transactionId]

data TransactionCreate = TransactionCreate { createTransactionBuyer  :: DB.UserId
                                           , createTransactionSeller :: DB.UserId
                                           , createTransactionPlanet :: DB.PlanetId
                                           , createTransactionAmount :: DB.Currency
                                           }

instance FromJSON TransactionCreate where
    parseJSON = withObject "TransactionCreate" $ \obj -> do
        createTransactionBuyer  <- obj .: "buyer"
        createTransactionSeller <- obj .: "seller"
        createTransactionPlanet <- obj .: "planet"
        createTransactionAmount <- obj .: "amount"
        return TransactionCreate{..}

postTransferR :: Handler Value
postTransferR = do
    TransactionCreate{..} <- requireCheckJsonBody :: Handler TransactionCreate
    now <- liftIO getCurrentTime
    let transaction = DB.Transaction { DB.transactionBuyer   = Just createTransactionBuyer
                                     , DB.transactionSeller  = createTransactionSeller
                                     , DB.transactionPlanet  = createTransactionPlanet
                                     , DB.transactionAmount  = createTransactionAmount
                                     , DB.transactionCreated = now
                                     }
    maybeTransactionId <- runDB $ runMaybeT $ do
        Just buyer  <- lift $ get createTransactionBuyer
        Just seller <- lift $ get createTransactionSeller
        -- We won't use this here. But we still want to make sure that the
        -- planet exists!
        Just _      <- lift $ get createTransactionPlanet
        -- Make sure that the buyer has enough money
        guard $ DB.userBalance buyer >= createTransactionAmount
        let buyerBalance = DB.userBalance buyer - createTransactionAmount
        let sellerBalance = DB.userBalance seller + createTransactionAmount
        lift $ do
            update createTransactionBuyer [DB.UserBalance =. buyerBalance]
            update createTransactionSeller [DB.UserBalance =. sellerBalance]
            update createTransactionPlanet [DB.PlanetOwnerId =. Just createTransactionBuyer]
            insert transaction

    case maybeTransactionId of
      Nothing            -> sendStatusJSON notAcceptable406
                                $ object ["error" .= ("not permited" :: Text)]
      Just transactionId -> sendStatusJSON created201
                                $ object ["transactionId" .= fromSqlKey transactionId]

getTransactionByIdR :: DB.TransactionId -> Handler Value
getTransactionByIdR transactionId = do
    transaction <- runDB $ get404 transactionId
    sendStatusJSON ok200 $ toJSON transaction

data TransactionPatch = TransactionPatch
    { patchTransactionBuyer  :: Maybe (Maybe DB.UserId)
    , patchTransactionSeller :: Maybe DB.UserId
    , patchTransactionPlanet :: Maybe DB.PlanetId
    , patchTransactionAmount :: Maybe DB.Currency
    }

instance FromJSON TransactionPatch where
    parseJSON = withObject "TransactionPatch" $ \obj -> do
        patchTransactionBuyer  <- obj .:? "buyer"
        patchTransactionSeller <- obj .:? "seller"
        patchTransactionPlanet <- obj .:? "planet"
        patchTransactionAmount <- obj .:? "amount"
        return TransactionPatch{..}


patchTransactionByIdR :: DB.TransactionId -> Handler Value
patchTransactionByIdR transactionId = do
    TransactionPatch {..} <- requireCheckJsonBody :: Handler TransactionPatch

    let patchData = catMaybes [ (DB.TransactionBuyer  =.) <$> patchTransactionBuyer
                              , (DB.TransactionSeller =.) <$> patchTransactionSeller
                              , (DB.TransactionPlanet =.) <$> patchTransactionPlanet
                              , (DB.TransactionAmount =.) <$> patchTransactionAmount
                              ]
    runDB $ update transactionId patchData

    sendStatusJSON noContent204 emptyObject

deleteTransactionByIdR :: DB.TransactionId -> Handler Value
deleteTransactionByIdR transactionId = do
    runDB $ delete transactionId
    sendStatusJSON noContent204 emptyObject
