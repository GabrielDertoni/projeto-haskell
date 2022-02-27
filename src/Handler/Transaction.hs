module Handler.Transaction where

import Yesod
import Data.Aeson.Types
import Data.Aeson (FromJSON(..), toJSON)
import Data.Maybe
import Data.Text
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (catMaybes)
import Database.Persist.Postgresql
import Network.HTTP.Types

import qualified Database.Models as DB
import Foundation

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

    {-
postDepositR :: Handler Value
postDepositR = do
    TransactionCreate{..} <- requireCheckJsonBody :: Handler TransactionCreate
    today <- liftIO getToday
    let transaction = DB.Transaction { DB.transactionBuyer  = createTransactionBuyer
                                     , DB.transactionSeller = Nothing
                                     , DB.transactionPlanet = Nothing
                                     , DB.transactionAmount = createTransactionAmount
                                     , DB.transactionDate   = today
                                     }
    transactionId <- runDB $ insert transaction

    runDB $ update userId [DB.UserBalance +=. createTransactionAmount]

    sendStatusJSON created201
        $ object ["TransactionId" .= fromSqlKey transactionId]


postWithdrawR :: Handler Value
postWithdrawR = do
    TransactionCreate{..} <- requireCheckJsonBody :: Handler TransactionCreate
    today <- liftIO getToday
    let transaction = DB.Transaction { DB.transactionBuyer  = Nothing
                                     , DB.transactionSeller = createTransactionSeller
                                     , DB.transactionPlanet = undefined
                                     , DB.transactionAmount = createTransactionAmount
                                     , DB.transactionDate   = today
                                     }
    transactionId <- runDB $ insert transaction

    runDB $ update userId [DB.UserBalance -=. createTransactionAmount]

    sendStatusJSON created201
        $ object ["TransactionId" .= fromSqlKey transactionId]
        -}

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
    transactionId <- runDB $ insert transaction

    runDB $ update createTransactionBuyer [DB.UserBalance -=. createTransactionAmount]
    runDB $ update createTransactionSeller [DB.UserBalance +=. createTransactionAmount]

    sendStatusJSON created201
        $ object ["TransactionId" .= fromSqlKey transactionId]

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
