module Handler.Transaction where

import Yesod
import Data.Aeson.Types
import Data.Aeson (toJSON)
import Data.Maybe
import Data.Text
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Maybe (catMaybes)
import Database.Persist.Postgresql
import GHC.Generics
import Network.HTTP.Types

import qualified Database.Models as DB
import Foundation

data TransactionCreate = TransactionCreate { createTransactionBuyer  :: DB.UserId
                                           , createTransactionSeller :: DB.UserId
                                           , createTransactionPlanet :: DB.PlanetId
                                           , createTransactionAmount :: DB.Currency
                                           } deriving (Generic, FromJSON)


postDepositR :: Handler Value
postDepositR = do
    TransactionCreate{..} <- requireCheckJsonBody :: Handler TransactionCreate
    now <- liftIO getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    let userId = createTransactionBuyer
    let transaction = DB.Transaction { DB.transactionBuyer  = userId
                                     , DB.transactionSeller = undefined
                                     , DB.transactionPlanet = undefined
                                     , DB.transactionAmount = createTransactionAmount
                                     , DB.transactionDate   = DB.Date { DB.year  = fromIntegral year
                                                                      , DB.month = fromIntegral month
                                                                      , DB.day   = fromIntegral day
                                                                      }
                                     }
    transactionId <- runDB $ insert transaction

    runDB $ update userId [DB.UserBalance +=. createTransactionAmount]

    sendStatusJSON created201
        $ object ["TransactionId" .= fromSqlKey transactionId]


postWithdrawR :: Handler Value
postWithdrawR = do
    TransactionCreate{..} <- requireCheckJsonBody :: Handler TransactionCreate
    now <- liftIO getCurrentTime
    let userId = createTransactionSeller
    let (year, month, day) = toGregorian $ utctDay now
    let transaction = DB.Transaction { DB.transactionBuyer  = undefined
                                     , DB.transactionSeller = userId
                                     , DB.transactionPlanet = undefined
                                     , DB.transactionAmount = createTransactionAmount
                                     , DB.transactionDate   = DB.Date { DB.year  = fromIntegral year
                                                                      , DB.month = fromIntegral month
                                                                      , DB.day   = fromIntegral day
                                                                      }
                                     }
    transactionId <- runDB $ insert transaction

    runDB $ update userId [DB.UserBalance -=. createTransactionAmount]

    sendStatusJSON created201
        $ object ["TransactionId" .= fromSqlKey transactionId]

postTransferR :: Handler Value
postTransferR = do
    TransactionCreate{..} <- requireCheckJsonBody :: Handler TransactionCreate
    now <- liftIO getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    let buyerId = createTransactionBuyer
    let sellerId = createTransactionSeller
    let amount = createTransactionAmount
    let transaction = DB.Transaction { DB.transactionBuyer  = buyerId
                                     , DB.transactionSeller = sellerId
                                     , DB.transactionPlanet = createTransactionPlanet
                                     , DB.transactionAmount = amount
                                     , DB.transactionDate   = DB.Date { DB.year  = fromIntegral year
                                                                      , DB.month = fromIntegral month
                                                                      , DB.day   = fromIntegral day
                                                                      }
                                     }
    transactionId <- runDB $ insert transaction

    runDB $ update buyerId [DB.UserBalance -=. amount]
    runDB $ update sellerId [DB.UserBalance +=. amount]

    sendStatusJSON created201
        $ object ["TransactionId" .= fromSqlKey transactionId]

getTransactionByIdR :: DB.TransactionId -> Handler Value
getTransactionByIdR transactionId = do
    transaction <- runDB $ get404 transactionId
    sendStatusJSON ok200 $ toJSON transaction

data TransactionPatch = TransactionPatch
    { patchTransactionBuyer  :: Maybe DB.UserId
    , patchTransactionSeller :: Maybe DB.UserId
    , patchTransactionPlanet :: Maybe DB.PlanetId
    , patchTransactionAmount :: Maybe DB.Currency
    } deriving (Generic, FromJSON)


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
