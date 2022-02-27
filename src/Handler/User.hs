module Handler.User where

import Yesod
import Data.Aeson.Types
import Data.Aeson (toJSON)
import Data.Maybe
import Data.Text
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Database.Persist.Postgresql
import GHC.Generics
import Network.HTTP.Types

import qualified Database.Models as DB
import Foundation

data UserCreate = UserCreate { name :: Text
                             , username :: Text
                             , birthdate :: DB.Date
                             } deriving (Generic, FromJSON)

postUserR :: Handler Value
postUserR = do
    userCrate <- requireCheckJsonBody :: Handler UserCreate
    now <- liftIO getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    let user = DB.User { DB.userName = name userCrate
                       , DB.userUsername = username userCrate
                       , DB.userBirthdate = birthdate userCrate
                       , DB.userDate = DB.Date { DB.year = fromIntegral year
                                               , DB.month = fromIntegral month
                                               , DB.day = fromIntegral day
                                               }
                       , DB.userBalance = DB.Stellarium 0
                       }
    userId <- runDB $ insert user

    sendStatusJSON created201
        $ object ["userId" .= fromSqlKey userId]

getUserByIdR :: DB.UserId -> Handler Value
getUserByIdR userId = do
    user <- runDB $ get404 userId
    sendStatusJSON ok200 $ toJSON user

{-
data UserPatch = UserPatch
    { userMaybeName  :: Maybe Text
    , userMaybeEmail :: Maybe Text
    } deriving (Generic, FromJSON)


patchUserByIdR :: UserId -> Handler Value
patchUserByIdR userId = do
    UserPatch {..} <- requireCheckJsonBody :: Handler UserPatch

    patchData <- return [
            column =. fromJust maybeValue
            | (column, maybeValue) <- [
                (UserName, userMaybeName),
                (UserEmail, userMaybeEmail)
            ]
            , isJust maybeValue
        ]

    runDB $ update userId patchData

    sendStatusJSON noContent204 emptyObject
-}

deleteUserByIdR :: DB.UserId -> Handler Value
deleteUserByIdR userId = do
    runDB $ delete userId
    sendStatusJSON noContent204 emptyObject
