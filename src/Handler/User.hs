module Handler.User where

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

data UserCreate = UserCreate { createUserName :: Text
                             , createUserNickname :: Text
                             , createUserEmail :: Text
                             , createUserBirthdate :: DB.Date
                             } deriving (Generic, FromJSON)

postUserR :: Handler Value
postUserR = do
    UserCreate{..} <- requireCheckJsonBody :: Handler UserCreate
    now <- liftIO getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    let user = DB.User { DB.userName      = createUserName
                       , DB.userNickname  = createUserNickname
                       , DB.userEmail     = createUserEmail
                       , DB.userBirthdate = createUserBirthdate
                       , DB.userDate      = DB.Date { DB.year  = fromIntegral year
                                                    , DB.month = fromIntegral month
                                                    , DB.day   = fromIntegral day
                                                    }
                       , DB.userBalance   = DB.Stellarium 0
                       }
    userId <- runDB $ insert user

    sendStatusJSON created201
        $ object ["userId" .= fromSqlKey userId]

getUserByIdR :: DB.UserId -> Handler Value
getUserByIdR userId = do
    user <- runDB $ get404 userId
    sendStatusJSON ok200 $ toJSON user

data UserPatch = UserPatch
    { patchUserName  :: Maybe Text
    , patchUserNickname :: Maybe Text
    , patchUserEmail :: Maybe Text
    } deriving (Generic, FromJSON)


patchUserByIdR :: DB.UserId -> Handler Value
patchUserByIdR userId = do
    UserPatch {..} <- requireCheckJsonBody :: Handler UserPatch

    let patchData = catMaybes [ (DB.UserName     =.) <$> patchUserName
                              , (DB.UserNickname =.) <$> patchUserNickname
                              , (DB.UserEmail    =.) <$> patchUserEmail
                              ]
    runDB $ update userId patchData

    sendStatusJSON noContent204 emptyObject

deleteUserByIdR :: DB.UserId -> Handler Value
deleteUserByIdR userId = do
    runDB $ delete userId
    sendStatusJSON noContent204 emptyObject
