module Handler.User where

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

data UserCreate = UserCreate { createUserName :: Text
                             , createUserNickname :: Text
                             , createUserEmail :: Text
                             , createUserBirthdate :: DB.Date
                             }

instance FromJSON UserCreate where
    parseJSON = withObject "UserCreate" $ \obj -> do
        createUserName      <- obj .: "name"
        createUserNickname  <- obj .: "nickname"
        createUserEmail     <- obj .: "email"
        createUserBirthdate <- obj .: "birthdate"
        return UserCreate{..}


postUserR :: Handler Value
postUserR = do
    UserCreate{..} <- requireCheckJsonBody
    now <- liftIO $ getCurrentTime
    let user = DB.User { DB.userName      = createUserName
                       , DB.userNickname  = createUserNickname
                       , DB.userEmail     = createUserEmail
                       , DB.userBirthdate = createUserBirthdate
                       , DB.userCreated   = now
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
    }

instance FromJSON UserPatch where
    parseJSON = withObject "UserPatch" $ \obj -> do
        patchUserName      <- obj .:? "name"
        patchUserNickname  <- obj .:? "nickname"
        patchUserEmail     <- obj .:? "email"
        return UserPatch{..}


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

getUserPlanetsR :: DB.UserId -> Handler Value
getUserPlanetsR userId = do
    planets <- runDB $ selectList [DB.PlanetOwnerId ==. Just userId] []
    sendStatusJSON ok200 $ object ["planets" .= toJSON planets]

getUserListR :: Handler Value
getUserListR = do
    users <- runDB $ selectList [] [Desc DB.UserBalance]
    sendStatusJSON ok200 $ object ["users" .= toJSON users]
