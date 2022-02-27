module Handler.User where

import Yesod
import Data.Aeson.Types
import Data.Aeson (toJSON)
import Data.Maybe
import Data.Text
import Database.Persist.Postgresql
import GHC.Generics
import Network.HTTP.Types

import Database.Models
import Foundation

postUserR :: Handler Value
postUserR = do
    user <- requireCheckJsonBody :: Handler User
    userId <- runDB $ insert user

    sendStatusJSON created201
        $ object ["result" .= fromSqlKey userId]


getUserByIdR :: UserId -> Handler Value
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

deleteUserByIdR :: UserId -> Handler Value
deleteUserByIdR userId = do
    runDB $ delete userId
    sendStatusJSON noContent204 emptyObject
