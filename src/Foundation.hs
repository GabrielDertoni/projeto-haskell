module Foundation where

import Yesod
import Database.Persist.Sql

import Database.Models

data StellariumApp = StellariumApp { connectionPool  :: ConnectionPool }



mkYesodData "StellariumApp" [parseRoutes|
 /user           UserR         POST
 /user/#UserId   UserByIdR GET            DELETE
|]

instance Yesod StellariumApp

instance YesodPersist StellariumApp where
   type YesodPersistBackend StellariumApp = SqlBackend

   runDB action = do
       master <- getYesod
       runSqlPool action $ connectionPool master
