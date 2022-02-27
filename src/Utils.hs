module Utils ( getToday ) where

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import qualified Database.Models as DB

getToday :: IO DB.Date
getToday = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    return DB.Date { DB.year  = fromIntegral year
                   , DB.month = fromIntegral month
                   , DB.day   = fromIntegral day
                   }
