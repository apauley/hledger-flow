module Hledger.Flow.DateTime where

import Data.Time.Clock
import Data.Time.Calendar

currentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
currentDate = toGregorian . utctDay <$> getCurrentTime

currentYear :: IO Integer
currentYear = do
  (y, _, _) <- currentDate
  return y
