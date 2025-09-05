module Hledger.Flow.DateTime where

import Data.Time.Calendar
import Data.Time.Clock

currentDate :: IO (Integer, Int, Int) -- :: (year,month,day)
currentDate = toGregorian . utctDay <$> getCurrentTime

currentYear :: IO Integer
currentYear = do
  (y, _, _) <- currentDate
  return y
