module Hledger.Flow.DateTime where

import Data.Time.Calendar
import Data.Time.Clock
import GHC.Clock (getMonotonicTimeNSec)

currentDate :: IO (Integer, Int, Int) -- :: (year,month,day)
currentDate = toGregorian . utctDay <$> getCurrentTime

currentYear :: IO Integer
currentYear = do
  (y, _, _) <- currentDate
  return y

timeAction :: IO a -> IO (a, NominalDiffTime)
timeAction action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  return (result, fromIntegral (end - start) / 1000000000)
