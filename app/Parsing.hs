module Parsing (StartYear, startYearReader, resolveStartYear) where

import Data.Char (isDigit)
import Hledger.Flow.DateTime (currentYear)
import Options.Applicative (ReadM, eitherReader)
import Text.Read (readMaybe)

data StartYear = CurrentYear | FromYear Integer deriving (Show)

startYearReader :: ReadM StartYear
startYearReader = eitherReader parse
  where
    parse "current" = Right CurrentYear
    parse s
      | length s == 4, all isDigit s, Just year <- readMaybe s = Right (FromYear year)
      | otherwise = Left "expected a four-digit year or 'current'"

resolveStartYear :: StartYear -> IO Integer
resolveStartYear CurrentYear = currentYear
resolveStartYear (FromYear year) = return year
