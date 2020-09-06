{-# LANGUAGE PartialTypeSignatures #-}

module Parsing (
    parseStartYear
  ) where

import Hledger.Flow.DateTime
import System.Exit (die)
import Text.Read (readMaybe)

parseStartYear :: Maybe String -> IO (Maybe Integer)
parseStartYear y = case y of
  Nothing -> return Nothing
  Just "current"  -> Just <$> currentYear
  Just s -> Just <$> parseInt s "Unable to parse year"

parseInt :: String -> String -> IO Integer
parseInt s errPrefix = case safeParseInt s errPrefix of
  Right i -> return i
  Left err -> die err

safeParseInt :: String -> String -> Either String Integer
safeParseInt s errPrefix = case (readMaybe s :: Maybe Integer) of
  Nothing -> Left $ errPrefix ++ " " ++ s
  Just i  -> Right i
