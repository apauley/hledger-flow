{-# LANGUAGE OverloadedStrings #-}

-- | Delimited statement records and the date evidence they contain.
module Hledger.Flow.StatementScan
  ( DelimitedStatement (..),
    parseDelimitedStatement,
    parseRuleSample,
    readRuleSample,
    readDelimitedStatement,
    splitHeader,
    looksLikeHeader,
    isDelimitedFile,
    DateCell,
    parseDateCell,
    dateCellYear,
    inferDateFormat,
    isSignedDecimal,
  )
where

import Data.Char (isDigit, isSpace, toLower)
import Data.List (intersect)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Time (fromGregorianValid)
import Hledger.Flow.PathHelpers (AbsFile)
import Path (toFilePath)
import qualified System.FilePath as F
import System.IO (IOMode (ReadMode), Handle, hIsEOF, hSetEncoding, mkTextEncoding, withFile)

data DelimitedStatement = DelimitedStatement
  { statementSeparator :: Char,
    statementHeader :: Maybe [T.Text],
    statementRows :: [[T.Text]]
  }
  deriving (Eq, Show)

isDelimitedFile :: FilePath -> Bool
isDelimitedFile path = map toLower (F.takeExtension path) `elem` [".csv", ".tsv", ".ssv"]

parseDelimitedStatement :: FilePath -> T.Text -> Either T.Text DelimitedStatement
parseDelimitedStatement = parseStatementSample (const Nothing)

parseRuleSample :: FilePath -> T.Text -> Either T.Text DelimitedStatement
parseRuleSample = parseStatementSample (Just . ruleRows)

parseStatementSample :: (DelimitedStatement -> Maybe Int) -> FilePath -> T.Text -> Either T.Text DelimitedStatement
parseStatementSample limit file contents =
  case logicalRecords (T.lines (T.dropWhile (== '\xFEFF') contents)) of
    [] -> Left noColumns
    first : rest -> do
      initial <- first >>= startStatement file
      rows <- traverse (>>= parseRow initial) (maybe rest (\n -> take (remaining n initial) rest) (limit initial))
      return initial {statementRows = statementRows initial ++ rows}

readDelimitedStatement :: Maybe Int -> AbsFile -> IO (Either T.Text DelimitedStatement)
readDelimitedStatement = readSample . const

readRuleSample :: AbsFile -> IO (Either T.Text DelimitedStatement)
readRuleSample = readSample (Just . ruleRows)

ruleRows :: DelimitedStatement -> Int
ruleRows initial = case statementHeader initial of
  Just _ -> 0
  Nothing -> 50

readSample :: (DelimitedStatement -> Maybe Int) -> AbsFile -> IO (Either T.Text DelimitedStatement)
readSample limit path = withFile (toFilePath path) ReadMode $ \handle -> do
  hSetEncoding handle =<< mkTextEncoding "UTF-8//ROUNDTRIP"
  first <- readRecord handle
  case first >>= traverse (startStatement (toFilePath path) . T.dropWhile (== '\xFEFF')) of
    Left err -> return (Left err)
    Right Nothing -> return (Left noColumns)
    Right (Just initial) -> do
      rows <- readRows handle initial (fmap (\n -> remaining n initial) (limit initial)) []
      return $ fmap (\xs -> initial {statementRows = statementRows initial ++ xs}) rows

remaining :: Int -> DelimitedStatement -> Int
remaining n initial = n - length (statementRows initial)

readRows :: Handle -> DelimitedStatement -> Maybe Int -> [[T.Text]] -> IO (Either T.Text [[T.Text]])
readRows _ _ (Just n) rows | n <= 0 = return (Right (reverse rows))
readRows handle initial limit rows = do
  next <- readRecord handle
  case next >>= traverse (parseRow initial) of
    Left err -> return (Left err)
    Right Nothing -> return (Right (reverse rows))
    Right (Just row) -> readRows handle initial (subtract 1 <$> limit) (row : rows)

readRecord :: Handle -> IO (Either T.Text (Maybe T.Text))
readRecord handle = do
  eof <- hIsEOF handle
  if eof then return (Right Nothing) else do
    line <- stripCR <$> T.hGetLine handle
    if T.null (T.strip line) then readRecord handle else finish (openQuote False line) [line]
  where
    finish False chunks = return (Right (Just (T.intercalate "\n" (reverse chunks))))
    finish True chunks = do
      eof <- hIsEOF handle
      if eof then return (Left "Unclosed quoted field.") else do
        line <- stripCR <$> T.hGetLine handle
        finish (openQuote True line) (line : chunks)

logicalRecords :: [T.Text] -> [Either T.Text T.Text]
logicalRecords = next . map stripCR
  where
    next [] = []
    next (line : rest)
      | T.null (T.strip line) = next rest
      | otherwise = collect (openQuote False line) [line] rest
    collect False chunks rest = Right (T.intercalate "\n" (reverse chunks)) : next rest
    collect True _ [] = [Left "Unclosed quoted field."]
    collect True chunks (line : rest) = collect (openQuote True line) (line : chunks) rest

stripCR :: T.Text -> T.Text
stripCR = T.dropWhileEnd (== '\r')

openQuote :: Bool -> T.Text -> Bool
openQuote = T.foldl' (\inside c -> if c == '"' then not inside else inside)

noColumns :: T.Text
noColumns = "Could not determine the statement's columns."

startStatement :: FilePath -> T.Text -> Either T.Text DelimitedStatement
startStatement file record = do
  let sep = chooseSeparator (map toLower (F.takeExtension file)) record
  fields <- splitFields sep record
  case fields of
    _ : _ : _ | looksLikeHeader fields -> return (DelimitedStatement sep (Just fields) [])
              | otherwise -> return (DelimitedStatement sep Nothing [fields])
    _ -> Left noColumns

parseRow :: DelimitedStatement -> T.Text -> Either T.Text [T.Text]
parseRow initial record = do
  row <- splitFields (statementSeparator initial) record
  let width = case (statementHeader initial, statementRows initial) of
        (Just header, _) -> length header
        (_, first : _) -> length first
        _ -> 0
  if length row == width then Right row else Left "Statement rows have different numbers of columns."

splitHeader :: T.Text -> (Char, [T.Text])
splitHeader line =
  let record = stripCR (T.dropWhile (== '\xFEFF') line)
      sep = chooseSeparator ".csv" record
   in (sep, either (const []) id (splitFields sep record))

chooseSeparator :: String -> T.Text -> Char
chooseSeparator ".tsv" _ = '\t'
chooseSeparator ".ssv" _ = ';'
chooseSeparator _ record = foldl' pick ',' [';', '\t']
  where
    pick best other | count other > count best = other
                    | otherwise = best
    count sep = snd (T.foldl' (step sep) (False, 0 :: Int) record)
    step _ (quoted, n) '"' = (not quoted, n)
    step sep (quoted, n) c = (quoted, if not quoted && c == sep then n + 1 else n)

splitFields :: Char -> T.Text -> Either T.Text [T.Text]
splitFields sep = field [] . T.unpack
  where
    field fields rest
      | '"' : contents <- dropWhile (\c -> isSpace c && c /= sep) rest = quoted fields [] contents
      | otherwise =
          let (value, tail') = break (== sep) rest
           in if '"' `elem` value then Left "Unexpected quote in a delimited field."
              else end fields (T.strip (T.pack value)) tail'
    quoted _ _ [] = Left "Unclosed quoted field."
    quoted fields value ('"' : '"' : rest) = quoted fields ('"' : value) rest
    quoted fields value ('"' : rest) =
      end fields (T.strip (T.pack (reverse value))) (dropWhile (\c -> isSpace c && c /= sep) rest)
    quoted fields value (c : rest) = quoted fields (c : value) rest
    end fields value [] = Right (reverse (value : fields))
    end fields value (c : rest)
      | c == sep = field (value : fields) rest
      | otherwise = Left "Unexpected text after a quoted field."

looksLikeHeader :: [T.Text] -> Bool
looksLikeHeader = not . any dataish
  where
    dataish f = isJust (parseDateCell f) ||
      (not (T.null f) && T.all (`elem` ("0123456789-/.:,+$€£ " :: String)) f)

isSignedDecimal :: T.Text -> Bool
isSignedDecimal raw = case T.splitOn "." (dropSign raw) of
  [whole] -> digits whole
  [whole, fraction] -> digits whole && digits fraction
  _ -> False
  where
    dropSign t = case T.uncons t of
      Just (sign, rest) | sign `elem` ['+', '-'] -> rest
      _ -> t

digits :: T.Text -> Bool
digits t = not (T.null t) && T.all (\c -> c >= '0' && c <= '9') t

decimal :: T.Text -> Maybe Int
decimal t
  | digits t, Right (n, "") <- T.decimal t = Just n
  | otherwise = Nothing

data DateCell = DateCell {dateCellYear :: Integer, dateFormats :: [T.Text]}
  deriving (Eq, Show)

parseDateCell :: T.Text -> Maybe DateCell
parseDateCell value = do
  let (date, suffix) = T.break (\c -> isSpace c || c == 'T') (T.strip value)
  (sep, _) <- T.uncons (T.dropWhile isDigit date)
  if sep `elem` ['-', '/', '.'] then Just () else Nothing
  case T.splitOn (T.singleton sep) date of
    [a,b,c]
      | T.length a == 4 && short b && short c -> do
          year <- parseYear a
          m <- decimal b
          d <- decimal c
          -- Year-first inputs use conventional ISO order wherever it fits.
          let formats = if valid year m d then [fmt sep ["%Y","%m","%d"]]
                        else [fmt sep ["%Y","%d","%m"] | valid year d m]
          cell year formats suffix
      | short a && short b && T.length c == 4 -> do
          year <- parseYear c
          lo <- decimal a
          hi <- decimal b
          cell year ([fmt sep ["%m","%d","%Y"] | valid year lo hi] ++
                     [fmt sep ["%d","%m","%Y"] | valid year hi lo]) suffix
    _ -> Nothing
  where
    short t = T.length t <= 2 && digits t
    valid y m d = isJust (fromGregorianValid y m d)
    parseYear t = do
      n <- decimal t
      if n >= 1900 && n <= 2099 then Just (toInteger n) else Nothing
    fmt sep = T.intercalate (T.singleton sep)
    cell _ [] _ = Nothing
    cell year formats suffix = case timeSuffix suffix of
      Nothing -> Nothing
      Just layout -> Just (DateCell year (maybe [] (\timeFormat -> map (<> timeFormat) formats) layout))

-- | An unfamiliar but time-like suffix supplies a year without a format guess.
timeSuffix :: T.Text -> Maybe (Maybe T.Text)
timeSuffix "" = Just (Just "")
timeSuffix suffix = do
  let (joiner, time) = case T.stripPrefix "T" suffix of
        Just t -> ("T", t)
        Nothing -> (" ", T.stripStart suffix)
  (hour, tail') <- T.uncons time
  if isDigit hour && ":" `T.isInfixOf` tail' then Just () else Nothing
  return $ case T.splitOn ":" time of
    [h,m] | within 23 h && within 59 m -> Just (joiner <> "%H:%M")
    [h,m,s] | within 23 h && within 59 m && within 59 s -> Just (joiner <> "%H:%M:%S")
    _ -> Nothing
  where
    within bound t = T.length t == 2 && maybe False (<= bound) (decimal t)

inferDateFormat :: [DateCell] -> Maybe T.Text
inferDateFormat cells = case map dateFormats cells of
  first : rest | [one] <- foldl' intersect first rest -> Just one
  _ -> Nothing
