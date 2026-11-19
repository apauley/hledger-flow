{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Flow.ReportReview
  ( Review (..),
    ReviewCategory (..),
    ReviewItem (..),
    ReviewFile,
    ReviewKind (..),
    captureReportInputs,
    countTransferAccounts,
    countUnknownTransactions,
    invalidateReportReview,
    loadReportReview,
    reportInputsCurrent,
    reviewFile,
    reviewHasData,
    saveReportReview,
    startReportRun,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict', encodeFile)
import Data.Char (isDigit, isSpace)
import Data.List (nub, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Hledger.Flow.InputSnapshot (InputSnapshot, inputsUnchanged, snapshotInputs)
import Hledger.Flow.LedgerSelection (journalInputs)
import Hledger.Flow.PathHelpers (AbsFile, RelFile, withStagedOutput, writeTimestampMarker)
import Hledger.Flow.Types (BaseDir, HledgerConf, HledgerInfo, hledgerConfPath)
import Path
import qualified Path.IO as PathIO
import qualified System.Directory as D
import qualified System.FilePath as F
import System.IO.Error (catchIOError, isDoesNotExistError)
import Text.Read (readMaybe)

data ReviewKind = UnknownTransactions | TransferBalances
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data ReviewFile = ReviewFile ReviewKind RelFile Integer
  deriving (Generic, FromJSON, ToJSON)

data ReviewItem = ReviewItem
  { itemKind :: ReviewKind,
    itemPath :: FilePath,
    itemYear :: Integer,
    itemCount :: Maybe Int
  }
  deriving (Show)

data ReviewCategory = ReviewCategory {reviewCount :: Maybe Int, reviewFiles :: [FilePath]}
  deriving (Show)

data Review = Review
  { reviewUnknowns :: ReviewCategory,
    reviewTransfers :: ReviewCategory,
    reviewItems :: [ReviewItem]
  }
  deriving (Show)

data ReportInputs = ReportInputs
  { runStarted :: UTCTime,
    runAggregate :: AbsFile,
    runConfig :: Maybe AbsFile,
    runInputs :: InputSnapshot
  }
  deriving (Generic, FromJSON, ToJSON)

data Manifest = Manifest ReportInputs [ReviewFile]
  deriving (Generic, FromJSON, ToJSON)

manifestFile :: BaseDir -> AbsFile
manifestFile base = base </> [relfile|.hledger-flow/reports.json|]

invalidateReportReview :: BaseDir -> IO ()
invalidateReportReview base =
  PathIO.removeFile (manifestFile base) `catchIOError` \err ->
    if isDoesNotExistError err then return () else ioError err

startReportRun :: BaseDir -> IO UTCTime
startReportRun base = do
  invalidateReportReview base
  writeTimestampMarker (base </> [relfile|.hledger-flow/report-started|])

configPath :: Maybe HledgerConf -> IO (Maybe AbsFile)
configPath = traverse ((>>= parseAbsFile) . D.canonicalizePath . hledgerConfPath)

captureReportInputs :: HledgerInfo -> UTCTime -> AbsFile -> Maybe HledgerConf -> [[AbsFile]] -> IO (Either T.Text ReportInputs)
captureReportInputs info started aggregate conf roots = do
  discovered <- traverse (journalInputs info) (nub roots)
  case sequence discovered of
    Left err -> return (Left err)
    Right paths -> do
      result <- try $ do
        config <- configPath conf
        snapshot <- snapshotInputs (maybe id (:) config (concat paths))
        return (ReportInputs started aggregate config snapshot)
      return (either (Left . T.pack . show) Right (result :: Either IOException ReportInputs))

reportInputsCurrent :: ReportInputs -> IO Bool
reportInputsCurrent inputs = inputsUnchanged (runInputs inputs) (runStarted inputs)

reviewFile :: BaseDir -> AbsFile -> Maybe ReviewFile
reviewFile base file = do
  kind <- case toFilePath (filename file) of
    "unknown-transactions.txt" -> Just UnknownTransactions
    "transfer-balance.txt" -> Just TransferBalances
    _ -> Nothing
  rel <- stripProperPrefix base file
  year <- readMaybe (F.dropTrailingPathSeparator (toFilePath (dirname (parent file))))
  return (ReviewFile kind rel year)

saveReportReview :: BaseDir -> ReportInputs -> [ReviewFile] -> IO ()
saveReportReview base inputs files =
  withStagedOutput (manifestFile base) (\stage -> encodeFile (toFilePath stage) (Manifest inputs files))

loadReportReview :: BaseDir -> AbsFile -> Maybe HledgerConf -> IO (Maybe Review)
loadReportReview base aggregate conf = do
  result <- try $ do
    saved <- decodeFileStrict' (toFilePath (manifestFile base))
    case saved of
      Nothing -> return Nothing
      Just (Manifest inputs files) -> do
        currentConfig <- configPath conf
        if runConfig inputs /= currentConfig || runAggregate inputs /= aggregate
          then return Nothing
          else do
            current <- reportInputsCurrent inputs
            if current then Just . summarize <$> traverse readReview files else return Nothing
  return (either (const Nothing :: IOException -> Maybe Review) id result)
  where
    readReview (ReviewFile kind rel year) = do
      text <- T.readFile (toFilePath (base </> rel))
      let body = T.unlines (drop 2 (T.lines text))
          count = if T.null (T.strip body) then Nothing else countFor kind body
      return (ReviewItem kind (toFilePath rel) year count)
    summarize items =
      let ordered = sortOn (\item -> (Down (itemYear item), itemPath item)) items
       in Review (category UnknownTransactions ordered) (category TransferBalances ordered) ordered
    category kind items =
      let relevant = filter ((== kind) . itemKind) items
       in ReviewCategory (sum <$> traverse itemCount relevant) (map itemPath relevant)

countFor :: ReviewKind -> T.Text -> Maybe Int
countFor UnknownTransactions = countUnknownTransactions
countFor TransferBalances = countTransferAccounts

reviewHasData :: ReviewKind -> T.Text -> Bool
reviewHasData kind text = case filter (not . T.null) (map T.strip (T.lines text)) of
  [] -> False
  [line] | csvHeader kind line -> False
  _ -> countFor kind text /= Just 0

csvHeader :: ReviewKind -> T.Text -> Bool
csvHeader UnknownTransactions = T.isPrefixOf "\"txnidx\",\"date\","
csvHeader TransferBalances = T.isPrefixOf "\"account\","

countUnknownTransactions :: T.Text -> Maybe Int
countUnknownTransactions text
  | null rows = Just 0
  | not (null headers) && all known rows = Just (length headers)
  | otherwise = Nothing
  where
    rows = filter (not . T.null . T.strip) (T.lines text)
    headers = filter dateHeader rows
    known row = dateHeader row || maybe False (isSpace . fst) (T.uncons row) || ";" `T.isPrefixOf` row
    dateHeader row = case T.unpack (T.take 10 row) of
      [a,b,c,d,'-',e,f,'-',g,h] ->
        all isDigit [a,b,c,d,e,f,g,h]
          && maybe True (\(end, _) -> isSpace end || end == '=') (T.uncons (T.drop 10 row))
      _ -> False

countTransferAccounts :: T.Text -> Maybe Int
countTransferAccounts text = case filter (not . T.null) (map T.strip (T.lines text)) of
  [] -> Just 0
  title : table | "Balance changes in " `T.isPrefixOf` title ->
    case break divider table of
      (heading, _ : body)
        | any (\line -> "||" `T.isInfixOf` line || "║" `T.isInfixOf` line) heading ->
            let rows = filter (not . T.isPrefixOf "└") body
             in if all accountRow rows then Just (length rows) else Nothing
      _ -> Nothing
  _ -> Nothing
  where
    divider row = "╞" `T.isPrefixOf` row || ("=" `T.isPrefixOf` row && "++" `T.isInfixOf` row)
    accountRow row = case mapMaybe (\sep -> nonemptyField sep row) ["||", "║"] of
      [_] -> True
      _ -> False
    nonemptyField sep row =
      let (label, rest) = T.breakOn sep row
       in if T.null rest || T.null (T.strip (T.dropWhile (== '│') label)) then Nothing else Just label
