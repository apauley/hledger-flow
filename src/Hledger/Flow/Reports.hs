{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Reports
  ( generateReports,
  )
where

import Control.Concurrent.STM
import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.Either
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (relativeToBase)
import Hledger.Flow.Common
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelFile)
import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as FlowTypes
import Path ((</>), parent, reldir, relfile, parseRelDir, toFilePath)
import qualified Path.IO as PathIO
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import qualified Turtle (single)
import Prelude hiding (putStrLn, readFile, writeFile)

data ReportParams = ReportParams
  { ledgerFile :: FilePath,
    reportYears :: [Integer],
    outputDir :: AbsDir
  }
  deriving (Show)

type ReportGenerator = RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> AbsDir -> Integer -> IO (Either AbsFile AbsFile)

generateReports :: RuntimeOptions -> IO ()
generateReports opts = withConsoleLogging $ \ch -> do
  when (showOptions opts) $ channelOutLn ch (T.pack (show opts))
  (reports, diff) <- timeAction $ generateReports' opts ch
  let failedAttempts = lefts reports
  let failedText = if List.null failedAttempts then "" else "(and attempted to write " <> T.pack (show (length failedAttempts)) <> " more) "
  channelOutLn ch $ "Generated " <> T.pack (show (length (rights reports))) <> " reports " <> failedText <> "in " <> T.pack (show diff)

generateReports' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [Either AbsFile AbsFile]
generateReports' opts ch = do
  let wipMsg =
        "These reports can be used as a starting point for more tailored reports.\n"
          <> "The first line of each report contains the command used - change the parameters and use it in your own reports.\n"
  channelOutLn ch wipMsg
  owners <- Turtle.single $ shellToList $ listOwners opts
  ledgerEnvValue <- lookupEnv "LEDGER_FILE"
  let hledgerJournal = fromMaybe (toFilePath (baseDir opts </> [relfile|all-years.journal|])) ledgerEnvValue
  hledgerJournalExists <- Directory.doesFileExist hledgerJournal
  unless hledgerJournalExists $ ioError $ userError $
    "Unable to find journal file: " ++ hledgerJournal ++ "\nIs your LEDGER_FILE environment variable set correctly?"
  let journalWithYears = baseDir opts </> [relfile|import/all-years.journal|]
  let aggregateReportDir
        | any ((== "all") . map toLower) owners = baseDir opts </> [reldir|reports/_all_|]
        | otherwise = baseDir opts </> [reldir|reports/all|]
  aggregateYears <- includeYears ch journalWithYears
  let aggregateParams =
        ReportParams
          { ledgerFile = hledgerJournal,
            reportYears = aggregateYears,
            outputDir = aggregateReportDir
          }
  let aggregateOnlyReports = reportActions opts ch [transferBalance] aggregateParams
  ownerParams <- ownerParameters opts ch owners
  let ownerWithAggregateParams = (if length owners > 1 then [aggregateParams] else []) ++ ownerParams
  let sharedOptions = (if prettyReports opts then ["--pretty-tables"] else []) ++ ["--depth", "2"]
  let ownerWithAggregateReports = List.concat $ fmap (reportActions opts ch [incomeStatement sharedOptions, incomeMonthlyStatement sharedOptions, balanceSheet sharedOptions]) ownerWithAggregateParams
  let ownerOnlyReports = List.concat $ fmap (reportActions opts ch [accountList, unknownTransactions]) ownerParams
  parAwareActions opts (aggregateOnlyReports ++ ownerWithAggregateReports ++ ownerOnlyReports)

reportActions :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [ReportGenerator] -> ReportParams -> [IO (Either AbsFile AbsFile)]
reportActions opts ch reports (ReportParams journal years reportsDir) = do
  y <- years
  map (\r -> r opts ch journal reportsDir y) reports

accountList :: ReportGenerator
accountList opts ch journal baseOutDir year = do
  let reportArgs = ["accounts"]
  generateReport opts ch journal year baseOutDir [relfile|accounts.txt|] reportArgs (not . T.null)

unknownTransactions :: ReportGenerator
unknownTransactions opts ch journal baseOutDir year = do
  let reportArgs = ["print", "unknown"]
  generateReport opts ch journal year baseOutDir [relfile|unknown-transactions.txt|] reportArgs (not . T.null)

incomeStatement :: [T.Text] -> ReportGenerator
incomeStatement sharedOptions opts ch journal baseOutDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions
  generateReport opts ch journal year baseOutDir [relfile|income-expenses.txt|] reportArgs (not . T.null)

incomeMonthlyStatement :: [T.Text] -> ReportGenerator
incomeMonthlyStatement sharedOptions opts ch journal baseOutDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions ++ ["--monthly"]
  generateReport opts ch journal year baseOutDir [relfile|monthly/income-expenses.txt|] reportArgs (not . T.null)

balanceSheet :: [T.Text] -> ReportGenerator
balanceSheet sharedOptions opts ch journal baseOutDir year = do
  let reportArgs = ["balancesheet"] ++ sharedOptions ++ ["--flat"]
  generateReport opts ch journal year baseOutDir [relfile|balance-sheet.txt|] reportArgs (not . T.null)

transferBalance :: ReportGenerator
transferBalance opts ch journal baseOutDir year = do
  let reportArgs = ["balance"] ++ (if prettyReports opts then ["--pretty-tables"] else []) ++ ["--quarterly", "--flat", "--no-total", "transfer"]
  generateReport opts ch journal year baseOutDir [relfile|transfer-balance.txt|] reportArgs (\txt -> (length $ T.lines txt) > 4)

generateReport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> Integer -> AbsDir -> RelFile -> [T.Text] -> (T.Text -> Bool) -> IO (Either AbsFile AbsFile)
generateReport opts ch journal year reportsDir fileName args successCheck = do
  yearDir <- parseRelDir (show year)
  let outputFile = reportsDir </> yearDir </> fileName
  PathIO.createDirIfMissing True (parent outputFile)
  let relativeJournal = relativeToBase opts journal
  let relativeOutputFile = relativeToBase opts (toFilePath outputFile)
  let confArgs = hledgerConfArgs opts
  let reportArgs = confArgs ++ ["--file", T.pack journal, "--period", T.pack (show year)] ++ args
  let reportDisplayArgs = confArgs ++ ["--file", T.pack relativeJournal, "--period", T.pack (show year)] ++ args
  let hledger = T.pack (toFilePath (FlowTypes.hlPath (hledgerInfo opts)))
  let cmdLabel = "hledger " <> showCmdArgs reportDisplayArgs
  ((exitCode, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr (hledger, reportArgs, Nothing)
  if (successCheck stdOut)
    then do
      T.writeFile (toFilePath outputFile) (cmdLabel <> "\n\n" <> stdOut)
      logVerbose opts ch $ "Wrote " <> T.pack relativeOutputFile
      return $ Right outputFile
    else do
      channelErrLn ch $ "Did not write '" <> T.pack relativeOutputFile <> "' (" <> cmdLabel <> ") " <> T.pack (show exitCode)
      exists <- PathIO.doesFileExist outputFile
      when exists (PathIO.removeFile outputFile)
      return $ Left outputFile

ownerParameters :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [FilePath] -> IO [ReportParams]
ownerParameters opts ch owners = do
  let actions = map (ownerParameters' opts ch) owners
  parAwareActions opts actions

ownerParameters' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> IO ReportParams
ownerParameters' opts ch name = do
  owner <- parseRelDir name
  let ownerJournal = baseDir opts </> [reldir|import|] </> owner </> [relfile|all-years.journal|]
      reportsDir = baseDir opts </> [reldir|reports|] </> owner
  years <- includeYears ch ownerJournal
  return $ ReportParams (toFilePath ownerJournal) years reportsDir
