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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (relativeToBase)
import Hledger.Flow.Common
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile)
import Hledger.Flow.RuntimeOptions
import Hledger.Flow.Status (plural)
import qualified Hledger.Flow.Types as FlowTypes
import Path ((</>), parent, reldir, relfile, parseRelDir, toFilePath)
import qualified Path.IO as PathIO
import qualified System.Directory as Directory
import Prelude hiding (putStrLn, readFile, writeFile)

data ReportParams = ReportParams
  { ledgerFiles :: [FilePath],
    reportYears :: [Integer],
    outputDir :: AbsDir
  }
  deriving (Show)

type ReportGenerator = RuntimeOptions -> TChan FlowTypes.LogMessage -> [FilePath] -> AbsDir -> Integer -> IO (Either AbsFile AbsFile)

generateReports :: RuntimeOptions -> IO ()
generateReports opts = withConsoleLogging $ \ch -> do
  when (showOptions opts) $ channelOutLn ch (T.pack (show opts))
  (reports, diff) <- timeAction $ generateReports' opts ch
  let skipped = length (lefts reports)
      skippedText = if skipped == 0 then "" else " Skipped " <> plural skipped "empty report" <> "."
  channelOutLn ch $ "Generated " <> plural (length (rights reports)) "report" <> " in reports/." <> skippedText
  channelOutLn ch "Each report starts with the hledger command that made it. Copy that line to tailor the report."
  logVerbose opts ch ("Reports took " <> T.pack (show diff))

generateReports' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [Either AbsFile AbsFile]
generateReports' opts ch = do
  owners <- listOwners opts
  configured <- configuredLedgerFile
  let hledgerJournal = maybe (toFilePath (baseDir opts </> [relfile|all-years.journal|])) snd configured
  hledgerJournalExists <- Directory.doesFileExist hledgerJournal
  unless hledgerJournalExists $ ioError $ userError $ case configured of
    Just (v, _) -> "LEDGER_FILE is set to " ++ v ++ ", which doesn't exist. Point it at "
      ++ toFilePath (baseDir opts </> [relfile|all-years.journal|]) ++ ", or unset it."
    Nothing -> "No all-years.journal in " ++ toFilePath (baseDir opts) ++ " yet. Run `hledger-flow import` first."
  let journalWithYears = baseDir opts </> [relfile|import/all-years.journal|]
  let aggregateReportDir
        | any ((== toFilePath [reldir|all|]) . map toLower . toFilePath) owners = baseDir opts </> [reldir|reports/_all_|]
        | otherwise = baseDir opts </> [reldir|reports/all|]
  aggregateYears <- includeYears ch journalWithYears
  let aggregateParams =
        ReportParams
          { ledgerFiles = [hledgerJournal],
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
reportActions opts ch reports (ReportParams journals years reportsDir) = do
  y <- years
  map (\r -> r opts ch journals reportsDir y) reports

accountList :: ReportGenerator
accountList opts ch journals baseOutDir year = do
  let reportArgs = ["accounts"]
  generateReport opts ch journals year baseOutDir [relfile|accounts.txt|] reportArgs (not . T.null)

unknownTransactions :: ReportGenerator
unknownTransactions opts ch journals baseOutDir year = do
  let reportArgs = ["print", "unknown"]
  generateReport opts ch journals year baseOutDir [relfile|unknown-transactions.txt|] reportArgs (not . T.null)

incomeStatement :: [T.Text] -> ReportGenerator
incomeStatement sharedOptions opts ch journals baseOutDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions
  generateReport opts ch journals year baseOutDir [relfile|income-expenses.txt|] reportArgs (not . T.null)

incomeMonthlyStatement :: [T.Text] -> ReportGenerator
incomeMonthlyStatement sharedOptions opts ch journals baseOutDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions ++ ["--monthly"]
  generateReport opts ch journals year baseOutDir [relfile|monthly/income-expenses.txt|] reportArgs (not . T.null)

balanceSheet :: [T.Text] -> ReportGenerator
balanceSheet sharedOptions opts ch journals baseOutDir year = do
  let reportArgs = ["balancesheet"] ++ sharedOptions ++ ["--flat"]
  generateReport opts ch journals year baseOutDir [relfile|balance-sheet.txt|] reportArgs (not . T.null)

transferBalance :: ReportGenerator
transferBalance opts ch journals baseOutDir year = do
  let reportArgs = ["balance"] ++ (if prettyReports opts then ["--pretty-tables"] else []) ++ ["--quarterly", "--flat", "--no-total", "transfer"]
  generateReport opts ch journals year baseOutDir [relfile|transfer-balance.txt|] reportArgs (\txt -> (length $ T.lines txt) > 4)

generateReport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [FilePath] -> Integer -> AbsDir -> RelFile -> [T.Text] -> (T.Text -> Bool) -> IO (Either AbsFile AbsFile)
generateReport opts ch journals year reportsDir fileName args successCheck = do
  yearDir <- parseRelDir (show year)
  let outputFile = reportsDir </> yearDir </> fileName
  PathIO.createDirIfMissing True (parent outputFile)
  let relativeJournals = map (relativeToBase opts) journals
  let relativeOutputFile = relativeToBase opts (toFilePath outputFile)
  let confArgs = hledgerConfArgs opts
  let fileArgs = concatMap (\file -> ["--file", T.pack file])
  let periodArgs = ["--period", T.pack (show year)]
  let reportArgs = confArgs ++ fileArgs journals ++ periodArgs ++ args
  let reportDisplayArgs = confArgs ++ fileArgs relativeJournals ++ periodArgs ++ args
  let hledger = T.pack (toFilePath (FlowTypes.hlPath (hledgerInfo opts)))
  let cmdLabel = "hledger " <> showCmdArgs reportDisplayArgs
  ((_, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr (hledger, reportArgs, Nothing)
  if (successCheck stdOut)
    then do
      T.writeFile (toFilePath outputFile) (cmdLabel <> "\n\n" <> stdOut)
      logVerbose opts ch $ "Wrote " <> T.pack relativeOutputFile
      return $ Right outputFile
    else do
      logVerbose opts ch $ "Skipped empty report " <> T.pack relativeOutputFile
      exists <- PathIO.doesFileExist outputFile
      when exists (PathIO.removeFile outputFile)
      return $ Left outputFile

ownerParameters :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [RelDir] -> IO [ReportParams]
ownerParameters opts ch owners = do
  let actions = map (ownerParameters' opts ch) owners
  parAwareActions opts actions

ownerParameters' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> RelDir -> IO ReportParams
ownerParameters' opts ch owner = do
  let ownerJournal = baseDir opts </> [reldir|import|] </> owner </> [relfile|all-years.journal|]
      reportsDir = baseDir opts </> [reldir|reports|] </> owner
  years <- includeYears ch ownerJournal
  let directives = directivesFile opts
  directivesExist <- PathIO.doesFileExist directives
  let journals = [toFilePath directives | directivesExist] ++ [toFilePath ownerJournal]
  return $ ReportParams journals years reportsDir
