{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Reports
    ( generateReports
    ) where

import Turtle hiding (stdout, stderr, proc)
import Prelude hiding (FilePath, putStrLn, writeFile)
import Hledger.Flow.RuntimeOptions
import Hledger.Flow.Common
import Control.Concurrent.STM
import Data.Either

import qualified Data.Text as T
import qualified Hledger.Flow.Types as FlowTypes
import qualified Data.List as List

data ReportParams = ReportParams { ledgerFile :: FilePath
                                 , reportYears :: [Integer]
                                 , outputDir :: FilePath
                                 }
                  deriving (Show)
type ReportGenerator = RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> FilePath -> Integer -> IO (Either FilePath FilePath)

generateReports :: RuntimeOptions -> IO ()
generateReports opts = sh (
  do
    ch <- liftIO newTChanIO
    logHandle <- fork $ consoleChannelLoop ch
    liftIO $ if (showOptions opts) then channelOutLn ch (repr opts) else return ()
    (reports, diff) <- time $ liftIO $ generateReports' opts ch
    let failedAttempts = lefts reports
    let failedText = if List.null failedAttempts then "" else format ("(and attempted to write "%d%" more) ") $ length failedAttempts
    liftIO $ channelOutLn ch $ format ("Generated "%d%" reports "%s%"in "%s) (length (rights reports)) failedText $ repr diff
    liftIO $ terminateChannelLoop ch
    wait logHandle
  )

generateReports' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [Either FilePath FilePath]
generateReports' opts ch = do
  let wipMsg = "Report generation is still a work-in-progress - please let me know how this can be more useful.\n\n"
               <> "Keep an eye out for report-related pull requests and issues, and feel free to submit some of your own:\n"
               <> "https://github.com/apauley/hledger-flow/pulls\n"
               <> "https://github.com/apauley/hledger-flow/issues\n"
  channelOutLn ch wipMsg
  owners <- single $ shellToList $ listOwners opts
  let aggregateJournal = journalFile opts []
  let aggregateReportDir = outputReportDir opts ["all"]
  aggregateYears <- includeYears ch aggregateJournal
  let aggregateParams = ReportParams aggregateJournal aggregateYears aggregateReportDir
  let aggregateOnlyReports = reportActions opts ch [transferBalance] aggregateParams
  ownerParams <- ownerParameters opts ch owners
  let ownerWithAggregateParams = (if length owners > 1 then [aggregateParams] else []) ++ ownerParams
  let sharedOptions = ["--pretty-tables", "--depth", "2"]
  let ownerWithAggregateReports = List.concat $ fmap (reportActions opts ch [incomeStatement sharedOptions, balanceSheet sharedOptions]) ownerWithAggregateParams
  let ownerOnlyReports = List.concat $ fmap (reportActions opts ch [accountList, unknownTransactions]) ownerParams
  parAwareActions opts (aggregateOnlyReports ++ ownerWithAggregateReports ++ ownerOnlyReports)

reportActions :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [ReportGenerator] -> ReportParams -> [IO (Either FilePath FilePath)]
reportActions opts ch reports (ReportParams journal years reportsDir) = do
  y <- years
  map (\r -> r opts ch journal reportsDir y) reports

accountList :: ReportGenerator
accountList opts ch journal reportsDir year = do
  let reportArgs = ["accounts"]
  generateReport opts ch journal reportsDir year ("accounts" <.> "txt") reportArgs (not . T.null)

unknownTransactions :: ReportGenerator
unknownTransactions opts ch journal reportsDir year = do
  let reportArgs = ["print", "unknown"]
  generateReport opts ch journal reportsDir year ("unknown-transactions" <.> "txt") reportArgs (not . T.null)

incomeStatement :: [Text] -> ReportGenerator
incomeStatement sharedOptions opts ch journal reportsDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions
  generateReport opts ch journal reportsDir year ("income-expenses" <.> "txt") reportArgs (not . T.null)

balanceSheet :: [Text] -> ReportGenerator
balanceSheet sharedOptions opts ch journal reportsDir year = do
  let reportArgs = ["balancesheet"] ++ sharedOptions ++ ["--flat"]
  generateReport opts ch journal reportsDir year ("balance-sheet" <.> "txt") reportArgs (not . T.null)

transferBalance :: ReportGenerator
transferBalance opts ch journal reportsDir year = do
  let reportArgs = ["balance", "--pretty-tables", "--quarterly", "--flat", "--no-total", "transfer"]
  generateReport opts ch journal reportsDir year ("transfer-balance" <.> "txt") reportArgs (\txt -> (length $ T.lines txt) > 4)

generateReport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> FilePath -> Integer -> FilePath -> [Text] -> (Text -> Bool) -> IO (Either FilePath FilePath)
generateReport opts ch journal baseOutDir year fileName args successCheck = do
  let reportsDir = baseOutDir </> intPath year
  mktree reportsDir
  let outputFile = reportsDir </> fileName
  let relativeJournal = relativeToBase opts journal
  let relativeOutputFile = relativeToBase opts outputFile
  let reportArgs = ["--file", format fp journal, "--period", repr year] ++ args
  let reportDisplayArgs = ["--file", format fp relativeJournal, "--period", repr year] ++ args
  let hledger = format fp $ FlowTypes.hlPath . hledgerInfo $ opts :: Text
  let cmdLabel = format ("hledger "%s) $ showCmdArgs reportDisplayArgs
  ((exitCode, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr procStrictWithErr (hledger, reportArgs, empty)
  if (successCheck stdOut)
    then
    do
      writeTextFile outputFile (cmdLabel <> "\n\n"<> stdOut)
      logVerbose opts ch $ format ("Wrote "%fp) $ relativeOutputFile
      return $ Right outputFile
    else
    do
      channelErrLn ch $ format ("Did not write '"%fp%"' ("%s%") "%s) relativeOutputFile cmdLabel (repr exitCode)
      exists <- testfile outputFile
      if exists then rm outputFile else return ()
      return $ Left outputFile

journalFile :: RuntimeOptions -> [FilePath] -> FilePath
journalFile opts dirs = (foldl (</>) (baseDir opts) ("import":dirs)) </> allYearsFileName

outputReportDir :: RuntimeOptions -> [FilePath] -> FilePath
outputReportDir opts dirs = foldl (</>) (baseDir opts) ("reports":dirs)

ownerParameters :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [FilePath] -> IO [ReportParams]
ownerParameters opts ch owners = do
  let actions = map (ownerParameters' opts ch) owners
  parAwareActions opts actions

ownerParameters' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> IO ReportParams
ownerParameters' opts ch owner = do
  let ownerJournal = journalFile opts [owner]
  years <- includeYears ch ownerJournal
  return $ ReportParams ownerJournal years (outputReportDir opts [owner])
