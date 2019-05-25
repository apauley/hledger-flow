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
  let baseJournal = journalFile opts []
  let baseReportDir = outputReportDir opts ["all"]
  baseYears <- includeYears ch baseJournal
  let baseParams = if length owners > 1 then [(ReportParams baseJournal baseYears baseReportDir)] else []
  ownerParams <- ownerParameters opts ch owners
  let reportParams = baseParams ++ ownerParams
  let actions = List.concat $ fmap (generateReports'' opts ch) reportParams
  parAwareActions opts actions

generateReports'' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ReportParams -> [IO (Either FilePath FilePath)]
generateReports'' opts ch (ReportParams journal years reportsDir) = do
  y <- years
  let sharedOptions = ["--depth", "2", "--pretty-tables", "not:equity"]
  let actions = map (\r -> r opts ch journal reportsDir y) [accountList, incomeStatement sharedOptions, balanceSheet sharedOptions]
  map (fmap fst) actions

accountList :: RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> FilePath -> Integer -> IO (Either FilePath FilePath, FlowTypes.FullTimedOutput)
accountList opts ch journal reportsDir year = do
  let reportArgs = ["accounts"]
  generateReport opts ch journal reportsDir year ("accounts" <.> "txt") reportArgs

incomeStatement :: [Text] -> RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> FilePath -> Integer -> IO (Either FilePath FilePath, FlowTypes.FullTimedOutput)
incomeStatement sharedOptions opts ch journal reportsDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions ++ ["--cost", "--value"]
  generateReport opts ch journal reportsDir year ("income-expenses" <.> "txt") reportArgs

balanceSheet :: [Text] -> RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> FilePath -> Integer -> IO (Either FilePath FilePath, FlowTypes.FullTimedOutput)
balanceSheet sharedOptions opts ch journal reportsDir year = do
  let reportArgs = ["balancesheet"] ++ sharedOptions ++ ["--cost", "--flat"]
  generateReport opts ch journal reportsDir year ("balance-sheet" <.> "txt") reportArgs

generateReport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> FilePath -> Integer -> FilePath -> [Text] -> IO (Either FilePath FilePath, FlowTypes.FullTimedOutput)
generateReport opts ch journal baseOutDir year fileName args = do
  let reportsDir = baseOutDir </> intPath year
  mktree reportsDir
  let outputFile = reportsDir </> fileName
  let relativeJournal = relativeToBase opts journal
  let reportArgs = ["--file", format fp journal, "--period", repr year] ++ args
  let reportDisplayArgs = ["--file", format fp relativeJournal, "--period", repr year] ++ args
  let hledger = format fp $ FlowTypes.hlPath . hledgerInfo $ opts :: Text
  let cmdLabel = format ("hledger "%s) $ showCmdArgs reportDisplayArgs
  result@((exitCode, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr procStrictWithErr (hledger, reportArgs, empty)
  if not (T.null stdOut)
    then
    do
      writeTextFile outputFile (cmdLabel <> "\n\n"<> stdOut)
      logVerbose opts ch $ format ("Wrote "%fp) $ relativeToBase opts outputFile
      return (Right outputFile, result)
    else
    do
      channelErrLn ch $ format ("No report output for '"%s%"' "%s) cmdLabel (repr exitCode)
      return (Left outputFile, result)

journalFile :: RuntimeOptions -> [FilePath] -> FilePath
journalFile opts dirs = (foldl (</>) (baseDir opts) dirs) </> "all-years" <.> "journal"

outputReportDir :: RuntimeOptions -> [FilePath] -> FilePath
outputReportDir opts dirs = foldl (</>) (baseDir opts) ("reports":dirs)

ownerParameters :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [FilePath] -> IO [ReportParams]
ownerParameters opts ch owners = do
  let actions = map (ownerParameters' opts ch) owners
  parAwareActions opts actions

ownerParameters' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> FilePath -> IO ReportParams
ownerParameters' opts ch owner = do
  let ownerJournal = journalFile opts ["import", owner]
  years <- includeYears ch ownerJournal
  return $ ReportParams ownerJournal years (outputReportDir opts [owner])
