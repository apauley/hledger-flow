{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Reports
    ( generateReports
    ) where

import qualified Turtle as Turtle hiding (stdout, stderr, proc)
import Turtle ((%), (</>), (<.>))
import Prelude hiding (putStrLn, writeFile)

import Hledger.Flow.RuntimeOptions
import Hledger.Flow.Common
import Hledger.Flow.BaseDir (turtleBaseDir, relativeToBase)

import Control.Concurrent.STM
import Data.Either
import Data.Maybe

import qualified Data.Text as T
import qualified Hledger.Flow.Types as FlowTypes
import Hledger.Flow.PathHelpers (TurtlePath)
import Hledger.Flow.Logging
import qualified Data.List as List

data ReportParams = ReportParams { ledgerFile :: TurtlePath
                                 , reportYears :: [Integer]
                                 , outputDir :: TurtlePath
                                 }
                  deriving (Show)
type ReportGenerator = RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> TurtlePath -> Integer -> IO (Either TurtlePath TurtlePath)

generateReports :: RuntimeOptions -> IO ()
generateReports opts = Turtle.sh (
  do
    ch <- Turtle.liftIO newTChanIO
    logHandle <- Turtle.fork $ consoleChannelLoop ch
    Turtle.liftIO $ if (showOptions opts) then channelOutLn ch (Turtle.repr opts) else return ()
    (reports, diff) <- Turtle.time $ Turtle.liftIO $ generateReports' opts ch
    let failedAttempts = lefts reports
    let failedText = if List.null failedAttempts then "" else Turtle.format ("(and attempted to write "%Turtle.d%" more) ") $ length failedAttempts
    Turtle.liftIO $ channelOutLn ch $ Turtle.format ("Generated "%Turtle.d%" reports "%Turtle.s%"in "%Turtle.s) (length (rights reports)) failedText $ Turtle.repr diff
    Turtle.liftIO $ terminateChannelLoop ch
    Turtle.wait logHandle
  )

generateReports' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [Either TurtlePath TurtlePath]
generateReports' opts ch = do
  let wipMsg = "Report generation is still a work-in-progress - please let me know how this can be more useful.\n\n"
               <> "Keep an eye out for report-related pull requests and issues, and feel free to submit some of your own:\n"
               <> "https://github.com/apauley/hledger-flow/pulls\n"
               <> "https://github.com/apauley/hledger-flow/issues\n"
  channelOutLn ch wipMsg
  owners <- Turtle.single $ shellToList $ listOwners opts
  ledgerEnvValue <- Turtle.need "LEDGER_FILE" :: IO (Maybe T.Text)
  let hledgerJournal = fromMaybe (turtleBaseDir opts </> allYearsFileName) $ fmap Turtle.fromText ledgerEnvValue
  hledgerJournalExists <- Turtle.testfile hledgerJournal
  _ <- if not hledgerJournalExists then Turtle.die $ Turtle.format ("Unable to find journal file: "%Turtle.fp%"\nIs your LEDGER_FILE environment variable set correctly?") hledgerJournal else return ()
  let journalWithYears = journalFile opts []
  let aggregateReportDir = outputReportDir opts ["all"]
  aggregateYears <- includeYears ch journalWithYears
  let aggregateParams = ReportParams { ledgerFile = hledgerJournal
                                     , reportYears = aggregateYears
                                     , outputDir = aggregateReportDir}
  let aggregateOnlyReports = reportActions opts ch [transferBalance] aggregateParams
  ownerParams <- ownerParameters opts ch owners
  let ownerWithAggregateParams = (if length owners > 1 then [aggregateParams] else []) ++ ownerParams
  let sharedOptions = ["--pretty-tables", "--depth", "2"]
  let ownerWithAggregateReports = List.concat $ fmap (reportActions opts ch [incomeStatement sharedOptions, incomeMonthlyStatement sharedOptions, balanceSheet sharedOptions]) ownerWithAggregateParams
  let ownerOnlyReports = List.concat $ fmap (reportActions opts ch [accountList, unknownTransactions]) ownerParams
  parAwareActions opts (aggregateOnlyReports ++ ownerWithAggregateReports ++ ownerOnlyReports)

reportActions :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [ReportGenerator] -> ReportParams -> [IO (Either TurtlePath TurtlePath)]
reportActions opts ch reports (ReportParams journal years reportsDir) = do
  y <- years
  map (\r -> r opts ch journal reportsDir y) reports

accountList :: ReportGenerator
accountList opts ch journal baseOutDir year = do
  let reportArgs = ["accounts"]
  generateReport opts ch journal year (baseOutDir </> intPath year) ("accounts" <.> "txt") reportArgs (not . T.null)

unknownTransactions :: ReportGenerator
unknownTransactions opts ch journal baseOutDir year = do
  let reportArgs = ["print", "unknown"]
  generateReport opts ch journal year (baseOutDir </> intPath year) ("unknown-transactions" <.> "txt") reportArgs (not . T.null)

incomeStatement :: [T.Text] -> ReportGenerator
incomeStatement sharedOptions opts ch journal baseOutDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions
  generateReport opts ch journal year (baseOutDir </> intPath year) ("income-expenses" <.> "txt") reportArgs (not . T.null)

incomeMonthlyStatement :: [T.Text] -> ReportGenerator
incomeMonthlyStatement sharedOptions opts ch journal baseOutDir year = do
  let reportArgs = ["incomestatement"] ++ sharedOptions ++ ["--monthly"]
  generateReport opts ch journal year (baseOutDir </> intPath year </> "monthly") ("income-expenses" <.> "txt") reportArgs (not . T.null)

balanceSheet :: [T.Text] -> ReportGenerator
balanceSheet sharedOptions opts ch journal baseOutDir year = do
  let reportArgs = ["balancesheet"] ++ sharedOptions ++ ["--flat"]
  generateReport opts ch journal year (baseOutDir </> intPath year) ("balance-sheet" <.> "txt") reportArgs (not . T.null)

transferBalance :: ReportGenerator
transferBalance opts ch journal baseOutDir year = do
  let reportArgs = ["balance", "--pretty-tables", "--quarterly", "--flat", "--no-total", "transfer"]
  generateReport opts ch journal year (baseOutDir </> intPath year) ("transfer-balance" <.> "txt") reportArgs (\txt -> (length $ T.lines txt) > 4)

generateReport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Integer -> TurtlePath -> TurtlePath -> [T.Text] -> (T.Text -> Bool) -> IO (Either TurtlePath TurtlePath)
generateReport opts ch journal year reportsDir fileName args successCheck = do
  Turtle.mktree reportsDir
  let outputFile = reportsDir </> fileName
  let relativeJournal = relativeToBase opts journal
  let relativeOutputFile = relativeToBase opts outputFile
  let reportArgs = ["--file", Turtle.format Turtle.fp journal, "--period", Turtle.repr year] ++ args
  let reportDisplayArgs = ["--file", Turtle.format Turtle.fp relativeJournal, "--period", Turtle.repr year] ++ args
  let hledger = Turtle.format Turtle.fp $ FlowTypes.hlPath . hledgerInfo $ opts :: T.Text
  let cmdLabel = Turtle.format ("hledger "%Turtle.s) $ showCmdArgs reportDisplayArgs
  ((exitCode, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr Turtle.procStrictWithErr (hledger, reportArgs, mempty)
  if (successCheck stdOut)
    then
    do
      Turtle.writeTextFile outputFile (cmdLabel <> "\n\n"<> stdOut)
      logVerbose opts ch $ Turtle.format ("Wrote "%Turtle.fp) $ relativeOutputFile
      return $ Right outputFile
    else
    do
      channelErrLn ch $ Turtle.format ("Did not write '"%Turtle.fp%"' ("%Turtle.s%") "%Turtle.s) relativeOutputFile cmdLabel (Turtle.repr exitCode)
      exists <- Turtle.testfile outputFile
      if exists then Turtle.rm outputFile else return ()
      return $ Left outputFile

journalFile :: RuntimeOptions -> [TurtlePath] -> TurtlePath
journalFile opts dirs = (foldl (</>) (turtleBaseDir opts) ("import":dirs)) </> allYearsFileName

outputReportDir :: RuntimeOptions -> [TurtlePath] -> TurtlePath
outputReportDir opts dirs = foldl (</>) (turtleBaseDir opts) ("reports":dirs)

ownerParameters :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [TurtlePath] -> IO [ReportParams]
ownerParameters opts ch owners = do
  let actions = map (ownerParameters' opts ch) owners
  parAwareActions opts actions

ownerParameters' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath-> IO ReportParams
ownerParameters' opts ch owner = do
  let ownerJournal = journalFile opts [owner]
  years <- includeYears ch ownerJournal
  return $ ReportParams ownerJournal years (outputReportDir opts [owner])
