{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Reports
    ( generateReports
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn, writeFile)
import qualified Data.Text as T
import Hledger.Flow.Types (LogMessage, FullTimedOutput)
import Hledger.Flow.Report.Types
import Hledger.Flow.Common
import Control.Concurrent.STM

generateReports :: ReportOptions -> IO ()
generateReports opts = sh (
  do
    ch <- liftIO newTChanIO
    logHandle <- fork $ consoleChannelLoop ch
    liftIO $ if (showOptions opts) then channelOut ch (repr opts) else return ()
    (reports, diff) <- time $ liftIO $ generateReports' opts ch
    liftIO $ channelOut ch $ format ("Generated "%d%" reports in "%s) (length reports) $ repr diff
    liftIO $ terminateChannelLoop ch
    wait logHandle
  )

generateReports' :: ReportOptions -> TChan LogMessage -> IO [FilePath]
generateReports' opts ch = do
  logVerbose opts ch "Something will be here Real Soon Now (tm)"
  channelOut ch "Report generation has not been implemented. Yet. https://github.com/apauley/hledger-flow/pull/4"
  ownerReports opts ch "everyone"

ownerReports :: ReportOptions -> TChan LogMessage -> Text -> IO [FilePath]
ownerReports opts ch owner = do
  let journal = (baseDir opts) </> "all-years" <.> "journal"
  let reportsDir = (baseDir opts) </> "reports" </> fromText owner
  let actions = map (\r -> r opts ch journal reportsDir) [accountList, incomeStatement]
  results <- if (sequential opts) then sequence actions else single $ shellToList $ parallel actions
  return $ map fst results

incomeStatement :: ReportOptions -> TChan LogMessage -> FilePath -> FilePath -> IO (FilePath, FullTimedOutput)
incomeStatement opts ch journal reportsDir = do
  mktree reportsDir
  let outputFile = reportsDir </> "income-expenses" <.> "txt"
  let sharedOptions = ["--depth", "2", "--pretty-tables", "not:equity"]
  let reportArgs = ["incomestatement"] ++ sharedOptions ++ ["--average", "--yearly"]
  generateReport' opts ch journal outputFile reportArgs

accountList :: ReportOptions -> TChan LogMessage -> FilePath -> FilePath -> IO (FilePath, FullTimedOutput)
accountList opts ch journal reportsDir = do
  let outputFile = reportsDir </> "accounts" <.> "txt"
  let reportArgs = ["accounts"]
  generateReport' opts ch journal outputFile reportArgs

generateReport' :: ReportOptions -> TChan LogMessage -> FilePath -> FilePath -> [Text] -> IO (FilePath, FullTimedOutput)
generateReport' opts ch journal outputFile args = do
  let reportsDir = directory outputFile
  mktree reportsDir
  let relativeJournal = relativeToBase opts journal
  let reportArgs = ["--file", format fp journal] ++ args
  let reportDisplayArgs = ["--file", format fp relativeJournal] ++ args
  let action = procStrictWithErr "hledger" reportArgs empty
  let cmd = format ("hledger "%s) $ showCmdArgs reportDisplayArgs
  result@((exitCode, stdOut, stdErr), _) <- timeAndExitOnErr opts ch cmd action
  if not (T.null stdOut) then do
    writeTextFile outputFile (cmd <> "\n\n"<> stdOut)
    channelOut ch $ format ("Wrote "%fp) $ relativeToBase opts outputFile
    else channelErr ch $ format ("No report output for '"%s%"' "%s) cmd (repr exitCode)
  if not (T.null stdErr)
    then channelErr ch $ stdErr
    else return ()
  return (outputFile, result)
