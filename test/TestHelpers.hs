{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestHelpers where

import Path

import Data.Maybe (fromMaybe)

import Hledger.Flow.Internals (versionInfo, systemInfo)
import Hledger.Flow.PathHelpers (RelFile)

import qualified Hledger.Flow.Types as FlowTypes
import Hledger.Flow.RuntimeOptions

defaultHlInfo :: FlowTypes.HledgerInfo
defaultHlInfo = FlowTypes.HledgerInfo [absfile|/path/to/hledger|] "1.2.3"

defaultOpts :: FlowTypes.BaseDir -> RuntimeOptions
defaultOpts bd = RuntimeOptions {
    baseDir = bd
  , importRunDir = [reldir|./|]
  , importStartYear = Nothing
  , onlyNewFiles = False
  , hfVersion = versionInfo
  , hledgerInfo = defaultHlInfo
  , sysInfo = systemInfo
  , verbose = False
  , showOptions = False
  , sequential = False
}

toJournal :: RelFile -> RelFile
toJournal inFile = do
  let journalFile = fromMaybe [relfile|oops.err|] $ replaceExtension ".journal" inFile
  let journalName = filename journalFile
  let yearDir = dirname . parent $ inFile
  (parent . parent . parent) inFile </> [reldir|3-journal|] </> yearDir </> journalName

inputJohnSavings2017 :: [RelFile]
inputJohnSavings2017 = [
  [relfile|import/john/bogartbank/savings/1-in/2017/2017-11-30.csv|],
  [relfile|import/john/bogartbank/savings/1-in/2017/2017-12-30.csv|]
  ]

johnSavingsJournals2017 :: [RelFile]
johnSavingsJournals2017 = map toJournal inputJohnSavings2017

inputJohnSavings2018 :: [RelFile]
inputJohnSavings2018 = [
  [relfile|import/john/bogartbank/savings/1-in/2018/2018-02-30.csv|],
  [relfile|import/john/bogartbank/savings/1-in/2018/2018-01-30.csv|]
  ]

johnSavingsJournals2018 :: [RelFile]
johnSavingsJournals2018 = map toJournal inputJohnSavings2018

inputJohnChecking2018 :: [RelFile]
inputJohnChecking2018 = [
  [relfile|import/john/bogartbank/checking/1-in/2018/2018-11-30.csv|],
  [relfile|import/john/bogartbank/checking/1-in/2018/2018-10-30.csv|],
  [relfile|import/john/bogartbank/checking/1-in/2018/2018-12-30.csv|]
  ]

johnCheckingJournals2018 :: [RelFile]
johnCheckingJournals2018 = map toJournal inputJohnChecking2018

inputJohnChecking2019 :: [RelFile]
inputJohnChecking2019 = [
  [relfile|import/john/bogartbank/checking/1-in/2019/2019-01-30.csv|],
  [relfile|import/john/bogartbank/checking/1-in/2019/2019-02-30.csv|]
  ]

johnCheckingJournals2019 :: [RelFile]
johnCheckingJournals2019 = map toJournal inputJohnChecking2019

inputJohnBogart :: [RelFile]
inputJohnBogart = inputJohnSavings2017 <> inputJohnSavings2018 <> inputJohnChecking2018 <> inputJohnChecking2019

johnCC2017 :: RelFile
johnCC2017 = [relfile|import/john/otherbank/creditcard/1-in/2017/2017-12-30.csv|]

johnCCJournal2017 :: RelFile
johnCCJournal2017 = toJournal johnCC2017

johnCC2018 :: RelFile
johnCC2018 = [relfile|import/john/otherbank/creditcard/1-in/2018/2018-01-30.csv|]

johnCCJournal2018 :: RelFile
johnCCJournal2018 = toJournal johnCC2018

johnInvest2018 :: RelFile
johnInvest2018 = [relfile|import/john/otherbank/investments/1-in/2018/2018-12-30.csv|]

johnInvestJournal2018 :: RelFile
johnInvestJournal2018 = toJournal johnInvest2018

johnInvest2019 :: RelFile
johnInvest2019 = [relfile|import/john/otherbank/investments/1-in/2019/2019-01-30.csv|]

johnInvestJournal2019 :: RelFile
johnInvestJournal2019 = toJournal johnInvest2019

inputJohnOther :: [RelFile]
inputJohnOther = [johnCC2017, johnCC2018, johnInvest2018, johnInvest2019]

janeSavings2017 :: RelFile
janeSavings2017 = [relfile|import/jane/bogartbank/savings/1-in/2017/2017-12-30.csv|]

janeSavings2018 :: [RelFile]
janeSavings2018 = [
    [relfile|import/jane/bogartbank/savings/1-in/2018/2018-01-30.csv|]
  , [relfile|import/jane/bogartbank/savings/1-in/2018/2018-12-30.csv|]
  ]

janeSavings2019 :: RelFile
janeSavings2019 = [relfile|import/jane/bogartbank/savings/1-in/2019/2019-01-30.csv|]

inputJaneBogart :: [RelFile]
inputJaneBogart = [
  janeSavings2017
  , [relfile|import/jane/bogartbank/savings/3-journals/2018/2018-01-30.journal|]
  , [relfile|import/jane/bogartbank/savings/3-journals/2018/2018-12-30.journal|]
  , janeSavings2019
  ]

janeSavingsJournal2017 :: RelFile
janeSavingsJournal2017 = toJournal janeSavings2017

janeSavingsJournals2018 :: [RelFile]
janeSavingsJournals2018 = map toJournal janeSavings2018

janeSavingsJournal2019 :: RelFile
janeSavingsJournal2019 = toJournal janeSavings2019

janeSavingsJournals :: [RelFile]
janeSavingsJournals = [janeSavingsJournal2017] ++ janeSavingsJournals2018 ++ [janeSavingsJournal2019]

janeCC2017 :: RelFile
janeCC2017 = [relfile|import/jane/otherbank/creditcard/1-in/2017/2017-12-30.csv|]

janeCCJournal2017 :: RelFile
janeCCJournal2017 = toJournal janeCC2017

janeCC2018 :: RelFile
janeCC2018 = [relfile|import/jane/otherbank/creditcard/1-in/2018/2018-01-30.csv|]

janeCCJournal2018 :: RelFile
janeCCJournal2018 = toJournal janeCC2018

janeInvest2018 :: RelFile
janeInvest2018 = [relfile|import/jane/otherbank/investments/1-in/2018/2018-12-30.csv|]

janeInvestJournal2018 :: RelFile
janeInvestJournal2018 = toJournal janeInvest2018

janeInvest2019 :: RelFile
janeInvest2019 = [relfile|import/jane/otherbank/investments/1-in/2019/2019-01-30.csv|]

janeInvestJournal2019 :: RelFile
janeInvestJournal2019 = toJournal janeInvest2019

inputJaneOther :: [RelFile]
inputJaneOther = [janeCC2017, janeCC2018, janeInvest2018, janeInvest2019]

inputFiles :: [RelFile]
inputFiles = inputJohnBogart <> inputJohnOther <> inputJaneBogart <> inputJaneOther

journalFiles :: [RelFile]
journalFiles = map toJournal inputFiles
