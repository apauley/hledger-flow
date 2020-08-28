{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module TestHelpers where

import Path
import Turtle
import Prelude hiding (FilePath)

import qualified Hledger.Flow.Types as FlowTypes
import Hledger.Flow.Common
import Hledger.Flow.RuntimeOptions

inputJohnBogart :: [FilePath]
inputJohnBogart = [
  "import/john/bogartbank/savings/1-in/2017/2017-11-30.csv",
  "import/john/bogartbank/savings/1-in/2017/2017-12-30.csv",
  "import/john/bogartbank/savings/1-in/2018/2018-02-30.csv",
  "import/john/bogartbank/savings/1-in/2018/2018-01-30.csv",
  "import/john/bogartbank/checking/1-in/2018/2018-11-30.csv",
  "import/john/bogartbank/checking/1-in/2018/2018-10-30.csv",
  "import/john/bogartbank/checking/1-in/2018/2018-12-30.csv",
  "import/john/bogartbank/checking/1-in/2019/2019-01-30.csv",
  "import/john/bogartbank/checking/1-in/2019/2019-02-30.csv"]

inputJohnOther :: [FilePath]
inputJohnOther = [
  "import/john/otherbank/creditcard/1-in/2017/2017-12-30.csv",
  "import/john/otherbank/creditcard/1-in/2018/2018-01-30.csv",
  "import/john/otherbank/investments/1-in/2018/2018-12-30.csv",
  "import/john/otherbank/investments/1-in/2019/2019-01-30.csv"]

inputJaneBogart :: [FilePath]
inputJaneBogart = [
  "import/jane/bogartbank/savings/1-in/2017/2017-12-30.csv",
  "import/jane/bogartbank/savings/1-in/2018/2018-01-30.csv",
  "import/jane/bogartbank/checking/1-in/2018/2018-12-30.csv",
  "import/jane/bogartbank/checking/1-in/2019/2019-01-30.csv"]

inputJaneOther :: [FilePath]
inputJaneOther = [
  "import/jane/otherbank/creditcard/1-in/2017/2017-12-30.csv",
  "import/jane/otherbank/creditcard/1-in/2018/2018-01-30.csv",
  "import/jane/otherbank/investments/1-in/2018/2018-12-30.csv",
  "import/jane/otherbank/investments/1-in/2019/2019-01-30.csv"]

inputFiles :: [FilePath]
inputFiles = inputJohnBogart <> inputJohnOther <> inputJaneBogart <> inputJaneOther

journalFiles :: [FilePath]
journalFiles = toJournals inputFiles

extraFiles :: [FilePath]
extraFiles = ["import/john/bogartbank/savings/2017-opening.journal"]

hiddenFiles :: [FilePath]
hiddenFiles = [".hiddenfile", "checking/.DS_Store", "import/john/bogartbank/savings/1-in/.anotherhiddenfile", "import/john/bogartbank/checking/1-in/2018/.hidden"]

defaultHlInfo :: FlowTypes.HledgerInfo
defaultHlInfo = FlowTypes.HledgerInfo "/path/to/hledger" "1.2.3"

defaultOpts :: FlowTypes.BaseDir -> RuntimeOptions
defaultOpts bd = RuntimeOptions bd [reldir|./|] True versionInfo' defaultHlInfo systemInfo False False False

toJournals :: [FilePath] -> [FilePath]
toJournals = map (changePathAndExtension "3-journal" "journal")

touchAll :: [FilePath] -> Shell ()
touchAll = foldl (\acc file -> acc <> superTouch file) (return ())

superTouch :: FilePath -> Shell ()
superTouch file = do
  mktree $ directory file
  touch file
