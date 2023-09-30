{-# LANGUAGE OverloadedStrings #-}

module TestHelpersTurtle where

import Turtle

import Hledger.Flow.PathHelpers (TurtlePath)
import Hledger.Flow.Common (changePathAndExtension)

inputJohnBogart :: [TurtlePath]
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

inputJohnOther :: [TurtlePath]
inputJohnOther = [
  "import/john/otherbank/creditcard/1-in/2017/2017-12-30.csv",
  "import/john/otherbank/creditcard/1-in/2018/2018-01-30.csv",
  "import/john/otherbank/investments/1-in/2018/2018-12-30.csv",
  "import/john/otherbank/investments/1-in/2019/2019-01-30.csv"]

inputJaneBogart :: [TurtlePath]
inputJaneBogart = [
  "import/jane/bogartbank/savings/1-in/2017/2017-12-30.csv",
  "import/jane/bogartbank/savings/1-in/2018/2018-01-30.csv",
  "import/jane/bogartbank/checking/1-in/2018/2018-12-30.csv",
  "import/jane/bogartbank/checking/1-in/2019/2019-01-30.csv"]

inputJaneOther :: [TurtlePath]
inputJaneOther = [
  "import/jane/otherbank/creditcard/1-in/2017/2017-12-30.csv",
  "import/jane/otherbank/creditcard/1-in/2018/2018-01-30.csv",
  "import/jane/otherbank/investments/1-in/2018/2018-12-30.csv",
  "import/jane/otherbank/investments/1-in/2019/2019-01-30.csv"]

inputFiles :: [TurtlePath]
inputFiles = inputJohnBogart <> inputJohnOther <> inputJaneBogart <> inputJaneOther

journalFiles :: [TurtlePath]
journalFiles = toJournals inputFiles ++ ignoredJournalFiles

ignoredJournalFiles :: [TurtlePath]
ignoredJournalFiles = ["import/john/bogartbank/checking/3-journal/2018/not-a-journal.pdf"]

extraFiles :: [TurtlePath]
extraFiles = ["import/john/bogartbank/savings/2017-opening.journal"]

hiddenFiles :: [TurtlePath]
hiddenFiles = [".hiddenfile",
               "checking/.DS_Store",
               "import/john/bogartbank/savings/1-in/.anotherhiddenfile",
               "import/john/bogartbank/checking/1-in/2018/.hidden",
               "import/john/bogartbank/checking/3-journal/2018/.hidden"]

toJournals :: [TurtlePath] -> [TurtlePath]
toJournals = map (changePathAndExtension "3-journal/" "journal")

touchAll :: [TurtlePath] -> Shell ()
touchAll = foldl (\acc file -> acc <> superTouch file) (return ())

superTouch :: TurtlePath -> Shell ()
superTouch file = do
  mktree $ directory file
  touch file
