{-# LANGUAGE OverloadedStrings #-}

module TestHelpersTurtle where

import Hledger.Flow.Common (changePathAndExtension)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

inputJohnBogart :: [FilePath]
inputJohnBogart =
  [ "import/john/bogartbank/savings/1-in/2017/2017-11-30.csv",
    "import/john/bogartbank/savings/1-in/2017/2017-12-30.csv",
    "import/john/bogartbank/savings/1-in/2018/2018-02-30.csv",
    "import/john/bogartbank/savings/1-in/2018/2018-01-30.csv",
    "import/john/bogartbank/checking/1-in/2018/2018-11-30.csv",
    "import/john/bogartbank/checking/1-in/2018/2018-10-30.csv",
    "import/john/bogartbank/checking/1-in/2018/2018-12-30.csv",
    "import/john/bogartbank/checking/1-in/2019/2019-01-30.csv",
    "import/john/bogartbank/checking/1-in/2019/2019-02-30.csv"
  ]

inputJohnOther :: [FilePath]
inputJohnOther =
  [ "import/john/otherbank/creditcard/1-in/2017/2017-12-30.csv",
    "import/john/otherbank/creditcard/1-in/2018/2018-01-30.csv",
    "import/john/otherbank/investments/1-in/2018/2018-12-30.csv",
    "import/john/otherbank/investments/1-in/2019/2019-01-30.csv"
  ]

inputJaneBogart :: [FilePath]
inputJaneBogart =
  [ "import/jane/bogartbank/savings/1-in/2017/2017-12-30.csv",
    "import/jane/bogartbank/savings/1-in/2018/2018-01-30.csv",
    "import/jane/bogartbank/checking/1-in/2018/2018-12-30.csv",
    "import/jane/bogartbank/checking/1-in/2019/2019-01-30.csv"
  ]

inputJaneOther :: [FilePath]
inputJaneOther =
  [ "import/jane/otherbank/creditcard/1-in/2017/2017-12-30.csv",
    "import/jane/otherbank/creditcard/1-in/2018/2018-01-30.csv",
    "import/jane/otherbank/investments/1-in/2018/2018-12-30.csv",
    "import/jane/otherbank/investments/1-in/2019/2019-01-30.csv"
  ]

inputFiles :: [FilePath]
inputFiles = inputJohnBogart <> inputJohnOther <> inputJaneBogart <> inputJaneOther

journalFiles :: [FilePath]
journalFiles = toJournals inputFiles ++ ignoredJournalFiles

ignoredJournalFiles :: [FilePath]
ignoredJournalFiles = ["import/john/bogartbank/checking/3-journal/2018/not-a-journal.pdf"]

extraFiles :: [FilePath]
extraFiles = ["import/john/bogartbank/savings/2017-opening.journal"]

hiddenFiles :: [FilePath]
hiddenFiles =
  [ ".hiddenfile",
    "checking/.DS_Store",
    "import/john/bogartbank/savings/1-in/.anotherhiddenfile",
    "import/john/bogartbank/checking/1-in/2018/.hidden",
    "import/john/bogartbank/checking/3-journal/2018/.hidden"
  ]

toJournals :: [FilePath] -> [FilePath]
toJournals = map (changePathAndExtension "3-journal/" "journal")

touchAll :: [FilePath] -> IO ()
touchAll = mapM_ superTouch

superTouch :: FilePath -> IO ()
superTouch file = do
  createDirectoryIfMissing True (takeDirectory file)
  appendFile file ""
