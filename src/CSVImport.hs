{-# LANGUAGE OverloadedStrings #-}

module CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn)
import Data.Text.IO (putStrLn)
import Common

importCSVs :: FilePath -> IO ()
importCSVs baseDir = do
  let importDir = baseDir </> "import"
  importExists <- testdir importDir
  let journals = if importExists
        then importBanks $ validDirs $ ls importDir
        else die $ format ("Unable to find CSV import dir at "%fp) importDir
  sh $ writeJournals (baseDir </> "import-all.journal") journals
  echo "Now viewing journals"
  view journals
  echo "Done viewing journals"

writeJournals :: FilePath -> Shell FilePath -> Shell ()
writeJournals journalFile journals = do
  t <- pathsToText journals
  liftIO $ writeTextFile journalFile t

pathsToText :: Shell FilePath -> Shell Text
pathsToText paths = do
  p <- paths
  let t = format ("!include "%fp%"\n") p
  return t

importBanks :: Shell FilePath -> Shell FilePath
importBanks bankDirs = do
  bd <- bankDirs
  bankName <- basenameLine bd
  let bankJournals = importAccounts bankName $ ls bd
  liftIO $ putStrLn $ format ("\n\nJournals for "%l) bankName
  bankJournals

importAccounts :: Line -> Shell FilePath -> Shell FilePath
importAccounts bankName accountDirs = do
  accDir <- accountDirs
  accName <- basenameLine accDir
  let rulesFileName = format (l%"-"%l%".rules") bankName accName
  let rulesFile = accDir </> fromText rulesFileName
  let preprocessScript = accDir </> fromText "preprocess"
  let accountSrcFiles = onlyFiles $ find (has (text "1-in")) accDir
  let accJournals = importAccountFiles bankName accName rulesFile preprocessScript accountSrcFiles
  accJournals

importAccountFiles :: Line -> Line -> FilePath -> FilePath -> Shell FilePath -> Shell FilePath
importAccountFiles bankName accountName rulesFile preprocessScript accountSrcFiles = do
  srcFile <- accountSrcFiles
  csvFile <- preprocessIfNeeded preprocessScript bankName accountName srcFile
  hledgerImport csvFile rulesFile

preprocessIfNeeded :: FilePath -> Line -> Line -> FilePath -> Shell FilePath
preprocessIfNeeded script bank account src = do
  shouldPreprocess <- testfile script
  if shouldPreprocess
    then preprocess script bank account src
    else return src

preprocess :: FilePath -> Line -> Line -> FilePath -> Shell FilePath
preprocess script bank account src = do
  let csvOut = changePathAndExtension "2-preprocessed" "csv" src
  mktree $ directory csvOut
  let script' = format fp script :: Text
  procs script' [lineToText bank, lineToText account, format fp src, format fp csvOut] empty
  return csvOut

hledgerImport :: FilePath -> FilePath -> Shell FilePath
hledgerImport csvSrc rulesFile = do
  let journalOut = changePathAndExtension "3-journal" "journal" csvSrc
  mktree $ directory journalOut
  procs "hledger" ["print", "--rules-file", format fp rulesFile, "--file", format fp csvSrc, "--output-file", format fp journalOut] empty
  return journalOut

changePathAndExtension :: FilePath -> Text -> FilePath -> FilePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeExtension :: Text -> FilePath -> FilePath
changeExtension extension path = (dropExtension path) <.> extension

changeOutputPath :: FilePath -> FilePath -> FilePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ splitDirectories srcFile
  where changeSrcDir f = if (f == "1-in/" || f == "2-preprocessed/") then newOutputLocation else f
