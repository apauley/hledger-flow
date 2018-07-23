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
  echo "BEGIN: importCSVs"
  let importDir = baseDir </> "import"
  importExists <- testdir importDir
  if importExists
    then sh $ importBanks $ validDirs $ ls importDir
    else die $ format ("Unable to find CSV import dir at "%fp) importDir
  echo "END: importCSVs"

importBanks :: Shell FilePath -> Shell ()
importBanks bankDirs = do
  echoShell "BEGIN: importBanks"
  view bankDirs
  bd <- bankDirs
  bankName <- basenameLine bd
  importAccounts bankName $ ls bd
  echoShell "END: importBanks"

importAccounts :: Line -> Shell FilePath -> Shell ()
importAccounts bankName accountDirs = do
  echoShell $ "BEGIN: importAccounts"
  printShell bankName
  accDir <- accountDirs
  accName <- basenameLine accDir
  let rulesFileName = format (l%"-"%l%".rules") bankName accName
  let rulesFile = accDir </> fromText rulesFileName
  printShell rulesFile
  let preprocessScript = accDir </> fromText "preprocess"
  let accountSrcFiles = onlyFiles $ find (has (text "1-in")) accDir
  importAccountFiles bankName accName rulesFile preprocessScript accountSrcFiles
  echoShell $ "END: importAccounts"

importAccountFiles :: Line -> Line -> FilePath -> FilePath -> Shell FilePath -> Shell ()
importAccountFiles bankName accountName rulesFile preprocessScript accountSrcFiles = do
  echoShell "BEGIN: importAccountFiles"
  view accountSrcFiles
  srcFile <- accountSrcFiles
  csvFile <- liftIO $ preprocessIfNeeded preprocessScript bankName accountName srcFile
  printShell $ format ("csvFile: "%fp) csvFile
  liftIO $ hledgerImport csvFile rulesFile
  echoShell "END: importAccountFiles"

preprocessIfNeeded :: FilePath -> Line -> Line -> FilePath -> IO FilePath
preprocessIfNeeded script bank account src = do
  shouldPreprocess <- testfile script
  print $ format ("shouldPreprocess: "%s) $ repr shouldPreprocess
  if shouldPreprocess
    then preprocess script bank account src
    else return src

preprocess :: FilePath -> Line -> Line -> FilePath -> IO FilePath
preprocess script bank account src = do
  let csvOut = changePathAndExtension "2-preprocessed" "csv" src
  let script' = format fp script :: Text
  procs script' [lineToText bank, lineToText account, format fp src, format fp csvOut] empty
  return csvOut

hledgerImport :: FilePath -> FilePath -> IO ()
hledgerImport csvSrc rulesFile = do
  echo "BEGIN: hledgerImport"
  procs "hledger" ["print", "--rules-file", format fp rulesFile, "--file", format fp csvSrc, "--output-file", "/tmp/ht"] empty
  echo "END: hledgerImport"

changePathAndExtension :: FilePath -> Text -> FilePath -> FilePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeExtension :: Text -> FilePath -> FilePath
changeExtension extension path = (dropExtension path) <.> extension

changeOutputPath :: FilePath -> FilePath -> FilePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ splitDirectories srcFile
  where changeSrcDir f = if (f == "1-in/" || f == "2-preprocessed/") then newOutputLocation else f
