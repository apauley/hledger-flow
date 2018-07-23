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
  importAccountFiles bankName accName preprocessScript accountSrcFiles
  echoShell $ "END: importAccounts"

importAccountFiles :: Line -> Line -> FilePath -> Shell FilePath -> Shell ()
importAccountFiles bankName accountName preprocessScript accountSrcFiles = do
  echoShell "BEGIN: importAccountFiles"
  view accountSrcFiles
  srcFile <- accountSrcFiles
  csvFile <- liftIO $ preprocessIfNeeded preprocessScript bankName accountName srcFile
  printShell $ format ("csvFile: "%fp) csvFile
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
  let csvOut = csvOutputFile src
  return csvOut

csvOutputFile :: FilePath -> FilePath
csvOutputFile srcFile = Turtle.mconcat $ map (\f -> if (f == "1-in/") then "2-preprocessed" else f) $ splitDirectories srcFile
