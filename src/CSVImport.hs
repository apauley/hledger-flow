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
  csvFile <- liftIO $ preprocessIfNeeded preprocessScript bankName accName accDir
  printShell $ format ("csvFile: "%fp) csvFile
  view accountDirs
  echoShell $ "END: importAccounts"

preprocessIfNeeded :: FilePath -> Line -> Line -> FilePath -> IO FilePath
preprocessIfNeeded script bank account src = do
  shouldPreprocess <- testfile script
  print shouldPreprocess
  if shouldPreprocess
    then preprocess script bank account src
    else return src

preprocess :: FilePath -> Line -> Line -> FilePath -> IO FilePath
preprocess script bank account src = return src
