{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn, take)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import Hledger.Flow.Types (LogMessage)
import Hledger.Flow.Import.Types
import Hledger.Flow.Common
import Control.Concurrent.STM

importCSVs :: ImportOptions -> IO ()
importCSVs opts = sh (
  do
    ch <- liftIO newTChanIO
    logHandle <- fork $ consoleChannelLoop ch
    liftIO $ if (showOptions opts) then channelOut ch (repr opts) else return ()
    liftIO $ logVerbose opts ch "Starting import"
    (journals, diff) <- time $ liftIO $ importCSVs' opts ch
    liftIO $ channelOut ch $ format ("Imported "%d%" journals in "%s) (length journals) $ repr diff
    liftIO $ terminateChannelLoop ch
    wait logHandle
  )

importCSVs' :: ImportOptions -> TChan LogMessage -> IO [FilePath]
importCSVs' opts ch = do
  channelOut ch "Collecting input files..."
  (inputFiles, diff) <- time $ single . shellToList . onlyFiles $ find (has (suffix "1-in")) $ baseDir opts
  let fileCount = length inputFiles
  if (fileCount == 0) then
    do
      let msg = format ("I couldn't find any input files underneath "%fp
                        %"\n\nhledger-makitso expects to find its input files in specifically\nnamed directories.\n\n"%
                        "Have a look at the documentation for a detailed explanation:\n"%s) (dirname (baseDir opts) </> "import/") (docURL "input-files")
      stderr $ select $ textToLines msg
      exit $ ExitFailure 1
    else
    do
      channelOut ch $ format ("Found "%d%" input files in "%s%". Proceeding with import...") fileCount (repr diff)
      let actions = map (extractAndImport opts ch) inputFiles :: [IO FilePath]
      importedJournals <- if (sequential opts) then sequence actions else single . shellToList $ parallel actions
      sh $ writeIncludesUpTo opts ch "import" importedJournals
      return importedJournals

extractAndImport :: ImportOptions -> TChan LogMessage -> FilePath -> IO FilePath
extractAndImport opts ch inputFile = do
  case extractImportDirs inputFile of
    Right importDirs -> importCSV opts ch importDirs inputFile
    Left errorMessage -> do
      stderr $ select $ textToLines errorMessage
      exit $ ExitFailure 1

importCSV :: ImportOptions -> TChan LogMessage -> ImportDirs -> FilePath -> IO FilePath
importCSV opts ch importDirs srcFile = do
  let preprocessScript = accountDir importDirs </> "preprocess"
  let constructScript = accountDir importDirs </> "construct"
  let bankName = importDirLine bankDir importDirs
  let accountName = importDirLine accountDir importDirs
  let ownerName = importDirLine ownerDir importDirs
  csvFile <- preprocessIfNeeded opts ch preprocessScript bankName accountName ownerName srcFile
  doCustomConstruct <- verboseTestFile opts ch constructScript
  let importFun = if doCustomConstruct
        then customConstruct opts ch constructScript bankName accountName ownerName
        else hledgerImport opts ch
  let journalOut = changePathAndExtension "3-journal" "journal" csvFile
  mktree $ directory journalOut
  importFun csvFile journalOut

preprocessIfNeeded :: ImportOptions -> TChan LogMessage -> FilePath -> Line -> Line -> Line -> FilePath -> IO FilePath
preprocessIfNeeded opts ch script bank account owner src = do
  shouldPreprocess <- verboseTestFile opts ch script
  if shouldPreprocess
    then preprocess opts ch script bank account owner src
    else return src

preprocess :: ImportOptions -> TChan LogMessage -> FilePath -> Line -> Line -> Line -> FilePath -> IO FilePath
preprocess opts ch script bank account owner src = do
  let csvOut = changePathAndExtension "2-preprocessed" "csv" src
  mktree $ directory csvOut
  let script' = format fp script :: Text
  let action = proc script' [format fp src, format fp csvOut, lineToText bank, lineToText account, lineToText owner] empty
  let relScript = relativeToBase opts script
  let relSrc = relativeToBase opts src
  let msg = format ("executing '"%fp%"' on '"%fp%"'") relScript relSrc
  _ <- logVerboseTime opts ch msg action
  return csvOut

hledgerImport :: ImportOptions -> TChan LogMessage -> FilePath -> FilePath -> IO FilePath
hledgerImport opts ch csvSrc journalOut = do
  case extractImportDirs csvSrc of
    Right importDirs -> hledgerImport' opts ch importDirs csvSrc journalOut
    Left errorMessage -> do
      stderr $ select $ textToLines errorMessage
      exit $ ExitFailure 1

hledgerImport' :: ImportOptions -> TChan LogMessage -> ImportDirs -> FilePath -> FilePath -> IO FilePath
hledgerImport' opts ch importDirs csvSrc journalOut = do
  let candidates = rulesFileCandidates csvSrc importDirs
  maybeRulesFile <- firstExistingFile candidates
  let relCSV = relativeToBase opts csvSrc
  case maybeRulesFile of
    Just rf -> do
      let relRules = relativeToBase opts rf
      let action = proc "hledger" ["print", "--rules-file", format fp rf, "--file", format fp csvSrc, "--output-file", format fp journalOut] empty
      let msg = format ("importing '"%fp%"' using rules file '"%fp%"'") relCSV relRules
      _ <- logVerboseTime opts ch msg action
      return journalOut
    Nothing ->
      do
        let relativeCandidates = map (relativeToBase opts) candidates
        let candidatesTxt = T.intercalate "\n" $ map (format fp) relativeCandidates
        let msg = format ("I couldn't find an hledger rules file while trying to import\n"%fp
                          %"\n\nI will happily use the first rules file I can find from any one of these "%d%" files:\n"%s
                          %"\n\nHere is a bit of documentation about rules files that you may find helpful:\n"%s)
                  relCSV (length candidates) candidatesTxt (docURL "rules-files")
        stderr $ select $ textToLines msg
        exit $ ExitFailure 1

rulesFileCandidates :: FilePath -> ImportDirs -> [FilePath]
rulesFileCandidates csvSrc importDirs = statementSpecificRulesFiles csvSrc importDirs ++ generalRulesFiles importDirs

importDirLines :: (ImportDirs -> FilePath) -> ImportDirs -> [Line]
importDirLines dirFun importDirs = NonEmpty.toList $ textToLines $ format fp $ dirname $ dirFun importDirs

importDirLine :: (ImportDirs -> FilePath) -> ImportDirs -> Line
importDirLine dirFun importDirs = foldl (<>) "" $ importDirLines dirFun importDirs

generalRulesFiles :: ImportDirs -> [FilePath]
generalRulesFiles importDirs = do
  let bank = importDirLines bankDir importDirs
  let account = importDirLines accountDir importDirs
  let accountRulesFile = accountDir importDirs </> buildFilename (bank ++ account) "rules"

  let bankRulesFile = importDir importDirs </> buildFilename bank "rules"
  [accountRulesFile, bankRulesFile]

statementSpecificRulesFiles :: FilePath -> ImportDirs -> [FilePath]
statementSpecificRulesFiles csvSrc importDirs = do
  let srcSuffix = snd $ T.breakOnEnd "_" (format fp (basename csvSrc))

  if ((T.take 3 srcSuffix) == "rfo")
    then
    do
      let srcSpecificFilename = fromText srcSuffix <.> "rules"
      map (</> srcSpecificFilename) [accountDir importDirs, bankDir importDirs, importDir importDirs]
    else []

customConstruct :: ImportOptions -> TChan LogMessage -> FilePath -> Line -> Line -> Line -> FilePath -> FilePath -> IO FilePath
customConstruct opts ch constructScript bank account owner csvSrc journalOut = do
  let script = format fp constructScript :: Text
  let importOut = inproc script [format fp csvSrc, "-", lineToText bank, lineToText account, lineToText owner] empty
  let action = proc "hledger" ["print", "--ignore-assertions", "--file", "-", "--output-file", format fp journalOut] importOut
  let relScript = relativeToBase opts constructScript
  let relSrc = relativeToBase opts csvSrc
  let msg = format ("executing '"%fp%"' on '"%fp%"'") relScript relSrc
  _ <- logVerboseTime opts ch msg action

  return journalOut
