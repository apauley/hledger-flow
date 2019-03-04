{-# LANGUAGE OverloadedStrings #-}

module CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn, take)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Hledger.MakeItSo.Data.Types
import Common

data ImportDirs = ImportDirs { importDir  :: FilePath
                             , ownerDir   :: FilePath
                             , bankDir    :: FilePath
                             , accountDir :: FilePath
                             , stateDir   :: FilePath
                             , yearDir    :: FilePath
                             } deriving (Show)

importCSVs :: HMISOptions -> IO ()
importCSVs = sh . importCSVs'

importCSVs' :: HMISOptions -> Shell [FilePath]
importCSVs' opts = do
  logVerbose opts "Collecting input files..."
  inputFiles <- shellToList . onlyFiles $ find (has (suffix "1-in/")) $ baseDir opts
  let fileCount = length inputFiles
  logVerbose opts $ format ("Found "%d%" input files") $ fileCount
  if (fileCount == 0) then
    do
      let msg = format ("I couldn't find any input files underneath "%fp
                        %"\n\nhledger-makitso expects to find its input files in specifically\nnamed directories.\n\n"%
                        "Have a look at the documentation for a detailed explanation:\n"%s) (dirname (baseDir opts) </> "import/") (docURL "input-files")
      stderr $ select $ textToLines msg
      exit $ ExitFailure 1
    else
    do
      importedJournals <- shellToList . (extractAndImport opts) . select $ inputFiles
      importIncludes <- writeIncludesUpTo "import" importedJournals
      writeMakeItSoJournal (baseDir opts) importIncludes

extractAndImport :: HMISOptions -> Shell FilePath -> Shell FilePath
extractAndImport opts inputFiles = do
  inputFile <- inputFiles
  case extractImportDirs inputFile of
    Right importDirs -> importCSV opts importDirs inputFile
    Left errorMessage -> do
      stderr $ select $ textToLines errorMessage
      exit $ ExitFailure 1

importCSV :: HMISOptions -> ImportDirs -> FilePath -> Shell FilePath
importCSV opts importDirs srcFile = do
  let preprocessScript = accountDir importDirs </> "preprocess"
  let constructScript = accountDir importDirs </> "construct"
  let bankName = importDirLine bankDir importDirs
  let accountName = importDirLine accountDir importDirs
  let ownerName = importDirLine ownerDir importDirs
  csvFile <- preprocessIfNeeded opts preprocessScript bankName accountName ownerName srcFile
  doCustomConstruct <- verboseTestFile opts constructScript
  let importFun = if doCustomConstruct
        then customConstruct opts constructScript bankName accountName ownerName
        else hledgerImport opts
  let journalOut = changePathAndExtension "3-journal" "journal" csvFile
  mktree $ directory journalOut
  importFun csvFile journalOut

preprocessIfNeeded :: HMISOptions -> FilePath -> Line -> Line -> Line -> FilePath -> Shell FilePath
preprocessIfNeeded opts script bank account owner src = do
  shouldPreprocess <- verboseTestFile opts script
  if shouldPreprocess
    then preprocess opts script bank account owner src
    else return src

preprocess :: HMISOptions -> FilePath -> Line -> Line -> Line -> FilePath -> Shell FilePath
preprocess opts script bank account owner src = do
  let csvOut = changePathAndExtension "2-preprocessed" "csv" src
  mktree $ directory csvOut
  let script' = format fp script :: Text
  let action = procs script' [format fp src, format fp csvOut, lineToText bank, lineToText account, lineToText owner] empty
  let relScript = fromMaybe script $ stripPrefix (directory $ baseDir opts) script
  let relSrc = fromMaybe src $ stripPrefix (directory $ baseDir opts) src
  let msg = format ("executing '"%fp%"' on '"%fp%"'") relScript relSrc
  _ <- liftIO $ logVerboseTime opts msg action
  return csvOut

hledgerImport :: HMISOptions -> FilePath  -> FilePath -> Shell FilePath
hledgerImport opts csvSrc journalOut = do
  case extractImportDirs csvSrc of
    Right importDirs -> hledgerImport' opts importDirs csvSrc journalOut
    Left errorMessage -> do
      stderr $ select $ textToLines errorMessage
      exit $ ExitFailure 1

hledgerImport' :: HMISOptions -> ImportDirs -> FilePath -> FilePath -> Shell FilePath
hledgerImport' opts importDirs csvSrc journalOut = do
  let candidates = rulesFileCandidates csvSrc importDirs
  maybeRulesFile <- firstExistingFile candidates
  let relCSV = fromMaybe csvSrc $ stripPrefix (directory $ baseDir opts) csvSrc
  case maybeRulesFile of
    Just rf -> do
      let relRules = fromMaybe rf $ stripPrefix (directory $ baseDir opts) rf
      let action = procs "hledger" ["print", "--rules-file", format fp rf, "--file", format fp csvSrc, "--output-file", format fp journalOut] empty
      let msg = format ("importing '"%fp%"' using rules file '"%fp%"'") relCSV relRules
      _ <- liftIO $ logVerboseTime opts msg action
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

importDirBreakdown ::  FilePath -> [FilePath]
importDirBreakdown = importDirBreakdown' []

importDirBreakdown' :: [FilePath] -> FilePath -> [FilePath]
importDirBreakdown' acc path = do
  let dir = directory path
  if (dirname dir == "import" || (dirname dir == ""))
    then dir:acc
    else importDirBreakdown' (dir:acc) $ parent dir

extractImportDirs :: FilePath -> Either Text ImportDirs
extractImportDirs inputFile = do
  case importDirBreakdown inputFile of
    [bd,owner,bank,account,filestate,year] -> Right $ ImportDirs bd owner bank account filestate year
    _ -> do
      Left $ format ("I couldn't find the right number of directories between \"import\" and the input file:\n"%fp
                      %"\n\nhledger-makitso expects to find input files in this structure:\n"%
                      "import/owner/bank/account/filestate/year/trxfile\n\n"%
                      "Have a look at the documentation for a detailed explanation:\n"%s) inputFile (docURL "input-files")

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

customConstruct :: HMISOptions -> FilePath -> Line -> Line -> Line -> FilePath -> FilePath -> Shell FilePath
customConstruct opts constructScript bank account owner csvSrc journalOut = do
  let script = format fp constructScript :: Text
  let importOut = inproc script [format fp csvSrc, "-", lineToText bank, lineToText account, lineToText owner] empty
  let action = procs "hledger" ["print", "--ignore-assertions", "--file", "-", "--output-file", format fp journalOut] importOut
  let relScript = fromMaybe constructScript $ stripPrefix (directory $ baseDir opts) constructScript
  let relSrc = fromMaybe csvSrc $ stripPrefix (directory $ baseDir opts) csvSrc
  let msg = format ("executing '"%fp%"' on '"%fp%"'") relScript relSrc
  _ <- liftIO $ logVerboseTime opts msg action

  return journalOut
