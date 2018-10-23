{-# LANGUAGE OverloadedStrings #-}

module CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn, take)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import Common

data ImportDirs = ImportDirs { importDir  :: FilePath
                             , ownerDir   :: FilePath
                             , bankDir    :: FilePath
                             , accountDir :: FilePath
                             , stateDir   :: FilePath
                             , yearDir    :: FilePath
                             } deriving (Show)

importCSVs :: FilePath -> IO ()
importCSVs baseDir = do
  let importBase = baseDir </> "import"
  importExists <- testdir importBase
  if importExists
    then
    do
      let importedJournals = importOwners $ lsDirs importBase
      sh $ writeMakeItSoJournal baseDir importedJournals
    else
    do
      let msg = format ("I couldn't find a directory named \"import\" underneath "%fp
                        %"\n\nhledger-makitso expects to find its input files in specifically\nnamed directories.\n\n"%
                        "Have a look at the documentation for a detailed explanation:\n"%s) baseDir (docURL "input-files")
      stderr $ select $ textToLines msg
      exit $ ExitFailure 1

importOwners :: Shell FilePath -> Shell FilePath
importOwners ownerDirs = do
  od <- ownerDirs
  ownerName <- basenameLine od
  let ownerJournals = importBanks ownerName $ lsDirs od
  let aggregateJournal = od </> buildFilename [ownerName] "journal"
  writeJournals aggregateJournal ownerJournals
  return aggregateJournal

importBanks :: Line -> Shell FilePath -> Shell FilePath
importBanks _ownerName bankDirs = do
  bd <- bankDirs
  bankName <- basenameLine bd
  let bankJournals = importAccounts bankName $ lsDirs bd
  let aggregateJournal = bd </> buildFilename [bankName] "journal"
  writeJournals aggregateJournal bankJournals
  return aggregateJournal

importAccounts :: Line -> Shell FilePath -> Shell FilePath
importAccounts bankName accountDirs = do
  accDir <- accountDirs
  accName <- basenameLine accDir
  let preprocessScript = accDir </> "preprocess"
  let constructScript = accDir </> "construct"
  let accountSrcFiles = onlyFiles $ find (has (text "1-in")) accDir
  let accJournals = importAccountFiles bankName accName preprocessScript constructScript accountSrcFiles
  let yearJournals = writeIncludeFiles accJournals
  let aggregateJournal = accDir </> buildFilename [bankName, accName] "journal"
  let openingJournal = accDir </> "opening.journal"
  liftIO $ touch openingJournal
  writeJournals aggregateJournal $ (return openingJournal) + yearJournals
  return aggregateJournal

importAccountFiles :: Line -> Line -> FilePath -> FilePath -> Shell FilePath -> Shell FilePath
importAccountFiles bankName accountName preprocessScript constructScript accountSrcFiles = do
  srcFile <- accountSrcFiles
  csvFile <- preprocessIfNeeded preprocessScript bankName accountName srcFile
  doCustomConstruct <- testfile constructScript
  let importFun = if doCustomConstruct
        then customConstruct constructScript bankName accountName
        else hledgerImport
  let journalOut = changePathAndExtension "3-journal" "journal" csvFile
  mktree $ directory journalOut
  importFun csvFile journalOut

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
  procs script' [format fp src, format fp csvOut, lineToText bank, lineToText account] empty
  return csvOut

hledgerImport :: FilePath  -> FilePath -> Shell FilePath
hledgerImport csvSrc journalOut = do
  case extractImportDirs csvSrc of
    Right importDirs -> hledgerImport' importDirs csvSrc journalOut
    Left errorMessage -> do
      stderr $ select $ textToLines errorMessage
      exit $ ExitFailure 1

hledgerImport' :: ImportDirs -> FilePath -> FilePath -> Shell FilePath
hledgerImport' importDirs csvSrc journalOut = do
  let candidates = rulesFileCandidates csvSrc importDirs
  maybeRulesFile <- firstExistingFile candidates
  case maybeRulesFile of
    Just rf -> do
      procs "hledger" ["print", "--rules-file", format fp rf, "--file", format fp csvSrc, "--output-file", format fp journalOut] empty
      return journalOut
    Nothing ->
      do
        let candidatesTxt = T.intercalate "\n" $ map (format fp) candidates
        let msg = format ("I couldn't find an hledger rules file while trying to import\n"%fp
                          %"\n\nI will happily use the first rules file I can find from any one of these "%d%" files:\n"%s
                          %"\n\nHere is a bit of documentation about rules files that you may find helpful:\n"%s)
                  csvSrc (length candidates) candidatesTxt (docURL "rules-files")
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
    [baseDir,owner,bank,account,filestate,year] -> Right $ ImportDirs baseDir owner bank account filestate year
    _ -> do
      Left $ format ("I couldn't find the right number of directories between \"import\" and the input file:\n"%fp
                      %"\n\nhledger-makitso expects to find input files in this structure:\n"%
                      "import/owner/bank/account/filestate/year/trxfile\n\n"%
                      "Have a look at the documentation for a detailed explanation:\n"%s) inputFile (docURL "input-files")

rulesFileCandidates :: FilePath -> ImportDirs -> [FilePath]
rulesFileCandidates csvSrc importDirs = statementSpecificRulesFiles csvSrc importDirs ++ generalRulesFiles importDirs

importDirLines :: (ImportDirs -> FilePath) -> ImportDirs -> [Line]
importDirLines dirFun importDirs = NonEmpty.toList $ textToLines $ format fp $ dirname $ dirFun importDirs

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

customConstruct :: FilePath -> Line -> Line -> FilePath -> FilePath -> Shell FilePath
customConstruct constructScript bank account csvSrc journalOut = do
  let script = format fp constructScript :: Text
  let importOut = inproc script [format fp csvSrc, "-", lineToText bank, lineToText account] empty
  procs "hledger" ["print", "--ignore-assertions", "--file", "-", "--output-file", format fp journalOut] importOut
  return journalOut

changePathAndExtension :: FilePath -> Text -> FilePath -> FilePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeExtension :: Text -> FilePath -> FilePath
changeExtension ext path = (dropExtension path) <.> ext

changeOutputPath :: FilePath -> FilePath -> FilePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ splitDirectories srcFile
  where changeSrcDir file = if (file == "1-in/" || file == "2-preprocessed/") then newOutputLocation else file
