{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.Import.CSVImport
    ( importCSVs
    ) where

import qualified Turtle hiding (stdout, stderr, proc, procStrictWithErr)
import Turtle ((%), (</>), (<.>))
import Prelude hiding (putStrLn, take, writeFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List.NonEmpty as NonEmpty
import qualified Hledger.Flow.Types as FlowTypes
import Hledger.Flow.Import.Types
import Hledger.Flow.BaseDir (relativeToBase, effectiveRunDir)
import Hledger.Flow.Import.ImportHelpers
import Hledger.Flow.Import.ImportHelpersTurtle (extractImportDirs, writeIncludesUpTo, writeToplevelAllYearsInclude)
import Hledger.Flow.PathHelpers (TurtlePath, pathToTurtle)
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Common
import Hledger.Flow.Logging
import Hledger.Flow.RuntimeOptions
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe, isNothing)
import Turtle (format, procStrict, empty, fp, s)

type FileWasGenerated = Bool

importCSVs :: RuntimeOptions -> IO ()
importCSVs opts = Turtle.sh (
  do
    ch <- Turtle.liftIO newTChanIO
    logHandle <- Turtle.fork $ consoleChannelLoop ch
    Turtle.liftIO $ when (showOptions opts) (channelOutLn ch (Turtle.repr opts))
    Turtle.liftIO $ logVerbose opts ch "Starting import"
    (journals, diff) <- Turtle.time $ Turtle.liftIO $ importCSVs' opts ch
    let generatedJournals = filter snd journals
    Turtle.liftIO $ channelOutLn ch $ Turtle.format ("Imported " % Turtle.d % "/" % Turtle.d % " journals in " % Turtle.s) (length generatedJournals) (length journals) $ Turtle.repr diff
    Turtle.liftIO $ terminateChannelLoop ch
    Turtle.wait logHandle
  )

importCSVs' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [(TurtlePath, FileWasGenerated)]
importCSVs' opts ch = do
  let effectiveDir = effectiveRunDir (baseDir opts) (importRunDir opts)
  let startYearMsg = maybe " " (Turtle.format (" (for the year " % Turtle.d % " and onwards) ")) (importStartYear opts)
  channelOutLn ch $ Turtle.format ("Collecting input files" % Turtle.s % "from "%Turtle.fp) startYearMsg (pathToTurtle effectiveDir)
  (inputFiles, diff) <- Turtle.time $ findInputFiles (fromMaybe 0 $ importStartYear opts) effectiveDir

  let fileCount = length inputFiles
  if fileCount == 0 && isNothing (importStartYear opts) then
    do
      let msg = Turtle.format ("I couldn't find any input files underneath " % Turtle.fp
                        % "\n\nhledger-flow expects to find its input files in specifically\nnamed directories.\n\n" %
                        "Have a look at the documentation for a detailed explanation:\n" % Turtle.s) (pathToTurtle effectiveDir) (docURL "input-files")
      errExit 1 ch msg []
    else
    do
      channelOutLn ch $ Turtle.format ("Found " % Turtle.d % " input files" % Turtle.s % "in " % Turtle.s % ". Proceeding with import...") fileCount startYearMsg (Turtle.repr diff)
      let actions = map (extractAndImport opts ch . pathToTurtle) inputFiles :: [IO (TurtlePath, FileWasGenerated)]
      importedJournals <- parAwareActions opts actions
      (journalsOnDisk, journalFindTime) <- Turtle.time $ findJournalFiles effectiveDir
      (_, writeIncludeTime1) <- Turtle.time $ writeIncludesUpTo opts ch (pathToTurtle effectiveDir) $ fmap pathToTurtle journalsOnDisk
      (_, writeIncludeTime2) <- Turtle.time $ writeToplevelAllYearsInclude opts
      let includeGenTime = journalFindTime + writeIncludeTime1 + writeIncludeTime2
      channelOutLn ch $ Turtle.format ("Wrote include files for " % Turtle.d % " journals in " % Turtle.s) (length journalsOnDisk) (Turtle.repr includeGenTime)
      return importedJournals

extractAndImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> IO (TurtlePath, FileWasGenerated)
extractAndImport opts ch inputFile = do
  case extractImportDirs inputFile of
    Right importDirs -> importCSV opts ch importDirs inputFile
    Left errorMessage -> do
      errExit 1 ch errorMessage (inputFile, False)

importCSV :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs -> TurtlePath -> IO (TurtlePath, FileWasGenerated)
importCSV opts ch importDirs srcFile = do
  let preprocessScript = accountDir importDirs </> "preprocess"
  let createRulesScript = accountDir importDirs </> "createRules"
  let constructScript = accountDir importDirs </> "construct"
  let bankName = importDirLine bankDir importDirs
  let accountName = importDirLine accountDir importDirs
  let ownerName = importDirLine ownerDir importDirs

  (csvFile, preprocessHappened) <- preprocessIfNeeded opts ch preprocessScript createRulesScript bankName accountName ownerName srcFile

  let journalOut = changePathAndExtension "3-journal/" "journal" csvFile
  shouldImport <- if onlyNewFiles opts && not preprocessHappened
    then not <$> verboseTestFile opts ch journalOut
    else return True

  importFun <- if shouldImport
    then constructOrImport opts ch constructScript bankName accountName ownerName
    else do
      _ <- logNewFileSkip opts ch "import" journalOut
      return $ \_p1 _p2 -> return journalOut
  Turtle.mktree $ Turtle.directory journalOut
  out <- importFun csvFile journalOut
  return (out, shouldImport)

constructOrImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> IO (TurtlePath -> TurtlePath -> IO TurtlePath)
constructOrImport opts ch constructScript bankName accountName ownerName = do
  constructScriptExists <- verboseTestFile opts ch constructScript
  if constructScriptExists
    then return $ customConstruct opts ch constructScript bankName accountName ownerName
    else return $ hledgerImport opts ch

preprocessIfNeeded :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> IO (TurtlePath, Bool)
preprocessIfNeeded opts ch preprocessScript createRulesScript bank account owner src = do
  let csvOut = changePathAndExtension "2-preprocessed/" "csv" src

  scriptExists <- verboseTestFile opts ch preprocessScript
  rulesCreated <- createRules opts ch createRulesScript bank account owner src
  targetExists <- verboseTestFile opts ch csvOut

  shouldProceed <- if onlyNewFiles opts 
    then return $ scriptExists && rulesCreated && not targetExists
    else return $ scriptExists && rulesCreated

  if shouldProceed
    then do
      out <- preprocess opts ch preprocessScript bank account owner src csvOut
      return (out, True)
    else do
      _ <- logNewFileSkip opts ch "preprocess" csvOut
      if targetExists
        then return (csvOut, False)
        else return (src, False)

logNewFileSkip :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> TurtlePath -> IO ()
logNewFileSkip opts ch logIdentifier absTarget =
  Control.Monad.when (onlyNewFiles opts) $ do
   let relativeTarget = relativeToBase opts absTarget
   logVerbose opts ch
     $ Turtle.format
        ("Skipping " % Turtle.s
         % " - only creating new files and this output file already exists: '"
         % Turtle.fp
         % "'") logIdentifier relativeTarget

createRules :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> IO Bool
createRules opts ch createRulesScript bank account owner src = do

  scriptExists <- verboseTestFile opts ch createRulesScript
  if scriptExists
    then do
      channelOutLn ch $ format ("Creating rules using: "%fp%".") (createRulesScript)
      _ <- runCreateRulesScript opts ch createRulesScript bank account owner src
      return True
    else do
      channelOutLn ch $ format ("Not creating rules, because createRulescript: "%fp%" was not found,") (createRulesScript)
      return False

runCreateRulesScript :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> IO ()
runCreateRulesScript _ _ createRulesScript bank account owner src = do
  let args = [ Turtle.format Turtle.fp src
             , Turtle.lineToText bank
             , Turtle.lineToText account
             , Turtle.lineToText owner ]

  -- Run the script with the formatted arguments
  void $ procStrict (format fp createRulesScript) args empty

preprocess :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> TurtlePath -> IO TurtlePath
preprocess opts ch script bank account owner src csvOut = do
  Turtle.mktree $ Turtle.directory csvOut
  let args = [Turtle.format Turtle.fp src, Turtle.format Turtle.fp csvOut, Turtle.lineToText bank, Turtle.lineToText account, Turtle.lineToText owner]
  let relScript = relativeToBase opts script
  let relSrc = relativeToBase opts src
  let cmdLabel = Turtle.format ("executing '" % Turtle.fp % "' on '" % Turtle.fp % "'") relScript relSrc
  _ <- timeAndExitOnErr opts ch cmdLabel channelOut channelErr (parAwareProc opts) (Turtle.format Turtle.fp script, args, Turtle.empty)
  return csvOut

hledgerImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> TurtlePath -> IO TurtlePath
hledgerImport opts ch csvSrc journalOut = do
  case extractImportDirs csvSrc of
    Right importDirs -> hledgerImport' opts ch importDirs csvSrc journalOut
    Left errorMessage -> do
      errExit 1 ch errorMessage csvSrc

hledgerImport' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs -> TurtlePath -> TurtlePath -> IO TurtlePath
hledgerImport' opts ch importDirs csvSrc journalOut = do
  let candidates = rulesFileCandidates csvSrc importDirs
  maybeRulesFile <- firstExistingFile candidates
  let relCSV = relativeToBase opts csvSrc
  case maybeRulesFile of
    Just rf -> do
      let relRules = relativeToBase opts rf
      let hledger = Turtle.format Turtle.fp $ pathToTurtle . FlowTypes.hlPath . hledgerInfo $ opts :: T.Text
      let args = [
            "import", "--dry-run",
            "--file", Turtle.format Turtle.fp (directivesFile opts),
            Turtle.format Turtle.fp csvSrc,
            "--rules-file", Turtle.format Turtle.fp rf
            ]

      let cmdLabel = Turtle.format ("importing '" % Turtle.fp % "' using rules file '" % Turtle.fp % "'") relCSV relRules
      ((_, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr (parAwareProc opts) (hledger, args, Turtle.empty)
      let withoutDryRunText = T.unlines $ drop 2 $ T.lines stdOut
      _ <- T.writeFile journalOut withoutDryRunText
      return journalOut
    Nothing ->
      do
        let relativeCandidates = map (relativeToBase opts) candidates
        let candidatesTxt = T.intercalate "\n" $ map (Turtle.format Turtle.fp) relativeCandidates
        let msg = Turtle.format ("I couldn't find an hledger rules file while trying to import\n" % Turtle.fp
                          % "\n\nI will happily use the first rules file I can find from any one of these " % Turtle.d % " files:\n" % Turtle.s
                          % "\n\nHere is a bit of documentation about rules files that you may find helpful:\n" % Turtle.s)
                  relCSV (length candidates) candidatesTxt (docURL "rules-files")
        errExit 1 ch msg csvSrc

rulesFileCandidates :: TurtlePath -> ImportDirs -> [TurtlePath]
rulesFileCandidates csvSrc importDirs = statementSpecificRulesFiles csvSrc importDirs ++ generalRulesFiles importDirs

importDirLines :: (ImportDirs -> TurtlePath) -> ImportDirs -> [Turtle.Line]
importDirLines dirFun importDirs = NonEmpty.toList $ Turtle.textToLines $ Turtle.format Turtle.fp $ Turtle.dirname $ dirFun importDirs

importDirLine :: (ImportDirs -> TurtlePath) -> ImportDirs -> Turtle.Line
importDirLine dirFun importDirs = foldl (<>) "" $ importDirLines dirFun importDirs

generalRulesFiles :: ImportDirs -> [TurtlePath]
generalRulesFiles importDirs = do
  let bank = importDirLines bankDir importDirs
  let account = importDirLines accountDir importDirs
  let accountRulesFile = accountDir importDirs </> buildFilename (bank ++ account) "rules"

  let bankRulesFile = importDir importDirs </> buildFilename bank "rules"
  [accountRulesFile, bankRulesFile]

statementSpecificRulesFiles :: TurtlePath -> ImportDirs -> [TurtlePath]
statementSpecificRulesFiles csvSrc importDirs = do
  let srcSuffix = snd $ T.breakOnEnd "_" (Turtle.format Turtle.fp (Turtle.basename csvSrc))

  if ((T.take 3 srcSuffix) == "rfo")
    then
    do
      let srcSpecificFilename = T.unpack srcSuffix <.> "rules"
      map (</> srcSpecificFilename) [accountDir importDirs, bankDir importDirs, importDir importDirs]
    else []

customConstruct :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> TurtlePath -> IO TurtlePath
customConstruct opts ch constructScript bank account owner csvSrc journalOut = do
  let script = Turtle.format Turtle.fp constructScript :: T.Text
  let relScript = relativeToBase opts constructScript
  let constructArgs = [Turtle.format Turtle.fp csvSrc, "-", Turtle.lineToText bank, Turtle.lineToText account, Turtle.lineToText owner]
  let constructCmdText = Turtle.format ("Running: " % Turtle.fp % " " % Turtle.s) relScript (showCmdArgs constructArgs)
  let stdLines = inprocWithErrFun (channelErrLn ch) (script, constructArgs, Turtle.empty)
  let hledger = Turtle.format Turtle.fp $ pathToTurtle . FlowTypes.hlPath . hledgerInfo $ opts :: T.Text
  let args = [
        "print", "--ignore-assertions",
        "--file", "-",
        "--output-file", Turtle.format Turtle.fp journalOut
        ]

  let relSrc = relativeToBase opts csvSrc
  let cmdLabel = Turtle.format ("executing '" % Turtle.fp % "' on '" % Turtle.fp % "'") relScript relSrc
  _ <- timeAndExitOnErr' opts ch cmdLabel [constructCmdText] channelOut channelErr (parAwareProc opts) (hledger, args, stdLines)
  return journalOut
