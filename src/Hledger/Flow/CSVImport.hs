{-# LANGUAGE OverloadedStrings #-}

module Hledger.Flow.CSVImport
    ( importCSVs
    ) where

import qualified Turtle as Turtle hiding (stdout, stderr, proc, procStrictWithErr)
import Turtle ((%), (</>), (<.>))
import Prelude hiding (putStrLn, take)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NonEmpty
import qualified Hledger.Flow.Types as FlowTypes
import Hledger.Flow.Import.Types
import Hledger.Flow.BaseDir (turtleBaseDir, turtleRunDir, relativeToBase)
import Hledger.Flow.PathHelpers (TurtlePath, forceTrailingSlash)
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Common
import Hledger.Flow.RuntimeOptions
import Control.Concurrent.STM

importCSVs :: RuntimeOptions -> IO ()
importCSVs opts = Turtle.sh (
  do
    ch <- Turtle.liftIO newTChanIO
    logHandle <- Turtle.fork $ consoleChannelLoop ch
    Turtle.liftIO $ if (showOptions opts) then channelOutLn ch (Turtle.repr opts) else return ()
    Turtle.liftIO $ logVerbose opts ch "Starting import"
    (journals, diff) <- Turtle.time $ Turtle.liftIO $ importCSVs' opts ch
    Turtle.liftIO $ channelOutLn ch $ Turtle.format ("Imported "%Turtle.d%" journals in "%Turtle.s) (length journals) $ Turtle.repr diff
    Turtle.liftIO $ terminateChannelLoop ch
    Turtle.wait logHandle
  )

pathSeparators :: [Char]
pathSeparators = ['/', '\\', ':']

inputFilePattern :: Turtle.Pattern T.Text
inputFilePattern = Turtle.contains (Turtle.once (Turtle.oneOf pathSeparators) <> Turtle.asciiCI "1-in" <> Turtle.once (Turtle.oneOf pathSeparators) <> Turtle.plus Turtle.digit <> Turtle.once (Turtle.oneOf pathSeparators))

importCSVs' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [TurtlePath]
importCSVs' opts ch = do
  let baseImportDir = forceTrailingSlash $ (turtleBaseDir opts) </> "import"
  let runDir = forceTrailingSlash $ Turtle.collapse $ (turtleBaseDir opts) </> (turtleRunDir opts)
  let effectiveDir = if useRunDir opts
        then if (forceTrailingSlash $ runDir </> "import") == baseImportDir then baseImportDir else runDir
        else baseImportDir
  channelOutLn ch $ Turtle.format ("Collecting input files from "%Turtle.fp) effectiveDir
  (inputFiles, diff) <- Turtle.time $ Turtle.single . shellToList . onlyFiles $ Turtle.find inputFilePattern effectiveDir
  let fileCount = length inputFiles
  if (fileCount == 0) then
    do
      let msg = Turtle.format ("I couldn't find any input files underneath "%Turtle.fp
                        %"\n\nhledger-flow expects to find its input files in specifically\nnamed directories.\n\n"%
                        "Have a look at the documentation for a detailed explanation:\n"%Turtle.s) effectiveDir (docURL "input-files")
      errExit 1 ch msg []
    else
    do
      channelOutLn ch $ Turtle.format ("Found "%Turtle.d%" input files in "%Turtle.s%". Proceeding with import...") fileCount (Turtle.repr diff)
      let actions = map (extractAndImport opts ch) inputFiles :: [IO TurtlePath]
      importedJournals <- parAwareActions opts actions
      _ <- writeIncludesUpTo opts ch effectiveDir importedJournals
      _ <- writeToplevelAllYearsInclude opts
      return importedJournals

extractAndImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> IO TurtlePath
extractAndImport opts ch inputFile = do
  case extractImportDirs inputFile of
    Right importDirs -> importCSV opts ch importDirs inputFile
    Left errorMessage -> do
      errExit 1 ch errorMessage inputFile

importCSV :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs -> TurtlePath -> IO TurtlePath
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
  Turtle.mktree $ Turtle.directory journalOut
  importFun csvFile journalOut

preprocessIfNeeded :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> IO TurtlePath
preprocessIfNeeded opts ch script bank account owner src = do
  shouldPreprocess <- verboseTestFile opts ch script
  if shouldPreprocess
    then preprocess opts ch script bank account owner src
    else return src

preprocess :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> IO TurtlePath
preprocess opts ch script bank account owner src = do
  let csvOut = changePathAndExtension "2-preprocessed" "csv" src
  Turtle.mktree $ Turtle.directory csvOut
  let args = [Turtle.format Turtle.fp src, Turtle.format Turtle.fp csvOut, Turtle.lineToText bank, Turtle.lineToText account, Turtle.lineToText owner]
  let relScript = relativeToBase opts script
  let relSrc = relativeToBase opts src
  let cmdLabel = Turtle.format ("executing '"%Turtle.fp%"' on '"%Turtle.fp%"'") relScript relSrc
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
      let hledger = Turtle.format Turtle.fp $ FlowTypes.hlPath . hledgerInfo $ opts :: T.Text
      let args = ["print", "--rules-file", Turtle.format Turtle.fp rf, "--file", Turtle.format Turtle.fp csvSrc, "--output-file", Turtle.format Turtle.fp journalOut]
      let cmdLabel = Turtle.format ("importing '"%Turtle.fp%"' using rules file '"%Turtle.fp%"'") relCSV relRules
      _ <- timeAndExitOnErr opts ch cmdLabel channelOut channelErr (parAwareProc opts) (hledger, args, Turtle.empty)
      return journalOut
    Nothing ->
      do
        let relativeCandidates = map (relativeToBase opts) candidates
        let candidatesTxt = T.intercalate "\n" $ map (Turtle.format Turtle.fp) relativeCandidates
        let msg = Turtle.format ("I couldn't find an hledger rules file while trying to import\n"%Turtle.fp
                          %"\n\nI will happily use the first rules file I can find from any one of these "%Turtle.d%" files:\n"%Turtle.s
                          %"\n\nHere is a bit of documentation about rules files that you may find helpful:\n"%Turtle.s)
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
      let srcSpecificFilename = Turtle.fromText srcSuffix <.> "rules"
      map (</> srcSpecificFilename) [accountDir importDirs, bankDir importDirs, importDir importDirs]
    else []

customConstruct :: RuntimeOptions -> TChan FlowTypes.LogMessage -> TurtlePath -> Turtle.Line -> Turtle.Line -> Turtle.Line -> TurtlePath -> TurtlePath -> IO TurtlePath
customConstruct opts ch constructScript bank account owner csvSrc journalOut = do
  let script = Turtle.format Turtle.fp constructScript :: T.Text
  let relScript = relativeToBase opts constructScript
  let constructArgs = [Turtle.format Turtle.fp csvSrc, "-", Turtle.lineToText bank, Turtle.lineToText account, Turtle.lineToText owner]
  let constructCmdText = Turtle.format ("Running: "%Turtle.fp%" "%Turtle.s) relScript (showCmdArgs constructArgs)
  let stdLines = inprocWithErrFun (channelErrLn ch) (script, constructArgs, Turtle.empty)
  let hledger = Turtle.format Turtle.fp $ FlowTypes.hlPath . hledgerInfo $ opts :: T.Text
  let args = ["print", "--ignore-assertions", "--file", "-", "--output-file", Turtle.format Turtle.fp journalOut]
  let relSrc = relativeToBase opts csvSrc
  let cmdLabel = Turtle.format ("executing '"%Turtle.fp%"' on '"%Turtle.fp%"'") relScript relSrc
  _ <- timeAndExitOnErr' opts ch cmdLabel [constructCmdText] channelOut channelErr (parAwareProc opts) (hledger, args, stdLines)
  return journalOut
