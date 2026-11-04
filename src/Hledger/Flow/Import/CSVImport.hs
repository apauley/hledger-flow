{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.CSVImport
  ( importCSVs,
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.BaseDir (effectiveRunDir, relativeToBase)
import Hledger.Flow.Common
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.ImportHelpers
import Hledger.Flow.Import.ImportHelpersTurtle (writeIncludesUpTo, writeToplevelAllYearsInclude)
import Hledger.Flow.Import.Types
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, RelDir, TurtlePath, pathToTurtle)
import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as FlowTypes
import qualified Path
import qualified Path.IO as PathIO
import System.FilePath (dropTrailingPathSeparator, replaceExtension)
import Turtle ((%), (<.>), (</>))
import qualified Turtle hiding (proc, procStrictWithErr, stderr, stdout)
import Prelude hiding (putStrLn, take, writeFile)

type FileWasGenerated = Bool

importCSVs :: RuntimeOptions -> IO ()
importCSVs opts =
  Turtle.sh
    ( do
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

importCSVs' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [(AbsFile, FileWasGenerated)]
importCSVs' opts ch = do
  let effectiveDir = effectiveRunDir (baseDir opts) (importRunDir opts)
  let startYearMsg = maybe " " (Turtle.format (" (for the year " % Turtle.d % " and onwards) ")) (importStartYear opts)
  channelOutLn ch $ Turtle.format ("Collecting input files" % Turtle.s % "from " % Turtle.fp) startYearMsg (pathToTurtle effectiveDir)
  (inputFiles, diff) <- Turtle.time $ findInputFiles (fromMaybe 0 $ importStartYear opts) effectiveDir

  let fileCount = length inputFiles
  if fileCount == 0 && isNothing (importStartYear opts)
    then do
      let msg =
            Turtle.format
              ( "I couldn't find any input files underneath "
                  % Turtle.fp
                  % "\n\nhledger-flow expects to find its input files in specifically\nnamed directories.\n\n"
                  % "Have a look at the documentation for a detailed explanation:\n"
                  % Turtle.s
              )
              (pathToTurtle effectiveDir)
              (docURL "input-files")
      errExit 1 ch msg []
    else do
      channelOutLn ch $ Turtle.format ("Found " % Turtle.d % " input files" % Turtle.s % "in " % Turtle.s % ". Proceeding with import...") fileCount startYearMsg (Turtle.repr diff)
      let actions = map (extractAndImport opts ch) inputFiles
      importedJournals <- parAwareActions opts actions
      (journalsOnDisk, journalFindTime) <- Turtle.time $ findJournalFiles effectiveDir
      (_, writeIncludeTime1) <- Turtle.time $ writeIncludesUpTo opts ch (pathToTurtle effectiveDir) $ fmap pathToTurtle journalsOnDisk
      (_, writeIncludeTime2) <- Turtle.time $ writeToplevelAllYearsInclude opts
      let includeGenTime = journalFindTime + writeIncludeTime1 + writeIncludeTime2
      channelOutLn ch $ Turtle.format ("Wrote include files for " % Turtle.d % " journals in " % Turtle.s) (length journalsOnDisk) (Turtle.repr includeGenTime)
      return importedJournals

extractAndImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> IO (AbsFile, FileWasGenerated)
extractAndImport opts ch inputFile = do
  case extractImportDirs inputFile of
    Right importDirs -> importCSV opts ch importDirs inputFile
    Left errorMessage -> do
      errExit 1 ch errorMessage (inputFile, False)

importCSV :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> IO (AbsFile, FileWasGenerated)
importCSV opts ch importDirs srcFile = do
  csvOut <- outputFile importDirs [Path.reldir|2-preprocessed|] ".csv" srcFile
  (csvFile, preprocessHappened) <- preprocessIfNeeded opts ch importDirs srcFile csvOut
  journalOut <- outputFile importDirs [Path.reldir|3-journal|] ".journal" csvFile
  shouldImport <-
    if onlyNewFiles opts && not preprocessHappened
      then needsRegeneration (Path.toFilePath csvFile) (Path.toFilePath journalOut)
      else return True
  PathIO.createDirIfMissing True (Path.parent journalOut)
  if shouldImport
    then do
      let script = accountDir importDirs Path.</> [Path.relfile|construct|]
      hasConstruct <- verboseTestFile opts ch (Path.toFilePath script)
      if hasConstruct
        then customConstruct opts ch script importDirs csvFile journalOut
        else hledgerImport opts ch importDirs csvFile journalOut
    else logNewFileSkip opts ch "import" journalOut
  return (journalOut, shouldImport)

outputFile :: ImportDirs Path.Abs -> RelDir -> String -> AbsFile -> IO AbsFile
outputFile dirs stage ext source = do
  name <- Path.parseRelFile (replaceExtension (Path.toFilePath (Path.filename source)) ext)
  return (accountDir dirs Path.</> stage Path.</> Path.dirname (yearDir dirs) Path.</> name)

preprocessIfNeeded :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO (AbsFile, Bool)
preprocessIfNeeded opts ch dirs src csvOut = do
  let script = accountDir dirs Path.</> [Path.relfile|preprocess|]
  scriptExists <- verboseTestFile opts ch (Path.toFilePath script)
  targetExists <- verboseTestFile opts ch (Path.toFilePath csvOut)
  shouldProceed <-
    if onlyNewFiles opts && scriptExists
      then do
        needsRegen <- needsRegeneration (Path.toFilePath src) (Path.toFilePath csvOut)
        Control.Monad.unless needsRegen $ logNewFileSkip opts ch "preprocess" csvOut
        return needsRegen
      else return scriptExists
  if shouldProceed
    then do
      preprocess opts ch script dirs src csvOut
      return (csvOut, True)
    else do
      if targetExists
        then return (csvOut, False)
        else return (src, False)

logNewFileSkip :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> AbsFile -> IO ()
logNewFileSkip opts ch logIdentifier absTarget =
  Control.Monad.when (onlyNewFiles opts) $ do
    let relativeTarget = relativeToBase opts (Path.toFilePath absTarget)
    logVerbose opts ch $
      Turtle.format
        ( "Skipping "
            % Turtle.s
            % " - output file is at least as new as source: '"
            % Turtle.fp
            % "'"
        )
        logIdentifier
        relativeTarget

preprocess :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
preprocess opts ch script dirs src csvOut = do
  PathIO.createDirIfMissing True (Path.parent csvOut)
  let args = map (T.pack . Path.toFilePath) [src, csvOut] ++ hookDirArgs dirs
  let relScript = relativeToBase opts (Path.toFilePath script)
  let relSrc = relativeToBase opts (Path.toFilePath src)
  let cmdLabel = Turtle.format ("executing '" % Turtle.fp % "' on '" % Turtle.fp % "'") relScript relSrc
  _ <- timeAndExitOnErr opts ch cmdLabel channelOut channelErr (parAwareProc opts) (T.pack (Path.toFilePath script), args, Turtle.empty)
  return ()

hledgerImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
hledgerImport opts ch importDirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  let candidates = rulesFileCandidates csvSrc importDirs
  maybeRulesFile <- firstExistingFile candidates
  let relCSV = relativeToBase opts csvSrc
  case maybeRulesFile of
    Just rf -> do
      let relRules = relativeToBase opts rf
      let hledger = Turtle.format Turtle.fp $ pathToTurtle . FlowTypes.hlPath . hledgerInfo $ opts :: T.Text
      directivesExist <- Turtle.testfile $ directivesFile opts
      let directivesArgs = if directivesExist then ["--file", Turtle.format Turtle.fp (directivesFile opts)] else []
      let csvArgs = ["--file", Turtle.format Turtle.fp csvSrc, "--rules-file", Turtle.format Turtle.fp rf]
      let args = hledgerConfArgs opts ++ ["print", "--explicit"] ++ directivesArgs ++ csvArgs

      let cmdLabel = Turtle.format ("importing '" % Turtle.fp % "' using rules file '" % Turtle.fp % "'") relCSV relRules
      ((_, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr (parAwareProc opts) (hledger, args, Turtle.empty)
      T.writeFile (Path.toFilePath journalOut) stdOut
    Nothing ->
      do
        let relativeCandidates = map (relativeToBase opts) candidates
        let candidatesTxt = T.intercalate "\n" $ map (Turtle.format Turtle.fp) relativeCandidates
        let msg =
              Turtle.format
                ( "I couldn't find an hledger rules file while trying to import\n"
                    % Turtle.fp
                    % "\n\nI will happily use the first rules file I can find from any one of these "
                    % Turtle.d
                    % " files:\n"
                    % Turtle.s
                    % "\n\nHere is a bit of documentation about rules files that you may find helpful:\n"
                    % Turtle.s
                )
                relCSV
                (length candidates)
                candidatesTxt
                (docURL "rules-files")
        errExit 1 ch msg ()

rulesFileCandidates :: TurtlePath -> ImportDirs b -> [TurtlePath]
rulesFileCandidates csvSrc importDirs = statementSpecificRulesFiles csvSrc importDirs ++ generalRulesFiles importDirs

importDirLines :: Path.Path b Path.Dir -> [Turtle.Line]
importDirLines = NonEmpty.toList . Turtle.textToLines . T.pack . dropTrailingPathSeparator . Path.toFilePath . Path.dirname

hookDirArgs :: ImportDirs b -> [T.Text]
hookDirArgs dirs = map (Turtle.lineToText . foldl (<>) "" . importDirLines) [bankDir dirs, accountDir dirs, ownerDir dirs]

generalRulesFiles :: ImportDirs b -> [TurtlePath]
generalRulesFiles importDirs = do
  let bank = importDirLines (bankDir importDirs)
  let account = importDirLines (accountDir importDirs)
  let accountRulesFile = pathToTurtle (accountDir importDirs) </> buildFilename (bank ++ account) "rules"

  let bankRulesFile = pathToTurtle (importDir importDirs) </> buildFilename bank "rules"
  [accountRulesFile, bankRulesFile]

statementSpecificRulesFiles :: TurtlePath -> ImportDirs b -> [TurtlePath]
statementSpecificRulesFiles csvSrc importDirs = do
  let srcSuffix = snd $ T.breakOnEnd "_" (Turtle.format Turtle.fp (Turtle.basename csvSrc))

  if ((T.take 3 srcSuffix) == "rfo")
    then do
      let srcSpecificFilename = T.unpack srcSuffix <.> "rules"
      map ((</> srcSpecificFilename) . pathToTurtle) [accountDir importDirs, bankDir importDirs, importDir importDirs]
    else []

customConstruct :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
customConstruct opts ch constructScript dirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  let script = T.pack (Path.toFilePath constructScript)
  let relScript = relativeToBase opts (Path.toFilePath constructScript)
  let constructArgs = [T.pack csvSrc, "-"] ++ hookDirArgs dirs
  let constructCmdText = Turtle.format ("Running: " % Turtle.fp % " " % Turtle.s) relScript (showCmdArgs constructArgs)
  let stdLines = inprocWithErrFun (channelErrLn ch) (script, constructArgs, Turtle.empty)
  let hledger = Turtle.format Turtle.fp $ pathToTurtle . FlowTypes.hlPath . hledgerInfo $ opts :: T.Text
  let args =
        hledgerConfArgs opts
          ++ [ "print",
               "--ignore-assertions",
               "--file",
               "-",
               "--output-file",
               T.pack (Path.toFilePath journalOut)
             ]

  let relSrc = relativeToBase opts csvSrc
  let cmdLabel = Turtle.format ("executing '" % Turtle.fp % "' on '" % Turtle.fp % "'") relScript relSrc
  _ <- timeAndExitOnErr' opts ch cmdLabel [constructCmdText] channelOut channelErr (parAwareProc opts) (hledger, args, stdLines)
  return ()
