{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.CSVImport
  ( importCSVs,
  )
where

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (effectiveRunDir, relativeToBase)
import Hledger.Flow.Common
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Import.ImportHelpers
import Hledger.Flow.Import.ImportHelpersTurtle (writeIncludesUpTo, writeToplevelAllYearsInclude)
import Hledger.Flow.Import.Types
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, RelDir)
import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as FlowTypes
import qualified Path
import qualified Path.IO as PathIO
import System.Environment (getEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose)
import qualified System.Info as Sys
import qualified System.Process as Process
import System.FilePath (dropExtensions, dropTrailingPathSeparator, replaceExtension, takeExtension, (<.>), (</>))
import Prelude hiding (putStrLn, take, writeFile)

type FileWasGenerated = Bool

importCSVs :: RuntimeOptions -> IO ()
importCSVs opts = withConsoleLogging $ \ch -> do
  when (showOptions opts) (channelOutLn ch (T.pack (show opts)))
  logVerbose opts ch "Starting import"
  (journals, diff) <- timeAction $ importCSVs' opts ch
  let generatedJournals = filter snd journals
  channelOutLn ch $ "Imported " <> T.pack (show (length generatedJournals)) <> "/" <> T.pack (show (length journals)) <> " journals in " <> T.pack (show diff)

importCSVs' :: RuntimeOptions -> TChan FlowTypes.LogMessage -> IO [(AbsFile, FileWasGenerated)]
importCSVs' opts ch = do
  let effectiveDir = effectiveRunDir (baseDir opts) (importRunDir opts)
  let startYearMsg = maybe " " (\y -> " (for the year " <> T.pack (show y) <> " and onwards) ") (importStartYear opts)
  let dirText = T.pack (Path.toFilePath effectiveDir)
  channelOutLn ch $ "Collecting input files" <> startYearMsg <> "from " <> dirText
  (inputFiles, diff) <- timeAction $ findInputFiles (fromMaybe 0 $ importStartYear opts) effectiveDir

  let fileCount = length inputFiles
  if fileCount == 0 && isNothing (importStartYear opts)
    then do
      let msg =
            "I couldn't find any input files underneath " <> dirText
              <> "\n\nhledger-flow expects to find its input files in specifically\nnamed directories.\n\n"
              <> "Have a look at the documentation for a detailed explanation:\n"
              <> docURL "input-files"
      errExit 1 ch msg []
    else do
      channelOutLn ch $ "Found " <> T.pack (show fileCount) <> " input files" <> startYearMsg <> "in " <> T.pack (show diff) <> ". Proceeding with import..."
      let actions = map (extractAndImport opts ch) inputFiles
      importedJournals <- parAwareActions opts actions
      let importRoot = baseDir opts Path.</> [Path.reldir|import|]
      (journalsOnDisk, journalFindTime) <- timeAction $ findJournalFiles importRoot
      (_, writeIncludeTime1) <- timeAction $ writeIncludesUpTo opts ch importRoot journalsOnDisk
      (_, writeIncludeTime2) <- timeAction $ writeToplevelAllYearsInclude opts
      let includeGenTime = journalFindTime + writeIncludeTime1 + writeIncludeTime2
      channelOutLn ch $ "Wrote include files for " <> T.pack (show (length journalsOnDisk)) <> " journals in " <> T.pack (show includeGenTime)
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
      then needsRegeneration csvFile journalOut
      else return True
  PathIO.createDirIfMissing True (Path.parent journalOut)
  if shouldImport
    then do
      hook <- findHook opts ch (accountDir importDirs Path.</> [Path.relfile|construct|])
      case hook of
        Just script -> withStagedOutput journalOut (customConstruct opts ch script importDirs csvFile)
        Nothing -> hledgerImport opts ch importDirs csvFile journalOut
    else logNewFileSkip opts ch "import" journalOut
  return (journalOut, shouldImport)

outputFile :: ImportDirs Path.Abs -> RelDir -> String -> AbsFile -> IO AbsFile
outputFile dirs stage ext source = do
  name <- Path.parseRelFile (replaceExtension (Path.toFilePath (Path.filename source)) ext)
  return (accountDir dirs Path.</> stage Path.</> Path.dirname (yearDir dirs) Path.</> name)

preprocessIfNeeded :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO (AbsFile, Bool)
preprocessIfNeeded opts ch dirs src csvOut = do
  hook <- findHook opts ch (accountDir dirs Path.</> [Path.relfile|preprocess|])
  case hook of
    Nothing -> do
      cached <- verboseTestFile opts ch csvOut
      return (if cached then csvOut else src, False)
    Just script -> do
      regenerate <- if onlyNewFiles opts then needsRegeneration src csvOut else return True
      if regenerate
        then withStagedOutput csvOut (preprocess opts ch script dirs src)
        else logNewFileSkip opts ch "preprocess" csvOut
      return (csvOut, regenerate)

findHook :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> IO (Maybe AbsFile)
findHook opts ch base = do
  alternatives <- if Sys.os == "mingw32"
    then mapM (`Path.addExtension` base) [".ps1", ".exe"]
    else return []
  first (base : alternatives)
  where
    first [] = return Nothing
    first (file : rest) = do
      exists <- verboseTestFile opts ch file
      if exists then return (Just file) else first rest

hookCommand :: AbsFile -> [T.Text] -> IO (T.Text, [T.Text])
hookCommand script args
  | Sys.os == "mingw32" && takeExtension path == ".ps1" = do
      root <- getEnv "SystemRoot"
      let shell = root </> "System32" </> "WindowsPowerShell" </> "v1.0" </> "powershell.exe"
      -- Windows PowerShell -File treats construct's literal "-" as an option.
      -- Invoke the script with literal strings and propagate its exit status.
      let command = "[Console]::OutputEncoding = New-Object Text.UTF8Encoding($false); & "
            <> T.unwords (map literal (T.pack path : args))
            <> "; if (!$?) { if ($LASTEXITCODE) { exit $LASTEXITCODE }; exit 1 }"
      return (T.pack shell, ["-NoProfile", "-NonInteractive", "-OutputFormat", "Text", "-Command", command])
  | otherwise = return (T.pack path, args)
  where
    path = Path.toFilePath script
    -- PowerShell also recognises typographic single quotes as delimiters.
    literal s = "'" <> T.concatMap escape s <> "'"
    escape c | c == '\'' || (c >= '\x2018' && c <= '\x201b') = T.replicate 2 (T.singleton c)
             | otherwise = T.singleton c

logNewFileSkip :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> AbsFile -> IO ()
logNewFileSkip opts ch logIdentifier absTarget =
  Control.Monad.when (onlyNewFiles opts) $ do
    let relativeTarget = relativeToBase opts (Path.toFilePath absTarget)
    logVerbose opts ch $
      "Skipping " <> logIdentifier <> " - output file is at least as new as source: '" <> T.pack relativeTarget <> "'"

preprocess :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
preprocess opts ch script dirs src csvOut = do
  let args = map (T.pack . Path.toFilePath) [src, csvOut] ++ hookDirArgs dirs
  let relScript = relativeToBase opts (Path.toFilePath script)
  let relSrc = relativeToBase opts (Path.toFilePath src)
  let cmdLabel = "executing '" <> T.pack relScript <> "' on '" <> T.pack relSrc <> "'"
  (cmd, hookArgs) <- hookCommand script args
  _ <- timeAndExitOnErr opts ch cmdLabel channelOut channelErr (cmd, hookArgs, Nothing)
  return ()

hledgerImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
hledgerImport opts ch importDirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  candidates <- rulesFileCandidates csvFile importDirs
  maybeRulesFile <- firstExistingFile candidates
  let relCSV = relativeToBase opts csvSrc
  case maybeRulesFile of
    Just rf -> do
      let relRules = relativeToBase opts (Path.toFilePath rf)
      let hledger = T.pack (Path.toFilePath (FlowTypes.hlPath (hledgerInfo opts)))
      let directives = directivesFile opts
      directivesExist <- PathIO.doesFileExist directives
      let directivesArgs = if directivesExist then ["--file", T.pack (Path.toFilePath directives)] else []
      let csvArgs = ["--file", T.pack csvSrc, "--rules-file", T.pack (Path.toFilePath rf)]
      let args = hledgerConfArgs opts ++ ["print", "--explicit"] ++ directivesArgs ++ csvArgs

      let cmdLabel = "importing '" <> T.pack relCSV <> "' using rules file '" <> T.pack relRules <> "'"
      ((_, stdOut, _), _) <- timeAndExitOnErr opts ch cmdLabel dummyLogger channelErr (hledger, args, Nothing)
      T.writeFile (Path.toFilePath journalOut) stdOut
    Nothing ->
      do
        let candidatesTxt = T.intercalate "\n" $ map (T.pack . relativeToBase opts . Path.toFilePath) candidates
        let msg =
              "I couldn't find an hledger rules file while trying to import\n" <> T.pack relCSV
                <> "\n\nI will happily use the first rules file I can find from any one of these "
                <> T.pack (show (length candidates)) <> " files:\n" <> candidatesTxt
                <> "\n\nHere is a bit of documentation about rules files that you may find helpful:\n"
                <> docURL "rules-files"
        errExit 1 ch msg ()

rulesFileCandidates :: AbsFile -> ImportDirs Path.Abs -> IO [AbsFile]
rulesFileCandidates csv dirs = do
  let bank = importDirLines (bankDir dirs)
      account = importDirLines (accountDir dirs)
  accountRule <- rulesName (bank ++ account)
  bankRule <- rulesName bank
  overrides <- if "rfo" `T.isPrefixOf` suffix
    then do
      name <- rulesName [suffix]
      return [dir Path.</> name | dir <- [accountDir dirs, bankDir dirs, importDir dirs]]
    else return []
  return (overrides ++ [accountDir dirs Path.</> accountRule, importDir dirs Path.</> bankRule])
  where
    stem = dropExtensions (Path.toFilePath (Path.filename csv))
    suffix = snd (T.breakOnEnd "_" (T.pack stem))
    rulesName parts = Path.parseRelFile (T.unpack (T.intercalate "-" parts) <.> "rules")

importDirLines :: Path.Path b Path.Dir -> [T.Text]
importDirLines = T.splitOn "\n" . T.pack . dropTrailingPathSeparator . Path.toFilePath . Path.dirname

hookDirArgs :: ImportDirs b -> [T.Text]
hookDirArgs dirs = map (T.concat . importDirLines) [bankDir dirs, accountDir dirs, ownerDir dirs]

customConstruct :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
customConstruct opts ch constructScript dirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  let relScript = relativeToBase opts (Path.toFilePath constructScript)
  let constructArgs = [T.pack csvSrc, "-"] ++ hookDirArgs dirs
  let constructCmdText = "Running: " <> T.pack relScript <> " " <> showCmdArgs constructArgs
  (cmd, hookArgs) <- hookCommand constructScript constructArgs
  let hook = (Process.proc (T.unpack cmd) (map T.unpack hookArgs))
        { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe }
  let hledger = T.pack (Path.toFilePath (FlowTypes.hlPath (hledgerInfo opts)))
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
  let cmdLabel = "executing '" <> T.pack relScript <> "' on '" <> T.pack relSrc <> "'"
  Process.withCreateProcess hook $ \input output _ handle -> do
    Just stdinPipe <- return input
    Just stdoutPipe <- return output
    hClose stdinPipe
    _ <- timeAndExitOnErr' opts ch cmdLabel [constructCmdText] channelOut channelErr
      (hledger, args, Just stdoutPipe)
    hClose stdoutPipe
    status <- Process.waitForProcess handle
    case status of
      ExitSuccess -> return ()
      ExitFailure _ -> exitWith status
