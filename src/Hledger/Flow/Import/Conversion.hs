{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Hledger.Flow.Import.Conversion
  ( findHook, preprocess, savedInput, logNewFileSkip, hledgerImport, customConstruct
  , hookDirArgs, RulesPaths(..), convertRules, constructWith
  ) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T
import Hledger.Flow.DateTime (timeAction)
import Hledger.Flow.BaseDir (relativeToBase)
import Hledger.Flow.Common
import Hledger.Flow.DocHelpers (DocLink (..), docURL)
import Hledger.Flow.Import.ImportHelpers
import Hledger.Flow.Import.Types
import Hledger.Flow.Logging
import Hledger.Flow.PathHelpers (AbsFile, isGlobMetacharacter)
import Hledger.Flow.RuntimeOptions
import qualified Hledger.Flow.Types as FlowTypes
import qualified Path
import qualified Path.IO as PathIO
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import Control.Concurrent.Async (concurrently)
import qualified Data.Text.IO as T
import System.IO (hClose, stderr)
import qualified System.Info as Sys
import qualified System.Process as Process
import System.FilePath (takeExtension, (</>))
import Prelude hiding (putStrLn, take, writeFile)

findHook :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> AbsFile -> IO (Maybe AbsFile)
findHook opts ch what base = do
  alternatives <- if Sys.os == "mingw32"
    then mapM (`Path.addExtension` base) [".ps1", ".exe"]
    else return []
  findFirstFile opts ch what (base : alternatives)

hookCommand :: AbsFile -> [T.Text] -> IO (T.Text, [T.Text])
hookCommand script args
  | Sys.os == "mingw32" && takeExtension path == ".ps1" = do
      root <- getEnv "SystemRoot"
      let shell = root </> "System32" </> "WindowsPowerShell" </> "v1.0" </> "powershell.exe"
      -- Windows PowerShell -File treats construct's literal "-" as an option.
      -- Invoke the script with literal strings and propagate its exit status.
      let command = "[Console]::OutputEncoding = New-Object Text.UTF8Encoding($false); & "
            <> T.unwords (map powerShellLiteral (T.pack path : args))
            <> "; if (!$?) { if ($LASTEXITCODE) { exit $LASTEXITCODE }; exit 1 }"
      return (T.pack shell, ["-NoProfile", "-NonInteractive", "-OutputFormat", "Text", "-Command", command])
  | otherwise = return (T.pack path, args)
  where
    path = Path.toFilePath script

logNewFileSkip :: RuntimeOptions -> TChan FlowTypes.LogMessage -> T.Text -> AbsFile -> IO ()
logNewFileSkip opts ch logIdentifier absTarget =
  Control.Monad.when (onlyNewFiles opts) $ do
    let relativeTarget = relativeToBase opts (Path.toFilePath absTarget)
    logVerbose opts ch $
      "Skipping " <> logIdentifier <> ": " <> T.pack relativeTarget <> " is at least as new as its source"

-- | Run the preprocess script, writing to the staged file. The command shown names the final
-- output instead, so a pasted command writes where hledger-flow does.
preprocess :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> AbsFile -> IO ()
preprocess opts ch script dirs src csvOut staged = do
  let args out = map (T.pack . Path.toFilePath) [src, out] ++ hookDirArgs dirs
      relSrc = relativeToBase opts (Path.toFilePath src)
      external = External (relativeHook opts script) ("preprocess " <> T.pack relSrc)
        [T.pack (Path.toFilePath script) : args csvOut] []
  (cmd, hookArgs) <- hookCommand script (args staged)
  void $ runExternal opts ch external True (cmd, hookArgs, Nothing)

relativeHook :: RuntimeOptions -> AbsFile -> T.Text
relativeHook opts = T.pack . relativeToBase opts . Path.toFilePath

savedInput :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> AbsFile -> IO AbsFile
savedInput opts ch src csvOut = do
  cached <- verboseTestFile opts ch "preprocessed CSV from an earlier run" csvOut
  if cached
    then do
      stale <- needsRegeneration src csvOut
      when stale $ ioError $ userError $
        relativeToBase opts (Path.toFilePath src) ++ " is newer than its cached CSV "
          ++ relativeToBase opts (Path.toFilePath csvOut) ++ ". Restore the preprocess hook to regenerate it,"
          ++ " or move the cached CSV aside to use the source directly. Then run hledger-flow import."
      return csvOut
    else return src

hledgerImport :: RuntimeOptions -> TChan FlowTypes.LogMessage -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> IO ()
hledgerImport opts ch importDirs csvFile journalOut = do
  let csvSrc = Path.toFilePath csvFile
  candidates <- rulesFileCandidates csvFile importDirs
  maybeRulesFile <- findFirstFile opts ch "rules file" candidates
  let relCSV = relativeToBase opts csvSrc
  case maybeRulesFile of
    Just rf -> do
      let directives = directivesFile opts
      exists <- PathIO.doesFileExist directives
      let paths = RulesPaths csvFile rf (if exists then Just directives else Nothing)
      stdOut <- convertRules opts ch paths paths (hledgerConfArgs opts) pure
      writeGeneratedText (Path.toFilePath journalOut) stdOut
    Nothing ->
      do
        let candidatesTxt = T.intercalate "\n" $ map (T.pack . relativeToBase opts . Path.toFilePath) candidates
        let msg =
              "I couldn't find an hledger rules file while trying to import\n" <> T.pack relCSV
                <> "\n\nI will happily use the first rules file I can find from any one of these "
                <> T.pack (show (length candidates)) <> " files:\n" <> candidatesTxt
                <> "\n\nHere is a bit of documentation about rules files that you may find helpful:\n"
                <> docURL RulesFiles
        errExit 1 ch msg

data RulesPaths = RulesPaths AbsFile AbsFile (Maybe AbsFile)

convertRules :: RuntimeOptions -> TChan FlowTypes.LogMessage -> RulesPaths -> RulesPaths -> [T.Text]
             -> (T.Text -> IO T.Text) -> IO T.Text
convertRules opts ch original@(RulesPaths csv rules _) actual config translate = do
  let shown = T.pack . relativeToBase opts . Path.toFilePath
      hledger = T.pack (Path.toFilePath (hledgerExecutable opts))
      args conf (RulesPaths input rf directives) = do
        let paths = input : maybe [] pure directives
        fileArg <- if any (any isGlobMetacharacter . Path.toFilePath) paths
          then hledgerFileArgument <$> hledgerInfo opts else pure T.pack
        let file p = ["--file", fileArg (Path.toFilePath p)]
        pure $ conf ++ ["print", "--explicit"] ++ maybe [] file directives ++ file input
               ++ ["--rules-file", T.pack (Path.toFilePath rf)]
  command <- args (hledgerConfArgs opts) original
  invocation <- args config actual
  let ext = External "hledger" ("convert " <> shown csv <> " with the rules in " <> shown rules)
              [hledger : command] ["Check the skip, fields and separator lines in " <> shown rules <> ".", "Docs: " <> docURL RulesFiles]
  announceExternal opts ch ext
  ((status,out,err),duration) <- timeAction $ runProcessCaptured (hledger,invocation,Nothing)
  diagnostic <- translate err
  diagnosticOut <- if status == ExitSuccess then pure out else translate out
  checkExternal opts ch ext False duration (status,diagnosticOut,diagnostic)
  pure out

hookDirArgs :: ImportDirs b -> [T.Text]
hookDirArgs dirs = map (T.concat . importDirLines) [bankDir dirs, accountDir dirs, ownerDir dirs]

-- | Pipe the construct script's output through hledger into the staged journal. The command
-- shown names the final journal instead, so a pasted command writes where hledger-flow does.
customConstruct :: RuntimeOptions -> TChan FlowTypes.LogMessage -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> AbsFile -> IO ()
customConstruct opts ch = constructWith opts ch (hledgerConfArgs opts) pure

constructWith :: RuntimeOptions -> TChan FlowTypes.LogMessage -> [T.Text] -> (T.Text -> IO T.Text) -> AbsFile -> ImportDirs Path.Abs -> AbsFile -> AbsFile -> AbsFile -> IO ()
constructWith opts ch config translate constructScript dirs csvFile journalOut staged = do
  let csvSrc = Path.toFilePath csvFile
      relScript = relativeHook opts constructScript
      relSrc = T.pack (relativeToBase opts csvSrc)
      constructArgs = [T.pack csvSrc, "-"] ++ hookDirArgs dirs
      hledger = T.pack (Path.toFilePath (hledgerExecutable opts))
      hledgerArgs conf out = conf ++ ["print", "--ignore-assertions", "--file", "-", "--output-file", T.pack (Path.toFilePath out)]
      shown = [T.pack (Path.toFilePath constructScript) : constructArgs, hledger : hledgerArgs (hledgerConfArgs opts) journalOut]
      constructExt = External relScript ("construct a journal from " <> relSrc) shown []
      hledgerExt = External "hledger" ("read the journal " <> relScript <> " constructed from " <> relSrc) shown []
  (cmd, hookArgs) <- hookCommand constructScript constructArgs
  let hook = (Process.proc (T.unpack cmd) (map T.unpack hookArgs))
        { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
  announceExternal opts ch constructExt
  Process.withCreateProcess hook $ \input output errors handle -> do
    Just stdinPipe <- return input
    Just stdoutPipe <- return output
    Just stderrPipe <- return errors
    hClose stdinPipe
    (((hledgerStatus, converterOut, converterErr), duration), hookErr) <- concurrently
      (timeAction (runProcessCaptured (hledger, hledgerArgs config staged, Just stdoutPipe)))
      (T.hGetContents stderrPipe)
    mappedErr <- translate converterErr
    let converted = (hledgerStatus, converterOut, mappedErr)
    status <- Process.waitForProcess handle
    color <- useColor stderr
    -- When hledger stops reading early, the script fails with a broken pipe, so hledger's error
    -- comes first, along with anything the script said.
    let scriptNote = case status of
          ExitFailure code -> [relScript <> " also failed, with exit code " <> T.pack (show code) <> "."]
          ExitSuccess -> []
        scriptOutput = outputSection color (verbose opts) ("Error output from " <> relScript) hookErr
    when (hledgerStatus /= ExitSuccess) $
      checkExternal opts ch hledgerExt {extHints = scriptNote ++ scriptOutput} True duration converted
    checkExternal opts ch constructExt True duration (status, "", hookErr)
    checkExternal opts ch hledgerExt True duration converted
