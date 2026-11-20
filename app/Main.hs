{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parsing ( StartYear, resolveStartYear, startYearReader )
import Path ( reldir )

import Prelude hiding (putStrLn)

import qualified Options.Applicative as Opts (command)
import Options.Applicative.Help.Pretty (Color (Cyan), Doc, annotate, bold, color, fillSep, indent, pretty, vsep, (<+>))
import Options.Applicative
    ( auto,
      eitherReader,
      argument,
      execParserPure,
      handleParseResult,
      headerDoc,
      helper,
      helpShowGlobals,
      hsubparser,
      info,
      prefs,
      progDesc,
      renderFailure,
      ParserResult (Failure),
      footerDoc,
      hidden,
      internal,
      optional,
      Alternative(many, (<|>)),
      Parser,
      flag,
      flag',
      help,
      helpDoc,
      long,
      metavar,
      option,
      short,
      str,
      style,
      switch )
-- Exposed by optparse-applicative, though not re-exported by Options.Applicative.
import Options.Applicative.Builder.Internal (noGlobal)
import System.Environment (getArgs)

import Hledger.Flow.Types (BaseDir, RunDir, HledgerInfo (..))
import Hledger.Flow.Common ( homeDirs, tildePath, deferredHledgerVersion, findHledgerConf, hledgerPathFromOption, hledgerInfoFromPath )
import Hledger.Flow.Status ( Situation (..) )
import Hledger.Flow.Status.Gather ( classifyDir )
import Hledger.Flow.Status.Render ( printStatus )
import qualified Data.Text as Text
import Path.IO ( getCurrentDir, resolveDir' )
import Hledger.Flow.HledgerConfig (ConfigUse (..), requireSafeConfig)
import Hledger.Flow.Init ( InitMode (..), runInit )
import Hledger.Flow.DocHelpers ( DocLink (..), docURL, siteURL )
import Hledger.Flow.Logging ( useColor )
import Hledger.Flow.RulesTemplate ( rulesTemplateFor, runRules )
import Data.Maybe (fromMaybe)
import Hledger.Flow.Internals (availableCores, versionInfo, systemInfo)
import Hledger.Flow.BaseDir ( determineBaseDir, findBaseDir )
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.ReleaseNotice ( withReleaseNotice, withCachedReleaseNotice )
import Hledger.Flow.Reports ( generateReports )
import Hledger.Flow.Import.CSVImport ( importCSVs )

import qualified Data.Text.IO as T
import Control.Exception (SomeAsyncException (..), SomeException, catch, displayException, fromException, throwIO)
import Hledger.Flow.PathHelpers (PathException (..), initAdvice, renderPathException, resolveExistingDir)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import System.IO.Error (ioeGetErrorString, isUserError)
#ifdef mingw32_HOST_OS
import System.IO.CodePage (withCP65001)
#endif

data ImportParams = ImportParams { maybeImportBaseDir :: Maybe FilePath
                                 , importStartYear :: Maybe StartYear
                                 , filePolicy :: RT.ImportPolicy
                                 } deriving (Show)

data ReportParams = ReportParams { maybeReportBaseDir :: Maybe FilePath
                                 , asciiReports :: Bool
                                 } deriving (Show)

data StatusParams = StatusParams { statusDir :: Maybe FilePath
                                 , statusJson :: Bool
                                 , statusRulesTemplate :: Maybe String
                                 } deriving (Show)

data InitParams = InitParams { initDir :: Maybe FilePath, initMode :: InitMode } deriving (Show)

data Command = Import ImportParams | Report ReportParams | Status StatusParams | Rules String (Maybe FilePath) | Init InitParams deriving (Show)

data MainParams = MainParams { verbosity :: Int
                             , hledgerPathOpt :: Maybe FilePath
                             , hledgerConfOpt :: Maybe FilePath
                             , showOpts :: Bool
                             , batchSize :: Maybe Int
                             , sequential :: Bool
                             } deriving (Show)
data BaseCommand = Version | Command { mainParams :: MainParams, command :: Maybe Command } deriving (Show)

main :: IO ()
#ifdef mingw32_HOST_OS
-- hledger inherits the console code page; generated journals, pipes and
-- diagnostics must use the same encoding for the whole run.
main = withCP65001 runCLI
#else
main = runCLI
#endif

runCLI :: IO ()
runCLI = do
  -- Status and errors use non-ASCII symbols; a C locale must not make them fail.
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  runCommand `catch` reportFatal

-- | Print uncaught errors as one plain message, without GHC's backtrace.
reportFatal :: SomeException -> IO ()
reportFatal e
  | Just (code :: ExitCode) <- fromException e = throwIO code
  | Just (SomeAsyncException _) <- fromException e = throwIO e
  | otherwise = do
      message <- fatalMessage e
      hPutStrLn stderr ("hledger-flow: " ++ message)
      exitWith (ExitFailure 1)

fatalMessage :: SomeException -> IO String
fatalMessage e
  | Just (pe :: PathException) <- fromException e = do
      homes <- homeDirs
      return (renderPathException (tildePath homes) pe)
  | Just ioe <- fromException e, isUserError ioe = return (ioeGetErrorString ioe)
  | otherwise = return (displayException e)

runCommand :: IO ()
runCommand = do
  colour <- useColor stdout
  let description = pretty ("An hledger workflow focusing on automated statement import and classification:\n" <> Text.unpack siteURL)
      cli = info (helper <*> baseCommandParser colour) (headerDoc (Just description) <> footerDoc (Just (topExamples colour)))
  args <- getArgs
  -- Subcommand help lists the global options. Keep them out of usage errors,
  -- where they would bury the message.
  let parse p = execParserPure (prefs p) cli args
  cmd <- handleParseResult $ case parse helpShowGlobals of
    Failure failure | (_, ExitFailure _) <- renderFailure failure "" -> parse mempty
    result -> result
  case cmd of
    Version -> do
      sysInfo <- systemInfo
      T.putStrLn $ versionInfo sysInfo 
    Command mainParams' (Just (Import subParams)) -> do
      (base, runDir) <- determineBaseDir (maybeImportBaseDir subParams)
      opts <- toRuntimeOptionsImport base runDir mainParams' subParams
      requireSafeConfig ImportConfig opts
      withReleaseNotice base $ importCSVs opts `catch` reportFatal
    Command mainParams' (Just (Report subParams)) -> do
      (base, _) <- determineBaseDir (maybeReportBaseDir subParams)
      opts <- toRuntimeOptionsReport base mainParams' subParams
      requireSafeConfig ReportConfig opts
      withReleaseNotice base $ generateReports opts `catch` reportFatal
    Command mainParams' (Just (Status subParams)) -> runStatus mainParams' subParams
    Command mainParams' (Just (Rules account dir)) -> do
      base <- statusBaseDir dir
      conf <- findHledgerConf (hledgerConfOpt mainParams')
      runRules conf base (Text.pack account)
    Command mainParams' (Just (Init subParams)) -> do
      cwd <- getCurrentDir
      dir <- maybe (return cwd) resolveDir' (initDir subParams)
      runInit (initMode subParams) (hledgerPathOpt mainParams') (hledgerConfOpt mainParams') cwd dir
    Command mainParams' Nothing -> runBare mainParams'

-- | (name, one-line description) of each command, in display order.
commandSummaries :: [(String, String)]
commandSummaries =
  [ ("status", "Show what's in your ledger and what to do next"),
    ("init", "Start a new ledger in a folder"),
    ("rules", "Create a starter rules file for an account"),
    ("import", "Turn statements into journals"),
    ("report", "Generate reports from your journals")
  ]

summaryOf :: String -> String
summaryOf name = maybe "" id (lookup name commandSummaries)

briefUsage :: Text.Text
briefUsage =
  "hledger-flow turns bank statements into hledger journals and reports.\n\nCommands:\n"
    <> Text.concat ["  " <> Text.justifyLeft width ' ' (Text.pack name) <> "   " <> Text.pack desc <> "\n" | (name, desc) <- commandSummaries]
    <> "\nRun `hledger-flow --help` for options and examples.\n"
  where
    width = maximum (map (length . fst) commandSummaries)

runStatus :: MainParams -> StatusParams -> IO ()
runStatus mainParams' params = do
  base <- statusBaseDir (statusDir params)
  case statusRulesTemplate params of
    Just account -> rulesTemplateFor base (Text.pack account) >>= either (ioError . userError . Text.unpack) T.putStr
    Nothing | statusJson params -> printStatus (hledgerPathOpt mainParams') (hledgerConfOpt mainParams') True base
            | otherwise -> printInteractiveStatus mainParams' base

statusBaseDir :: Maybe FilePath -> IO BaseDir
statusBaseDir target = do
  cwd <- getCurrentDir
  dir <- maybe (return cwd) (resolveExistingDir cwd) target
  findBaseDir dir >>= maybe (throwIO (MissingBaseDir dir)) return

printInteractiveStatus :: MainParams -> BaseDir -> IO ()
printInteractiveStatus params base = withCachedReleaseNotice base $
  printStatus (hledgerPathOpt params) (hledgerConfOpt params) False base

-- | Running hledger-flow with no command: say where we are and what to do.
runBare :: MainParams -> IO ()
runBare mainParams' = do
  situation <- getCurrentDir >>= classifyDir
  case situation of
    InLedger base -> printInteractiveStatus mainParams' base
    EmptyDir -> T.putStrLn ("This folder is empty.\n" <> Text.pack initAdvice)
    Elsewhere ->
      T.putStr $
        briefUsage
          <> "\nNo hledger-flow ledger here or in any parent folder.\n"
          <> "To start one, change to an empty folder.\n" <> Text.pack initAdvice <> "\n"

toRuntimeOptionsImport :: BaseDir -> RunDir -> MainParams -> ImportParams -> IO RT.RuntimeOptions
toRuntimeOptionsImport bd runDir mainParams' subParams' = do
  startYear <- traverse resolveStartYear (importStartYear subParams')
  hlp <- hledgerPathFromOption $ hledgerPathOpt mainParams'
  version <- deferredHledgerVersion hlp
  conf <- findHledgerConf $ hledgerConfOpt mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = runDir
                           , RT.importStartYear = startYear
                           , RT.importPolicy = filePolicy subParams'
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerExecutable = hlp
                           , RT.hledgerVersion = version
                           , RT.hledgerConf = conf
                           , RT.sysInfo = sysInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams'
                           , RT.batchSize = fromMaybe (availableCores sysInfo) (batchSize mainParams')
                           , RT.prettyReports = True
                           }

toRuntimeOptionsReport :: BaseDir -> MainParams -> ReportParams -> IO RT.RuntimeOptions
toRuntimeOptionsReport bd mainParams' subParams' = do
  HledgerInfo hlp version <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  conf <- findHledgerConf $ hledgerConfOpt mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = [reldir|.|]
                           , RT.importStartYear = Nothing
                           , RT.importPolicy = RT.Full
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerExecutable = hlp
                           , RT.hledgerVersion = return version
                           , RT.hledgerConf = conf
                           , RT.sysInfo = sysInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams'
                           , RT.batchSize = fromMaybe (availableCores sysInfo) (batchSize mainParams')
                           , RT.prettyReports = not(asciiReports subParams')
                           }

baseCommandParser :: Bool -> Parser BaseCommand
baseCommandParser colour = (Command <$> verboseParser <*> commandParser colour)
  <|> flag' Version (long "version" <> short 'V' <> noGlobal <> help "Display version information.")

commandParser :: Bool -> Parser (Maybe Command)
commandParser colour = optional $ hsubparser $
  subcommand "status" StatusCommand (Status <$> subcommandParserStatus)
    [ ("hledger-flow status --json", "The same status, for scripts and agents"),
      ("hledger-flow status --rules-template me/mybank/checking", "Print a starter rules file for one account") ]
    <> subcommand "init" GettingStarted (Init <$> subcommandParserInit)
    [ ("hledger-flow init ~/finances", "Start a ledger in ~/finances") ]
    <> subcommand "rules" RulesFiles
      (Rules <$> argument str (metavar "ACCOUNT" <> help "Account as owner/bank/account.")
             <*> optional (argument str (metavar "DIR" <> help "Inspect the ledger containing this folder.")))
    [ ("hledger-flow rules me/mybank/checking", "Write a starter rules file for this account") ]
    <> subcommand "import" InputFiles (Import <$> subcommandParserImport colour)
    [ ("hledger-flow import", "Import the current folder"),
      ("hledger-flow import import/me/mybank", "Import one bank's statements"),
      ("hledger-flow import --start-year current", "Import this year only"),
      ("hledger-flow import --incremental", "Rebuild only what changed") ]
    <> subcommand "report" Reports (Report <$> subcommandParserReport)
    [ ("hledger-flow report", "Write reports to the reports folder") ]
    <> metavar "COMMAND"
  where
    subcommand name link parser examples =
      Opts.command name (info parser (progDesc (summaryOf name) <> footerDoc (Just (examplesDoc colour link examples))))

-- optparse-applicative writes ANSI styles into help even when stdout is a
-- pipe, so style only after useColor has checked the terminal and NO_COLOR.
literal, heading :: Bool -> Doc -> Doc
literal colour = if colour then annotate (color Cyan) else id
heading colour = if colour then annotate bold else id

-- | Commands with descriptions in a column, or below each command when a
-- column would not fit in 80 characters, then the command's docs link.
examplesDoc :: Bool -> DocLink -> [(String, String)] -> Doc
examplesDoc colour link examples = vsep $
  heading colour (pretty (if length examples == 1 then "Example:" else "Examples:" :: String))
    : concatMap row examples
    ++ [mempty, heading colour (pretty ("Docs:" :: String)) <+> pretty (docURL link)]
  where
    width = maximum (map (length . fst) examples)
    fits = 2 + width + 3 + maximum (map (length . snd) examples) <= 80
    row (cmd, desc)
      | fits = [indent 2 (literal colour (pretty cmd) <> pretty (replicate (width - length cmd + 3) ' ' ++ desc))]
      | otherwise = [indent 2 (literal colour (pretty cmd)), indent 4 (pretty desc)]

topExamples :: Bool -> Doc
topExamples colour = examplesDoc colour Docs
  [ ("hledger-flow init", "Start a ledger in the current folder"),
    ("hledger-flow status", "See what's there and what to do next"),
    ("hledger-flow import", "Turn statements into journals"),
    ("hledger-flow report", "Generate reports"),
    ("hledger-flow status --json", "The same status, for scripts and agents")
  ]

subcommandParserInit :: Parser InitParams
subcommandParserInit = InitParams
  <$> optional (argument str (metavar "DIR" <> help "The folder for the new ledger, created if missing. Default: the current folder."))
  <*> flag WriteLedger DryRun (long "dry-run" <> help "Show which files would be created or kept, without writing anything.")

subcommandParserStatus :: Parser StatusParams
subcommandParserStatus = StatusParams
  <$> optional (argument str (metavar "DIR" <> help "A folder inside the ledger. Default: the current folder."))
  <*> switch (long "json" <> help "Print the status as JSON, for scripts and agents.")
  <*> optional (option str (long "rules-template" <> metavar "ACCOUNT" <> help "Print a starter rules file for owner/bank/account, built from its newest statement's header row."))

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Show each step and the commands hledger-flow runs.")))
  <*> optional (option str (long "hledger-path" <> short 'H' <> metavar "FILE" <> help "Use this hledger executable instead of the one on PATH."))
  <*> optional (option str (long "hledger-conf" <> metavar "FILE" <> hidden <> help "Pass this config file to hledger. Default: the one hledger finds itself."))
  <*> switch (long "show-options" <> hidden <> help "Print the version, hledger path and settings before import or report runs. status always shows them.")
  <*> optional (option auto (long "batch-size" <> metavar "SIZE" <> hidden <> help "Number of files to process in parallel. Must be positive. Default: the number of cores in use. Ignored during sequential processing."))
  <*> switch (long "sequential" <> hidden <> help "Disable parallel processing.")

subcommandParserImport :: Bool -> Parser ImportParams
subcommandParserImport colour = ImportParams <$> optional (argument str (metavar "DIR" <> help "The folder to import: the ledger root for everything, or a folder inside it, such as an owner, bank or account, for part of it. Default: the current folder."))
  <*> optional (option startYearReader (long "start-year" <> metavar "YEAR" <> help "Import statements from YEAR onwards only. YEAR is a four-digit year or current. Default: all years."))
  <*> filePolicyParser colour

incrementalModes :: [(String, RT.ImportPolicy)]
incrementalModes = [("run-scripts", RT.RunScripts), ("skip-unchanged-scripts", RT.SkipUnchangedScripts)]

filePolicyParser :: Bool -> Parser RT.ImportPolicy
filePolicyParser colour =
  flag' RT.TimestampOnly (long "new-files-only" <> help "Rebuild a file only when its statement or cached CSV is newer. Fast, but misses other changes, such as an edited rules file.")
  <|> flag' RT.RunScripts (long "incremental" <> style (const (pretty ("--incremental[=MODE]" :: String))) <> helpDoc (Just incrementalHelp))
  <|> option (eitherReader readPolicy) (long "incremental" <> internal)
  <|> pure RT.Full
  where
    readPolicy value = maybe (Left ("unknown mode '" ++ value ++ "'. Use run-scripts or skip-unchanged-scripts.")) Right (lookup value incrementalModes)
    para = fillSep . map pretty . words
    mode name = literal colour (pretty (name :: String))
    incrementalHelp = vsep
      [ para "Rebuild only the journals whose inputs changed, comparing file contents. Accounts without scripts behave the same in both modes.",
        mempty,
        mode "run-scripts" <+> pretty ("(default)" :: String),
        indent 2 (para "Run every preprocess and construct script. Reuse a journal when its inputs, including a preprocess script's output, still match."),
        mode "skip-unchanged-scripts",
        indent 2 (para "Also skip a script when the script and its statement are unchanged. Misses edits to other files the script reads, such as a lookup table.")
      ]

subcommandParserReport :: Parser ReportParams
subcommandParserReport = ReportParams
  <$> optional (argument str (metavar "DIR" <> help "The ledger folder. Default: the ledger containing the current folder."))
  <*> switch (long "ascii-reports" <> help "Draw report tables with plain ASCII instead of box-drawing characters.")
