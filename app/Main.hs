{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parsing ( StartYear, resolveStartYear, startYearReader )
import Path ( reldir )

import Prelude hiding (putStrLn)

import qualified Options.Applicative as Opts (command)
import Options.Applicative.Help.Pretty (Doc, pretty, vsep)
import Options.Applicative
    ( auto,
      argument,
      customExecParser,
      headerDoc,
      helper,
      hsubparser,
      info,
      prefs,
      progDesc,
      footerDoc,
      hidden,
      optional,
      Alternative(many, (<|>)),
      Parser,
      flag',
      help,
      long,
      metavar,
      option,
      short,
      str,
      switch )

import Hledger.Flow.Types (HledgerInfo (..))
import Hledger.Flow.Common ( deferredHledgerVersion, findHledgerConf, hledgerPathFromOption, hledgerInfoFromPath )
import Hledger.Flow.Status ( Situation (..) )
import Hledger.Flow.Status.Gather ( classifyDir )
import Hledger.Flow.Status.Render ( printStatus )
import qualified Data.Text as Text
import Path.IO ( getCurrentDir, resolveDir' )
import Hledger.Flow.Init ( runInit )
import Hledger.Flow.DocHelpers ( DocLink (Docs), docURL, siteURL )
import Hledger.Flow.RulesTemplate ( rulesTemplateFor )
import Hledger.Flow.Internals (versionInfo, systemInfo)
import GHC.Conc (getNumCapabilities)
import Hledger.Flow.BaseDir ( determineBaseDir, findBaseDir )
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.ReleaseNotice ( withReleaseNotice )
import Hledger.Flow.Reports ( generateReports )
import Hledger.Flow.Import.CSVImport ( importCSVs )

import qualified Data.Text.IO as T
import Control.Exception (SomeAsyncException (..), SomeException, catch, displayException, fromException, throwIO)
import Hledger.Flow.PathHelpers (PathException (..), resolveExistingDir)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import System.IO.Error (ioeGetErrorString, isUserError)
#ifdef mingw32_HOST_OS
import System.IO.CodePage (withCP65001)
#endif

data ImportParams = ImportParams { maybeImportBaseDir :: Maybe FilePath
                                 , importStartYear :: Maybe StartYear
                                 , onlyNewFiles :: Bool
                                 } deriving (Show)

data ReportParams = ReportParams { maybeReportBaseDir :: Maybe FilePath
                                 , asciiReports :: Bool
                                 } deriving (Show)

data StatusParams = StatusParams { statusDir :: Maybe FilePath
                                 , statusJson :: Bool
                                 , statusRulesTemplate :: Maybe String
                                 } deriving (Show)

newtype InitParams = InitParams { initDir :: Maybe FilePath } deriving (Show)

data Command = Import ImportParams | Report ReportParams | Status StatusParams | Init InitParams deriving (Show)

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
      hPutStrLn stderr ("hledger-flow: " ++ fatalMessage e)
      exitWith (ExitFailure 1)

fatalMessage :: SomeException -> String
fatalMessage e
  | Just (pe :: PathException) <- fromException e = show pe
  | Just ioe <- fromException e, isUserError ioe = ioeGetErrorString ioe
  | otherwise = displayException e

runCommand :: IO ()
runCommand = do
  let description = pretty ("An hledger workflow focusing on automated statement import and classification:\n" <> Text.unpack siteURL)
  cmd <- customExecParser (prefs mempty)
    (info (helper <*> baseCommandParser) (headerDoc (Just description) <> footerDoc (Just topExamples)))
  case cmd of
    Version -> do
      sysInfo <- systemInfo
      T.putStrLn $ versionInfo sysInfo 
    Command mainParams' (Just (Import subParams)) ->
      withReleaseNotice $ (toRuntimeOptionsImport mainParams' subParams >>= importCSVs) `catch` reportFatal
    Command mainParams' (Just (Report subParams)) ->
      withReleaseNotice $ (toRuntimeOptionsReport mainParams' subParams >>= generateReports) `catch` reportFatal
    Command mainParams' (Just (Status subParams)) -> runStatus mainParams' subParams
    Command mainParams' (Just (Init subParams)) -> do
      cwd <- getCurrentDir
      dir <- maybe (return cwd) resolveDir' (initDir subParams)
      runInit (hledgerPathOpt mainParams') cwd dir
    Command mainParams' Nothing -> runBare mainParams'

-- | (name, one-line description) of each command, in display order.
commandSummaries :: [(String, String)]
commandSummaries =
  [ ("status", "Show what's in your ledger and what to do next"),
    ("init", "Start a new ledger in a folder"),
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
  cwd <- getCurrentDir
  dir <- maybe (return cwd) (resolveExistingDir cwd) (statusDir params)
  base <- findBaseDir dir >>= maybe (throwIO (MissingBaseDir dir)) return
  case statusRulesTemplate params of
    Just account -> rulesTemplateFor base (Text.pack account) >>= either (ioError . userError . Text.unpack) T.putStr
    Nothing -> printStatus (hledgerPathOpt mainParams') (hledgerConfOpt mainParams') (statusJson params) base

-- | Running hledger-flow with no command: say where we are and what to do.
runBare :: MainParams -> IO ()
runBare mainParams' = do
  situation <- getCurrentDir >>= classifyDir
  case situation of
    InLedger base ->
      printStatus (hledgerPathOpt mainParams') (hledgerConfOpt mainParams') False base
    EmptyDir -> T.putStrLn "This folder is empty. Run `hledger-flow init` to start your ledger here."
    Elsewhere ->
      T.putStr $
        briefUsage
          <> "\nNo hledger-flow ledger here or in any parent folder.\n"
          <> "Run `hledger-flow init` in an empty folder to start one.\n"

-- One hledger process per core in use keeps every core busy without
-- multiplying memory on small machines. The runtime fixed this at startup.
determineBatchSize :: MainParams -> IO Int
determineBatchSize mainParams' = maybe getNumCapabilities return (batchSize mainParams')

toRuntimeOptionsImport :: MainParams -> ImportParams -> IO RT.RuntimeOptions
toRuntimeOptionsImport mainParams' subParams' = do
  startYear <- traverse resolveStartYear (importStartYear subParams')
  let maybeBD = maybeImportBaseDir subParams' :: Maybe FilePath
  (bd, runDir) <- determineBaseDir maybeBD
  hlp <- hledgerPathFromOption $ hledgerPathOpt mainParams'
  version <- deferredHledgerVersion hlp
  conf <- findHledgerConf $ hledgerConfOpt mainParams'
  size <- determineBatchSize mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = runDir
                           , RT.importStartYear = startYear
                           , RT.onlyNewFiles = onlyNewFiles subParams'
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerExecutable = hlp
                           , RT.hledgerVersion = version
                           , RT.hledgerConf = conf
                           , RT.sysInfo = sysInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams'
                           , RT.batchSize = size
                           , RT.prettyReports = True
                           }

toRuntimeOptionsReport :: MainParams -> ReportParams -> IO RT.RuntimeOptions
toRuntimeOptionsReport mainParams' subParams' = do
  let maybeBD = maybeReportBaseDir subParams' :: Maybe FilePath
  (bd, _) <- determineBaseDir maybeBD
  HledgerInfo hlp version <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  conf <- findHledgerConf $ hledgerConfOpt mainParams'
  size <- determineBatchSize mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = [reldir|.|]
                           , RT.importStartYear = Nothing
                           , RT.onlyNewFiles = False
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerExecutable = hlp
                           , RT.hledgerVersion = return version
                           , RT.hledgerConf = conf
                           , RT.sysInfo = sysInfo
                           , RT.verbose = verbosity mainParams' > 0
                           , RT.showOptions = showOpts mainParams'
                           , RT.sequential = sequential mainParams'
                           , RT.batchSize = size
                           , RT.prettyReports = not(asciiReports subParams')
                           }

baseCommandParser :: Parser BaseCommand
baseCommandParser = (Command <$> verboseParser <*> commandParser)
  <|> flag' Version (long "version" <> short 'V' <> help "Display version information")

commandParser :: Parser (Maybe Command)
commandParser = optional $ hsubparser $
  subcommand "status" (Status <$> subcommandParserStatus)
    <> subcommand "init" (Init . InitParams <$> optional (argument str (metavar "DIR" <> help "The folder for the new ledger, created if missing. Defaults to the current folder.")))
    <> subcommand "import" (Import <$> subcommandParserImport)
    <> subcommand "report" (Report <$> subcommandParserReport)
    <> metavar "COMMAND"
  where
    subcommand name parser = Opts.command name (info parser (progDesc (summaryOf name) <> footerDoc (lines' <$> lookup name subcommandExamples)))

lines' :: [String] -> Doc
lines' = vsep . map pretty

topExamples :: Doc
topExamples = lines'
  [ "Examples:",
    "  hledger-flow init            Start a ledger in the current folder",
    "  hledger-flow status          See what's there and what to do next",
    "  hledger-flow import          Turn statements into journals",
    "  hledger-flow report          Generate reports",
    "  hledger-flow status --json   The same status, for scripts and agents",
    "",
    "Docs: " <> Text.unpack (docURL Docs)
  ]

subcommandExamples :: [(String, [String])]
subcommandExamples =
  [ ("status", ["Examples:", "  hledger-flow status --json", "  hledger-flow status --rules-template me/mybank/checking"]),
    ("init", ["Example:", "  hledger-flow init ~/finances"]),
    ("import", ["Examples:", "  hledger-flow import import/me/mybank", "  hledger-flow import --start-year current"]),
    ("report", ["Example:", "  hledger-flow report"])
  ]

subcommandParserStatus :: Parser StatusParams
subcommandParserStatus = StatusParams
  <$> optional (argument str (metavar "DIR" <> help "A folder inside the ledger. Defaults to the current folder."))
  <*> switch (long "json" <> help "Print the status as JSON, for scripts and agents")
  <*> optional (option str (long "rules-template" <> metavar "ACCOUNT" <> help "Print a starter rules file for owner/bank/account, built from its newest statement's header row"))

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print more verbose output")))
  <*> optional (option str (long "hledger-path" <> short 'H' <> metavar "HLEDGER-PATH" <> help "The full path to an hledger executable"))
  <*> optional (option str (long "hledger-conf" <> metavar "PATH" <> hidden <> help "The hledger config file to pass to hledger --conf"))
  <*> switch (long "show-options" <> hidden <> help "Print the hledger-flow version, hledger path and settings import or report runs with. status always shows them.")
  <*> optional (option auto (long "batch-size" <> metavar "SIZE" <> hidden <> help "Number of files to process in parallel. Must be positive. Default: the number of cores in use. Ignored during sequential processing."))
  <*> switch (long "sequential" <> hidden <> help "Disable parallel processing")

subcommandParserImport :: Parser ImportParams
subcommandParserImport = ImportParams
  <$> optional (argument str (metavar "DIR" <> help "The directory to import. Use the base directory for a full import or a sub-directory for a partial import. Defaults to the current directory."))
  <*> optional (option startYearReader (long "start-year" <> metavar "YEAR" <> help "Import only from the specified year and onwards, ignoring previous years. By default all available years are imported. Use a 4-digit year or 'current' for the current year"))
  <*> switch (long "new-files-only" <> help "Skip regenerating output files that are at least as new as their source. This applies to preprocessed files and hledger journal files.")

subcommandParserReport :: Parser ReportParams
subcommandParserReport = ReportParams
  <$> optional (argument str (metavar "BASEDIR" <> help "The hledger-flow base directory"))
  <*> switch (long "ascii-reports" <> help "If to avoid using hledger --pretty-tables flag when generating reports.")
