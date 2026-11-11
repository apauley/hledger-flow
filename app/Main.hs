{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parsing ( parseStartYear )
import Path ( reldir )

import Prelude hiding (putStrLn)

import qualified Options.Applicative as Opts (command)
import Options.Applicative
    ( auto,
      argument,
      customExecParser,
      headerDoc,
      helper,
      hsubparser,
      info,
      prefs,
      progDescDoc,
      showHelpOnEmpty,
      showHelpOnError,
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

import Hledger.Flow.Common ( hledgerInfoFromPath )
import Hledger.Flow.Internals (versionInfo, systemInfo)
import Hledger.Flow.BaseDir ( determineBaseDir )
import qualified Hledger.Flow.RuntimeOptions as RT
import Hledger.Flow.Reports ( generateReports )
import Hledger.Flow.Import.CSVImport ( importCSVs )

import qualified Data.Text.IO as T
import Control.Exception (SomeAsyncException (..), SomeException, catch, displayException, fromException, throwIO)
import Hledger.Flow.PathHelpers (PathException)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, isUserError)
#ifdef mingw32_HOST_OS
import System.IO.CodePage (withCP65001)
#endif

data ImportParams = ImportParams { maybeImportBaseDir :: Maybe FilePath
                                 , importStartYear :: Maybe String
                                 , onlyNewFiles :: Bool
                                 } deriving (Show)

data ReportParams = ReportParams { maybeReportBaseDir :: Maybe FilePath
                                 , asciiReports :: Bool
                                 } deriving (Show)

data Command = Import ImportParams | Report ReportParams deriving (Show)

data MainParams = MainParams { verbosity :: Int
                             , hledgerPathOpt :: Maybe FilePath
                             , hledgerConfOpt :: Maybe FilePath
                             , showOpts :: Bool
                             , batchSize :: Maybe Int
                             , sequential :: Bool
                             } deriving (Show)
data BaseCommand = Version | Command { mainParams :: MainParams, command :: Command } deriving (Show)

main :: IO ()
#ifdef mingw32_HOST_OS
-- hledger inherits the console code page; generated journals, pipes and
-- diagnostics must use the same encoding for the whole run.
main = withCP65001 runCLI
#else
main = runCLI
#endif

runCLI :: IO ()
runCLI = runCommand `catch` reportFatal

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
  let description = "An hledger workflow focusing on automated statement import and classification:\nhttps://github.com/apauley/hledger-flow#readme"
  cmd <- customExecParser (prefs (showHelpOnError <> showHelpOnEmpty))
    (info (helper <*> baseCommandParser) (headerDoc (Just description)))
  case cmd of
    Version -> do
      sysInfo <- systemInfo
      T.putStrLn $ versionInfo sysInfo 
    Command mainParams' (Import subParams) -> toRuntimeOptionsImport mainParams' subParams >>= importCSVs
    Command mainParams' (Report subParams) -> toRuntimeOptionsReport mainParams' subParams >>= generateReports

defaultBatchSize :: Int
defaultBatchSize = 20

determineBatchSize :: MainParams -> IO Int
determineBatchSize mainParams' =
  case (batchSize mainParams') of
    Nothing   -> return defaultBatchSize
    Just size -> return size

toRuntimeOptionsImport :: MainParams -> ImportParams -> IO RT.RuntimeOptions
toRuntimeOptionsImport mainParams' subParams' = do
  startYear <- parseStartYear $ importStartYear subParams'
  let maybeBD = maybeImportBaseDir subParams' :: Maybe FilePath
  (bd, runDir) <- determineBaseDir maybeBD
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  size <- determineBatchSize mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = runDir
                           , RT.importStartYear = startYear
                           , RT.onlyNewFiles = onlyNewFiles subParams'
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerInfo = hli
                           , RT.hledgerConf = hledgerConfOpt mainParams'
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
  hli <- hledgerInfoFromPath $ hledgerPathOpt mainParams'
  size <- determineBatchSize mainParams'
  sysInfo <- systemInfo
  return RT.RuntimeOptions { RT.baseDir = bd
                           , RT.importRunDir = [reldir|.|]
                           , RT.importStartYear = Nothing
                           , RT.onlyNewFiles = False
                           , RT.hfVersion = versionInfo sysInfo
                           , RT.hledgerInfo = hli
                           , RT.hledgerConf = hledgerConfOpt mainParams'
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

commandParser :: Parser Command
commandParser = fmap Import (subcommand "import" "Uses hledger with your own rules and/or scripts to convert electronic statements into categorised journal files" subcommandParserImport)
  <|> fmap Report (subcommand "report" "Generate Reports" subcommandParserReport)

  where
    subcommand name desc parser =
      hsubparser (Opts.command name (info parser (progDescDoc (Just desc))) <> metavar name)

verboseParser :: Parser MainParams
verboseParser = MainParams
  <$> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print more verbose output")))
  <*> optional (option str (long "hledger-path" <> short 'H' <> metavar "HLEDGER-PATH" <> help "The full path to an hledger executable"))
  <*> optional (option str (long "hledger-conf" <> metavar "PATH" <> help "The hledger config file to pass to hledger --conf"))
  <*> switch (long "show-options" <> help "Print the options this program will run with")
  <*> optional (option auto (long "batch-size" <> metavar "SIZE" <> help ("Number of files to process in parallel. Must be positive. Default: " <> show defaultBatchSize <> ". Ignored during sequential processing.")))
  <*> switch (long "sequential" <> help "Disable parallel processing")

subcommandParserImport :: Parser ImportParams
subcommandParserImport = ImportParams
  <$> optional (argument str (metavar "DIR" <> help "The directory to import. Use the base directory for a full import or a sub-directory for a partial import. Defaults to the current directory."))
  <*> optional (option str (long "start-year" <> metavar "YEAR" <> help "Import only from the specified year and onwards, ignoring previous years. By default all available years are imported. Valid values include a 4-digit year or 'current' for the current year"))
  <*> switch (long "new-files-only" <> help "Skip regenerating output files that are at least as new as their source. This applies to preprocessed files and hledger journal files.")

subcommandParserReport :: Parser ReportParams
subcommandParserReport = ReportParams
  <$> optional (argument str (metavar "BASEDIR" <> help "The hledger-flow base directory"))
  <*> switch (long "ascii-reports" <> help "If to avoid using hledger --pretty-tables flag when generating reports.")
