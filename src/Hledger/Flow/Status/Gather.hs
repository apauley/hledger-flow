{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Collect a ledger's status from disk. Read-only and silent.
module Hledger.Flow.Status.Gather
  ( classifyDir,
    gatherStatus,
  )
where

import Control.Monad (filterM, forM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Hledger.Flow.BaseDir (findBaseDir, relativeToBase')
import Hledger.Flow.Common (firstExistingFile, needsRegeneration)
import Hledger.Flow.Import.ImportHelpers (defaultRulesCandidates, extractImportDirs, rulesFileCandidates)
import Hledger.Flow.Import.Inventory (findImportFiles, generatedOutput, isManualJournal, stageFiles)
import Hledger.Flow.Import.Types (ImportDirs (..))
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelFile)
import Hledger.Flow.Status
import Hledger.Flow.Types (BaseDir, HledgerInfo)
import Path
import qualified Path.IO as PathIO
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import qualified System.FilePath as FilePath
import qualified System.Info as Sys

-- | Is this folder inside a ledger, empty, or something else?
classifyDir :: AbsDir -> IO Situation
classifyDir dir = do
  found <- findBaseDir dir
  case found of
    Just base -> return (InLedger base)
    Nothing -> do
      (dirs, files) <- PathIO.listDirRel dir
      let names = map (FilePath.dropTrailingPathSeparator . toFilePath) dirs ++ map toFilePath files
      return (if all ("." `List.isPrefixOf`) names then EmptyDir else Elsewhere)

gatherStatus :: Maybe HledgerInfo -> BaseDir -> IO LedgerStatus
gatherStatus hledger base = do
  let root = base </> [reldir|import|]
  files <- findImportFiles root
  let statements = stageFiles root [reldir|1-in|] files
      groups = Map.toAscList (Map.fromListWith (flip (++)) [(accountName root s, [s]) | s <- statements])
  accounts <- forM groups (uncurry (accountStatus base))
  ledgerEnv <- lookupEnv "LEDGER_FILE"
  ledgerFile <- forM (filter (not . null) (maybe [] pure ledgerEnv)) $ \v ->
    LedgerFile v <$> Directory.doesFileExist v
  return
    LedgerStatus
      { lsBaseDir = base,
        lsHledger = hledger,
        lsLedgerFile = case ledgerFile of
          lf : _ -> Just lf
          [] -> Nothing,
        lsAccounts = accounts,
        lsMisplaced = List.sortOn mpPath (misplacedFiles root files),
        lsManualJournals = length (filter (isManualJournal root) files)
      }

accountName :: AbsDir -> AbsFile -> T.Text
accountName root file = case stripProperPrefix root file of
  Just rel -> T.intercalate "/" (map T.pack (take 3 (FilePath.splitDirectories (toFilePath rel))))
  Nothing -> T.pack (toFilePath file)

accountStatus :: BaseDir -> T.Text -> [AbsFile] -> IO AccountStatus
accountStatus base name statements = do
  dirsPerStatement <- forM statements $ \s -> either (ioError . userError . T.unpack) return (extractImportDirs s)
  dirs <- case dirsPerStatement of
    d : _ -> return d
    [] -> ioError (userError ("No statements for " ++ T.unpack name))
  let relative = relativeToBase' (toFilePath base) . toFilePath
  defaults <- defaultRulesCandidates dirs
  rulesFile <- firstExistingFile defaults
  resolved <- forM (zip statements dirsPerStatement) $ \(s, d) -> rulesFileCandidates s d >>= firstExistingFile
  preprocess <- hookExists (accountDir dirs) [relfile|preprocess|]
  construct <- hookExists (accountDir dirs) [relfile|construct|]
  journals <- forM statements $ \s -> do
    journal <- generatedOutput [reldir|3-journal|] ".journal" s
    exists <- PathIO.doesFileExist journal
    if exists
      then do
        outdated <- needsRegeneration s journal
        return (if outdated then Outdated else Current)
      else return Missing
  return
    AccountStatus
      { asAccount = name,
        asStatements = length statements,
        asYears = List.nub (List.sort (map yearOf statements)),
        asRulesFile = relative <$> rulesFile,
        asRulesCandidates = map relative defaults,
        asWithoutRules = length (filter isNothing resolved),
        asPreprocess = preprocess,
        asConstruct = construct,
        asJournalsMissing = length (filter (== Missing) journals),
        asJournalsOutdated = length (filter (== Outdated) journals)
      }
  where
    yearOf s = read (FilePath.dropTrailingPathSeparator (toFilePath (dirname (parent s))))

data JournalState = Current | Outdated | Missing deriving (Eq)

-- | Same hook names import looks for.
hookExists :: AbsDir -> RelFile -> IO Bool
hookExists dir hook = do
  let base = dir </> hook
  alternatives <-
    if Sys.os == "mingw32"
      then mapM (`addExtension` base) [".ps1", ".exe"]
      else return []
  not . null <$> filterM PathIO.doesFileExist (base : alternatives)
