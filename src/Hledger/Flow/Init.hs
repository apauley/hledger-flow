{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Start a new ledger.
module Hledger.Flow.Init
  ( InitMode (..),
    InitOutcome (..),
    initLedger,
    runInit,
  )
where

import Control.Monad (forM, unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.BaseDir (findBaseDir)
import Data.Maybe (fromMaybe)
import Hledger.Flow.Common (Shell (..), findHledgerConf, homeDirs, homeShellPath, nativeShell, shellArgument, tildePath)
import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Init.Example (exampleAccount, exampleAccountPath, exampleBaseFiles, exampleImportFiles, starterOwner)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, pathOccupied, writeNewUtf8File)
import Hledger.Flow.Status.Render (printNext)
import Hledger.Flow.Types (BaseDir)
import Path
import Path.IO (doesDirExist, doesFileExist, renameDir, withTempDir)
import System.IO.Error (tryIOError)
import qualified System.FilePath as FilePath

data InitMode = WriteLedger | DryRun deriving (Eq, Show)

data InitOutcome
  = -- | The new ledger, and an unrelated ledger above it, if any.
    Created BaseDir (Maybe BaseDir) RelDir [(RelFile, Bool)]
  | -- | Starter paths relative to the destination, marked True if new.
    Preview [(RelFile, Bool)]
  | -- | The folder already has @import/@.
    AlreadyLedger BaseDir
  | -- | The folder is inside this ledger's @import/@ tree.
    InsideLedger BaseDir
  deriving (Eq, Show)

-- | Create @DIR/import/@ unless that would nest inside another ledger.
initLedger :: InitMode -> AbsDir -> IO InitOutcome
initLedger mode dir = do
  hasImport <- doesDirExist (dir </> [reldir|import|])
  if hasImport
    then return (AlreadyLedger dir)
    else do
      above <- findBaseDir dir
      case above of
        Just b | let imp = b </> [reldir|import|], imp == dir || imp `isProperPrefixOf` dir -> return (InsideLedger b)
        _ -> do
          year <- currentYear
          owner <- starterOwner
          let importDir = [reldir|import|]
              imports = exampleImportFiles owner year
          occupiedImport <- pathOccupied (dir </> importDir)
          when occupiedImport $ ioError (userError ("Cannot create import directory at " ++ toFilePath (dir </> importDir)))
          baseFiles <- forM (exampleBaseFiles year) $ \(name, contents) -> do
            created <- initBaseFile mode (dir </> name) contents
            return (name, created)
          case mode of
            DryRun -> return (Preview (baseFiles ++ [(importDir </> name, True) | (name, _) <- imports]))
            WriteLedger -> do
              withTempDir dir ".hledger-flow-init-" $ \temp -> do
                let staged = temp </> importDir
                mapM_ (\(name, contents) -> writeNewUtf8File (staged </> name) contents) imports
                renameDir staged (dir </> importDir)
              return (Created dir above (exampleAccount owner) baseFiles)

-- | Whether this run creates (or would create) a starter file outside import/.
initBaseFile :: InitMode -> AbsFile -> T.Text -> IO Bool
initBaseFile mode target contents = do
  checkDirectory (parent target)
  occupied <- pathOccupied target
  if occupied
    then do
      isFile <- doesFileExist target
      unless isFile $ ioError (userError ("Cannot create starter file at " ++ toFilePath target))
      return False
    else case mode of
      DryRun -> return True
      WriteLedger -> do
        result <- tryIOError (writeNewUtf8File target contents)
        case result of
          Right () -> return True
          Left err -> do
            exists <- doesFileExist target
            if exists then return False else ioError err

-- A missing directory can be created only if its nearest existing ancestor is a directory.
checkDirectory :: AbsDir -> IO ()
checkDirectory dir = do
  exists <- doesDirExist dir
  unless exists $ do
    occupied <- pathOccupied dir
    when occupied $ ioError (userError ("Cannot create directory at " ++ toFilePath dir))
    when (parent dir /= dir) $ checkDirectory (parent dir)

-- | Run @hledger-flow init@ for a folder, printing what happened and what's next.
runInit :: InitMode -> Maybe FilePath -> Maybe FilePath -> AbsDir -> AbsDir -> IO ()
runInit mode hledgerPath confOption cwd dir = do
  homes <- homeDirs
  let absolute = FilePath.dropTrailingPathSeparator . toFilePath
      shown = T.pack . tildePath homes . absolute
  outcome <- initLedger mode dir
  case outcome of
    AlreadyLedger b ->
      ioError . userError . T.unpack $
        shown b <> " is already a hledger-flow ledger. Run `hledger-flow status` there to see where it stands."
    InsideLedger b ->
      ioError . userError . T.unpack $
        "That folder is inside the ledger at " <> shown b <> ". Start a new ledger outside its import/ folder."
    Preview files -> do
      T.putStrLn ("Dry run for " <> shown dir)
      mapM_ (\(name, new) -> T.putStrLn ((if new then "Would create " else "Would keep existing ") <> T.pack (toFilePath name))) files
    Created base parentLedger account baseFiles -> do
      T.putStrLn ("Started a ledger in " <> shown base)
      T.putStrLn ("Created an example account (" <> exampleAccountPath account <> ") and starter journals for manual entries and balances.")
      mapM_ (\(name, created) -> T.putStrLn ((if created then "Created " else "Kept existing ") <> T.pack (toFilePath name))) baseFiles
      mapM_ (\p -> T.putStrLn ("Note: " <> shown p <> " also has an import/ folder. Commands run inside " <> shown base <> " use the new ledger.")) parentLedger
      when (base /= cwd) $ do
        let changeDir = case nativeShell of
              Posix -> "cd "
              PowerShell -> "Set-Location -LiteralPath "
            path = absolute base
        T.putStrLn ("Change to it first: " <> changeDir <> fromMaybe (shellArgument nativeShell (T.pack path)) (homeShellPath nativeShell homes path))
      conf <- findHledgerConf confOption
      printNext hledgerPath conf base
