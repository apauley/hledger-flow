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

import Control.Exception (throwIO)
import Control.Monad (forM, unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.BaseDir (findBaseDir)
import Data.Maybe (fromMaybe)
import Hledger.Flow.Common (Shell (..), findHledgerConf, homeDirs, homeShellPath, nativeShell, shellArgument, tildePath)
import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Init.Example (exampleAccount, exampleAccountPath, exampleBaseFiles, exampleImportFiles, starterOwner)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, PathException (CannotCreate), RelDir, RelFile, pathOccupied, writeNewUtf8File)
import Hledger.Flow.Status.Render (printStarterNext)
import Hledger.Flow.Types (BaseDir)
import Path
import Path.IO (doesDirExist, doesFileExist, renameDir, withTempDir)
import System.IO.Error (tryIOError)
import qualified System.FilePath as FilePath

data InitMode = WriteLedger | DryRun deriving (Eq, Show)

data InitOutcome
  = -- | Parent ledger (if any), example account, and starter files marked True if new.
    StarterLedger (Maybe BaseDir) RelDir [(RelFile, Bool)]
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
              imports = [(importDir </> name, contents) | (name, contents) <- exampleImportFiles owner year]
          occupiedImport <- pathOccupied (dir </> importDir)
          when occupiedImport $ throwIO (CannotCreate "import directory" (toFilePath (dir </> importDir)))
          baseFiles <- forM (exampleBaseFiles year) $ \(name, contents) -> do
            created <- initBaseFile mode (dir </> name) contents
            return (name, created)
          when (mode == WriteLedger) $
            withTempDir dir ".hledger-flow-init-" $ \temp -> do
              mapM_ (\(name, contents) -> writeNewUtf8File (temp </> name) contents) imports
              renameDir (temp </> importDir) (dir </> importDir)
          return (StarterLedger above (exampleAccount owner) (baseFiles ++ [(name, True) | (name, _) <- imports]))

-- | Whether this run creates (or would create) a starter file outside import/.
initBaseFile :: InitMode -> AbsFile -> T.Text -> IO Bool
initBaseFile mode target contents = do
  checkDirectory (parent target)
  occupied <- pathOccupied target
  if occupied
    then do
      isFile <- doesFileExist target
      unless isFile $ throwIO (CannotCreate "starter file" (toFilePath target))
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
    when occupied $ throwIO (CannotCreate "directory" (toFilePath dir))
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
    StarterLedger parentLedger account files -> do
      T.putStrLn ((if mode == DryRun then "Dry run for " else "Started a ledger in ") <> shown dir)
      T.putStrLn ("Example account: " <> exampleAccountPath account)
      mapM_ (\(name, new) -> T.putStrLn (fileAction mode new <> T.pack (toFilePath name))) files
      when (mode == WriteLedger) $ do
        mapM_ (\p -> T.putStrLn ("Note: " <> shown p <> " also has an import/ folder. Commands run inside " <> shown dir <> " use the new ledger.")) parentLedger
        when (dir /= cwd) $ do
          let changeDir = case nativeShell of
                Posix -> "cd "
                PowerShell -> "Set-Location -LiteralPath "
              path = absolute dir
          T.putStrLn ("Change to it first: " <> changeDir <> fromMaybe (shellArgument nativeShell (T.pack path)) (homeShellPath nativeShell homes path))
        conf <- findHledgerConf confOption
        printStarterNext hledgerPath conf dir

fileAction :: InitMode -> Bool -> T.Text
fileAction DryRun True = "Would create "
fileAction DryRun False = "Would keep existing "
fileAction WriteLedger True = "Created "
fileAction WriteLedger False = "Kept existing "
