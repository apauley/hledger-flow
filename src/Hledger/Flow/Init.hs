{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Start a new ledger.
module Hledger.Flow.Init
  ( InitOutcome (..),
    initLedger,
    runInit,
  )
where

import Control.Monad (forM, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.BaseDir (findBaseDir)
import Data.Maybe (fromMaybe)
import Hledger.Flow.Common (Shell (..), findHledgerConf, homeDirs, homeShellPath, nativeShell, shellArgument, tildePath)
import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Init.Example (exampleBaseFiles, exampleImportFiles)
import Hledger.Flow.PathHelpers (AbsDir, RelFile, pathOccupied, writeNewUtf8File)
import Hledger.Flow.Status.Render (printNext)
import Hledger.Flow.Types (BaseDir)
import Path
import Path.IO (createDirIfMissing, doesDirExist, doesFileExist, renameDir, withTempDir)
import System.IO.Error (tryIOError)
import qualified System.FilePath as FilePath

data InitOutcome
  = -- | The new ledger, and an unrelated ledger above it, if any.
    Created BaseDir (Maybe BaseDir) [(RelFile, Bool)]
  | -- | The folder already has @import/@.
    AlreadyLedger BaseDir
  | -- | The folder is inside this ledger's @import/@ tree.
    InsideLedger BaseDir
  deriving (Eq, Show)

-- | Create @DIR/import/@ unless that would nest inside another ledger.
initLedger :: AbsDir -> IO InitOutcome
initLedger dir = do
  hasImport <- doesDirExist (dir </> [reldir|import|])
  if hasImport
    then return (AlreadyLedger dir)
    else do
      above <- findBaseDir dir
      case above of
        Just b | let imp = b </> [reldir|import|], imp == dir || imp `isProperPrefixOf` dir -> return (InsideLedger b)
        _ -> do
          year <- currentYear
          createDirIfMissing True dir
          baseFiles <- forM (exampleBaseFiles year) $ \(name, contents) -> do
            let target = dir </> name
            occupied <- pathOccupied target
            if occupied
              then do
                isFile <- doesFileExist target
                if isFile
                  then return (name, False)
                  else ioError (userError ("Cannot create starter file at " ++ toFilePath target))
              else do
                result <- tryIOError (writeNewUtf8File target contents)
                case result of
                  Right () -> return (name, True)
                  Left err -> do
                    exists <- doesFileExist target
                    if exists then return (name, False) else ioError err
          withTempDir dir ".hledger-flow-init-" $ \temp -> do
            let staged = temp </> [reldir|import|]
            mapM_ (\(name, contents) -> writeNewUtf8File (staged </> name) contents) (exampleImportFiles year)
            renameDir staged (dir </> [reldir|import|])
          return (Created dir above baseFiles)

-- | Run @hledger-flow init@ for a folder, printing what happened and what's next.
runInit :: Maybe FilePath -> Maybe FilePath -> AbsDir -> AbsDir -> IO ()
runInit hledgerPath confOption cwd dir = do
  homes <- homeDirs
  let absolute = FilePath.dropTrailingPathSeparator . toFilePath
      shown = T.pack . tildePath homes . absolute
  outcome <- initLedger dir
  case outcome of
    AlreadyLedger b ->
      ioError . userError . T.unpack $
        shown b <> " is already a hledger-flow ledger. Run `hledger-flow status` there to see where it stands."
    InsideLedger b ->
      ioError . userError . T.unpack $
        "That folder is inside the ledger at " <> shown b <> ". Start a new ledger outside its import/ folder."
    Created base parentLedger baseFiles -> do
      T.putStrLn ("Started a ledger in " <> shown base)
      T.putStrLn "Created an example account (import/me/mybank/checking) and starter journals for manual entries and balances."
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
