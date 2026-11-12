{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Start a new ledger.
module Hledger.Flow.Init
  ( InitOutcome (..),
    initLedger,
    runInit,
  )
where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Flow.BaseDir (findBaseDir)
import Data.Maybe (fromMaybe)
import Hledger.Flow.Common (Shell (..), homeDirs, homeShellPath, nativeShell, shellArgument, tildePath)
import Hledger.Flow.PathHelpers (AbsDir)
import Hledger.Flow.Status.Render (printNext)
import Hledger.Flow.Types (BaseDir)
import Path
import Path.IO (createDirIfMissing, doesDirExist)
import qualified System.FilePath as FilePath

data InitOutcome
  = -- | The new ledger, and an unrelated ledger above it, if any.
    Created BaseDir (Maybe BaseDir)
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
          createDirIfMissing True (dir </> [reldir|import|])
          return (Created dir above)

-- | Run @hledger-flow init@ for a folder, printing what happened and what's next.
runInit :: Maybe FilePath -> AbsDir -> AbsDir -> IO ()
runInit hledgerPath cwd dir = do
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
    Created base parentLedger -> do
      T.putStrLn ("Started a ledger in " <> shown base)
      mapM_ (\p -> T.putStrLn ("Note: " <> shown p <> " also has an import/ folder. Commands run inside " <> shown base <> " use the new ledger.")) parentLedger
      when (base /= cwd) $ do
        let changeDir = case nativeShell of
              Posix -> "cd "
              PowerShell -> "Set-Location -LiteralPath "
            path = absolute base
        T.putStrLn ("Change to it first: " <> changeDir <> fromMaybe (shellArgument nativeShell (T.pack path)) (homeShellPath nativeShell homes path))
      printNext hledgerPath base
