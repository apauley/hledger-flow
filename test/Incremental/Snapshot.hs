{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Incremental.Snapshot (tests, runWorker) where

import Control.Concurrent
import Control.Concurrent.STM (newTChanIO)
import Control.Exception (bracket, try, IOException, throwIO)
import Control.Monad (unless)
import GHC.Conc (threadStatus, ThreadStatus(..))
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, utf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Hledger.Flow.Import.Cache
import Hledger.Flow.Import.Dependencies
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.Import.ImportHelpers (extractImportDirs)
import Hledger.Flow.Import.Incremental
import Hledger.Flow.Import.PrivateDirectory
import Hledger.Flow.Import.Snapshot
import Hledger.Flow.RuntimeOptions
import Path
import qualified Path.IO as P
import qualified System.Directory as D
#ifdef mingw32_HOST_OS
import Data.Word (Word32)
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.String (CWString, peekCWString, withCWString)
import Foreign.Marshal.Array (allocaArray)
#endif
import qualified System.FilePath as FP
import System.Environment (getExecutablePath)
import System.IO (hFlush, hGetLine, stdout)
import System.Process
import System.Timeout (timeout)
import Test.HUnit
import TestHelpers (defaultOpts, withTestDir)

tests :: Test
tests = TestLabel "Private input snapshots" $ TestList
  [ TestLabel "no snapshot area for a hit" $ TestCase $ withTestDir $ \base -> do
      withSnapshotRun base $ \_ -> pure ()
      P.doesDirExist (base </> [reldir|.hledger-flow/cache/snapshots|]) >>= assertBool "created lazily" . not
  , TestLabel "copy selected bytes and translate locations" $ TestCase $ withTestDir $ \base -> do
      let statement = base </> [relfile|account/2026/input.csv|]
          rules = base </> [relfile|account/bank.rules|]
          category = base </> [relfile|shared/coffee.rules|]
          directives = base </> [relfile|directives.journal|]
          accounts = base </> [relfile|shared/accounts.journal|]
          a = "account2 expenses:cafe\n"
      P.createDirIfMissing True (parent statement)
      P.createDir (parent category)
      BS.writeFile (toFilePath statement) "2026-01-01,Coffee,3\n"
      BS.writeFile (toFilePath rules) "include ../shared/coffee.rules\n"
      BS.writeFile (toFilePath category) a
      BS.writeFile (toFilePath directives) "include shared/accounts.journal\n"
      BS.writeFile (toFilePath accounts) "account expenses:cafe\n"
      result <- withSnapshotRun base $ \run -> withSnapshot run V1524 (RulesInput statement rules (Just directives) Nothing) $ \snapshot -> do
        RulesInput dataCopy rulesCopy (Just directivesCopy) Nothing <- pure (snapshotInputs snapshot)
        let expected = [statement, rules, category, directives, accounts]
        digests <- mapM digestFile expected
        assertEqual "manifest follows the order of conversion inputs"
          (zip expected digests) (snapshotManifest snapshot)
        BS.writeFile (toFilePath accounts) "account expenses:food\n"
        BS.readFile (toFilePath (parent directivesCopy </> [relfile|shared/accounts.journal|]))
          >>= assertEqual "private copy retains original declarations" "account expenses:cafe\n"
        assertEqual "CSV basename" (filename statement) (filename dataCopy)
        canonicalCopy <- D.canonicalizePath (toFilePath rulesCopy)
        let location = ":1:2:\n"
            excerpt = "1 | " <> T.pack (toFilePath rulesCopy) <> "\n"
        assertEqual "maps location, preserves excerpt" ("hledger: Error: " <> T.pack (toFilePath rules) <> location <> excerpt)
           =<< translateDiagnostic snapshot ("hledger: Error: " <> T.pack canonicalCopy <> location <> excerpt)
        let leafCopy = parent (parent rulesCopy) </> [relfile|shared/coffee.rules|]
        pure [dataCopy, rulesCopy, leafCopy, directivesCopy]
      case result of
        Reusable files -> mapM_ (\p -> P.doesFileExist p >>= assertBool "copy released" . not) files
        Fallback reason -> assertFailure (show reason)
  , TestLabel "publish only when converted snapshot matches live inputs" $ TestCase snapshotPublication
  , TestLabel "construct pins an external config" $ TestCase $ withTestDir $ \base -> withTestDir $ \home -> do
      let config = home </> [relfile|hledger.conf|]
      BS.writeFile (toFilePath config) "[balance]\n--period=thismonth\n"
      result <- withSnapshotRun base $ \run -> withSnapshot run V1996 (ConstructInput (Just config)) $ \snapshot -> do
        ConstructInput (Just copied) <- pure (snapshotInputs snapshot)
        assertBool "separate private copy" (config /= copied)
        assertEqual "config path" (T.pack (toFilePath config) <> ":6:3:")
           =<< translateDiagnostic snapshot (T.pack (toFilePath copied) <> ":6:3:")
        let windowsError p = "hledger.exe: Error: " <> T.pack (toFilePath p) <> ":6:3:"
        assertEqual "Windows hledger diagnostic" (windowsError config)
           =<< translateDiagnostic snapshot (windowsError copied)
        BS.readFile (toFilePath copied) >>= assertEqual "whole-file content" "[balance]\n--period=thismonth\n"
      case result of
        Reusable () -> pure ()
        Fallback reason -> assertFailure (show reason)
  , TestLabel "conversion errors propagate and clean up" $ TestCase $ withTestDir $ \base -> do
      result <- try $ withSnapshotRun base $ \run ->
        withSnapshot run V1524 (ConstructInput Nothing) $ \_ -> ioError (userError "converter failed")
      case result :: Either IOException (ReuseDecision ()) of
        Left _ -> pure ()
        Right _ -> assertFailure "swallowed converter error"
      names <- D.listDirectory (toFilePath (base </> [reldir|.hledger-flow/cache/snapshots|]))
      assertBool "only registration lock survives" (length names <= 1)
#ifndef mingw32_HOST_OS
  , TestLabel "canonical snapshot below symlink maps to original spelling" $ TestCase $ withTestDir $ \root -> do
      let base = root </> [reldir|ledger alias|]
          real = root </> [reldir|ledger|]
          config = base </> [relfile|config with spaces|]
      P.createDir real
      D.createDirectoryLink (toFilePath real) (FP.dropTrailingPathSeparator (toFilePath base))
      BS.writeFile (toFilePath config) "# ok\n"
      result <- withSnapshotRun base $ \run -> withSnapshot run V1524 (ConstructInput (Just config)) $ \snap -> do
        ConstructInput (Just copied) <- pure (snapshotInputs snap)
        canonical <- D.canonicalizePath (toFilePath copied)
        assertBool "release canonicalized the ancestor" (canonical /= toFilePath copied)
        let ruleLine fileName = "the date rule is:   %1                   (" <> T.pack fileName <> ":7)"
        let err name = "hledger: Error: " <> T.dropEnd 1 (T.pack (show name)) <> ":2:2:"
            warning name = "Warning: " <> T.pack name <> ": replaced delimiter"
        assertEqual "quoted config diagnostic" (err (toFilePath config)) =<< translateDiagnostic snap (err canonical)
        assertEqual "CSV warning" (warning (toFilePath config)) =<< translateDiagnostic snap (warning canonical)
        assertEqual "diagnostic location" (ruleLine (toFilePath config))
          =<< translateDiagnostic snap (ruleLine canonical)
      assertEqual "converted" (Reusable ()) result
#else
  , TestLabel "260 UTF-16 units in data, include and pinned config" $ TestCase $ withTestDir $ \base -> do
      let csv = base </> [relfile|in.csv|]
          rules = base </> [relfile|in.rules|]
          fixed copied = toFilePath base FP.</> ".hledger-flow" FP.</> "cache" FP.</> "snapshots"
            FP.</> "r00000000" FP.</> "t00000000" FP.</> copied
          runFor part lengthWanted = do
            let folder = if part == "config" then "c" else "l"
                suffix = if part == "CSV" then "😀.csv" else "😀.rules"
                units = sum . map (\c -> if fromEnum c > 65535 then 2 else 1)
                padding = lengthWanted - units (fixed (folder FP.</> suffix))
            assertBool "fixture has room for the filename" (padding >= 0)
            file <- (base </>) <$> parseRelFile (replicate padding 'x' ++ suffix)
            BS.writeFile (toFilePath file) ""
            BS.writeFile (toFilePath csv) "2026-01-01,coffee,3"
            if part == "include"
              then BS.writeFile (toFilePath rules) (encodeUtf8 ("include " <> T.pack (toFilePath (filename file)) <> "\n"))
              else writeFile (toFilePath rules) ""
            let input = case part of
                  "CSV" -> RulesInput file rules Nothing Nothing
                  "include" -> RulesInput csv rules Nothing Nothing
                  _ -> ConstructInput (Just file)
            result <- withSnapshotRun base $ \run -> withSnapshot run V1996 input (const (pure ()))
            case (lengthWanted, result) of
              (259, Reusable ()) -> pure ()
              (260, Fallback reason) -> assertEqual part "overlong Windows snapshot path" (reasonScope reason)
              _ -> assertFailure (part ++ " at " ++ show lengthWanted ++ ": " ++ show result)
      mapM_ (\part -> mapM_ (runFor part) [259,260]) ["CSV","include","config"]
  , TestLabel "short filename diagnostic when available" $ TestCase $ withTestDir $ \base -> do
      let config = base </> [relfile|config containing spaces|]
      BS.writeFile (toFilePath config) "# config\n"
      result <- withSnapshotRun base $ \run -> withSnapshot run V1524 (ConstructInput (Just config)) $ \snap -> do
        ConstructInput (Just copied) <- pure (snapshotInputs snap)
        withCWString (toFilePath copied) $ \long -> allocaArray 260 $ \buffer -> do
          n <- getShortPath long buffer 260
          if n == 0 || n >= 260 then pure () else do
            short <- peekCWString buffer
            assertEqual "resolved file identity" (T.pack (toFilePath config) <> ":4:2:")
              =<< translateDiagnostic snap (T.pack short <> ":4:2:")
      assertEqual "converted" (Reusable ()) result
#endif
  , TestLabel "permission failure makes no copies" $ TestCase $ withTestDir $ \base -> do
      let deny _ = ioError (userError "permission denied")
      result <- withSnapshotRunUsing deny base $ \run ->
        withSnapshot run V1524 (ConstructInput Nothing) $ \_ -> assertFailure "ran a private conversion"
      case result of
        Fallback r -> assertBool "private staging reason" ("private staging unavailable" `T.isPrefixOf` reasonScope r)
        Reusable _ -> assertFailure "admitted unprotected staging"
      D.listDirectory (toFilePath (base </> [reldir|.hledger-flow/cache/snapshots|])) >>= assertEqual "no financial data" ["registration.lock"]
  , TestLabel "registration protects an initializing directory" $ TestCase $ withTestDir $ \base -> do
      created <- newEmptyMVar
      proceed <- newEmptyMVar
      initializer <- newEmptyMVar
      cleaner <- newEmptyMVar
      let suspended dir = createPrivateDirectory dir >> putMVar created () >> takeMVar proceed
      _ <- forkFinally (withSnapshotRunUsing suspended base $ \run ->
        withSnapshot run V1524 (ConstructInput Nothing) (const (pure ()))) (putMVar initializer)
      takeMVar created
      tid <- forkFinally (withSnapshotRun base (const (pure ()))) (putMVar cleaner)
      let blocked = do
            status <- threadStatus tid
            case status of
              ThreadBlocked _ -> pure True
              ThreadFinished -> pure False
              ThreadDied -> pure False
              ThreadRunning -> yield >> blocked
      waiting <- timeout 5000000 blocked
      putMVar proceed ()
      first <- takeMVar initializer >>= either throwIO pure
      takeMVar cleaner >>= either throwIO pure
      assertEqual "cleanup waited for registration" (Just True) waiting
      assertEqual "initializer consumed its own tree" (Reusable ()) first
  , TestLabel "reap killed worker, preserve live and unknown directories" $ TestCase $ withTestDir $ \base -> do
      let config = base </> [relfile|hledger.conf|]
      BS.writeFile (toFilePath config) "# private config\n"
      exe <- getExecutablePath
      let child = (proc exe ["--snapshot-test-worker", toFilePath base]) {std_in = CreatePipe, std_out = CreatePipe}
      result <- withSnapshotRun base $ \active -> withSnapshot active V1524 (ConstructInput (Just config)) $ \snap -> do
        ConstructInput (Just live) <- pure (snapshotInputs snap)
        let unknown = base </> [reldir|.hledger-flow/cache/snapshots/r01234567|]
            keep = unknown </> [relfile|unrelated|]
        P.createDir unknown
        writeFile (toFilePath keep) "preserve"
        withCreateProcess child $ \_ out _ pid -> do
          Just stdoutPipe <- pure out
          copiedName <- hGetLine stdoutPipe
          abandoned <- parseAbsFile copiedName
          terminateProcess pid
          _ <- waitForProcess pid
          P.doesFileExist abandoned >>= assertBool "private data survived unhandled termination"
          withSnapshotRun base (const (pure ()))
          P.doesFileExist abandoned >>= assertBool "abandoned copy reclaimed" . not
          P.doesFileExist keep >>= assertBool "unknown directory preserved"
          BS.readFile (toFilePath live) >>= assertEqual "other run untouched" "# private config\n"
      assertEqual "live run completes" (Reusable ()) result
  , TestLabel "different include spellings cannot overwrite a snapshot file" $ TestCase $ withTestDir $ \base -> do
      let csv = base </> [relfile|input.csv|]
          rules = base </> [relfile|bank.rules|]
          upper = [relfile|A.rules|]
          lower = [relfile|a.rules|]
      BS.writeFile (toFilePath csv) "2026-01-01,Coffee,3\n"
      BS.writeFile (toFilePath rules) "include A.rules\ninclude a.rules\n"
      BS.writeFile (toFilePath (base </> upper)) "account1 assets:bank\n"
      sameFile <- P.doesFileExist (base </> lower)
      BS.writeFile (toFilePath (base </> lower)) "account2 expenses:cafe\n"
      result <- withSnapshotRun base $ \run -> withSnapshot run V1996 (RulesInput csv rules Nothing Nothing) $ \snap -> do
        RulesInput _ staged _ _ <- pure (snapshotInputs snap)
        mapM (BS.readFile . toFilePath . (parent staged </>)) [upper,lower]
      case result of
        Fallback reason -> assertEqual "fallback reason" "snapshot filenames collide" (reasonScope reason)
        Reusable contents -> do
          assertBool "aliased destinations must fall back" (not sameFile)
          assertEqual "distinct copies preserve each set of rules" ["account1 assets:bank\n","account2 expenses:cafe\n"] contents
  , TestLabel "Windows uses UTF-16 units without the NUL" $ TestCase $ do
      assertBool "259" (windowsSnapshotPathFits (replicate 257 'a' ++ "😀"))
      assertBool "260" (not (windowsSnapshotPathFits (replicate 258 'a' ++ "😀")))
  ]

-- Run the real importer with this native test executable as its converter.
-- Its print command changes the live include while consuming the supplied rules.
snapshotPublication :: IO ()
snapshotPublication = withTestDir $ \base -> P.withCurrentDir base $
  bracket getLocaleEncoding setLocaleEncoding $ \_ -> do
    setLocaleEncoding utf8
    exe <- getExecutablePath >>= parseAbsFile
    let opts = (defaultOpts base) {hledgerExecutable = exe, importPolicy = RunScripts}
        root = base </> [reldir|import|]
        source = root </> [relfile|o/b/a/1-in/2026/in.csv|]
        leaf = root </> [relfile|shared/category.rules|]
        a = "account2 expenses:cafe\n"
    P.createDirIfMissing True (parent source)
    P.createDir (parent leaf)
    BS.writeFile (toFilePath source) "2026-01-01,Coffee,3\n"
    BS.writeFile (toFilePath (root </> [relfile|b.rules|])) "include shared/category.rules\n"
    BS.writeFile (toFilePath leaf) a
    BS.writeFile "hledger.conf" ""
    dirs <- either (assertFailure . T.unpack) pure (extractImportDirs root source)
    ch <- newTChanIO
    let run = withIncrementalRun opts ch 1 $ \ctx -> importStatement ctx opts ch dirs source
        journal = "2026-01-01 Coffee\n    expenses:cafe  3\n    assets:bank\n"
    record <- recordPath base source
    BS.writeFile "snapshot-converter-test" "restore"
    (out,_,_) <- run
    readFile (toFilePath out) >>= assertEqual "journal built from copied rule A" journal
    entry <- readRecord record >>= maybe (assertFailure "missing journal record") pure . (>>= journalRecord)
    lookup (T.pack (toFilePath leaf)) (stageInputs entry) @?= Just (digestHex (digestBytes a))
    digestFile out >>= assertEqual "record identifies installed journal bytes" (stageOutput entry)
    -- Force a rebuild with an existing record. Leaving B in place must also
    -- discard the old A record, even though the snapshot produced A again.
    P.removeFile out
    BS.writeFile "snapshot-converter-test" "keep"
    _ <- run
    readFile (toFilePath out) >>= assertEqual "kept journal still built from A" journal
    BS.readFile (toFilePath leaf) >>= assertEqual "live include remains B" "account2 expenses:travel\n"
    readRecord record >>= assertEqual "invalidated journal record" Nothing . (>>= journalRecord)

-- The real test executable owns the lifetime lock. Its parent terminates it
-- abruptly, so no in-process finalizer can remove the abandoned tree.
runWorker :: [String] -> IO Bool
runWorker ["--snapshot-test-worker", baseName] = do
  base <- parseAbsDir baseName
  let config = base </> [relfile|hledger.conf|]
  result <- withSnapshotRun base $ \run -> withSnapshot run V1524 (ConstructInput (Just config)) $ \snapshot -> do
    ConstructInput (Just copied) <- pure (snapshotInputs snapshot)
    putStrLn (toFilePath copied)
    hFlush stdout
    _ <- getLine
    pure ()
  case result of
    Fallback reason -> ioError (userError (show reason))
    Reusable () -> pure True
runWorker args = do
  fixture <- D.doesFileExist "snapshot-converter-test"
  if fixture then fixtureConverter args >> pure True else pure False

fixtureConverter :: [String] -> IO ()
fixtureConverter ["--version"] = putStrLn "hledger 1.99.6"
fixtureConverter ["--conf",_,"print","--explicit","--file",_,"--rules-file",rules] = do
  D.renameDirectory "import/shared" "import/held"
  D.createDirectory "import/shared"
  BS.writeFile "import/shared/category.rules" "account2 expenses:travel\n"
  ["include",relative] <- words . B8.unpack <$> BS.readFile rules
  ["account2",account] <- words . B8.unpack <$> BS.readFile (FP.takeDirectory rules FP.</> relative)
  keep <- (== "keep") <$> BS.readFile "snapshot-converter-test"
  unless keep $ do
    D.removeDirectoryRecursive "import/shared"
    D.renameDirectory "import/held" "import/shared"
  putStr $ "2026-01-01 Coffee\n    " ++ account ++ "  3\n    assets:bank\n"
fixtureConverter args = ioError (userError ("Unexpected converter arguments: " ++ show args))

#ifdef mingw32_HOST_OS
foreign import capi unsafe "windows.h GetShortPathNameW"
  getShortPath :: CWString -> CWString -> Word32 -> IO Word32
#endif
