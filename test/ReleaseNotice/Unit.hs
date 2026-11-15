{-# LANGUAGE OverloadedStrings #-}

module ReleaseNotice.Unit (tests) where

import Control.Concurrent.MVar
import Control.Exception (AsyncException (UserInterrupt), throwIO, try)
import Control.Monad (forM_, join)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Version (makeVersion, showVersion)
import Paths_hledger_flow (version)
import Hledger.Flow.Cache (initializeCache)
import Hledger.Flow.PathHelpers (AbsDir)
import Hledger.Flow.ReleaseNotice
import Path (toFilePath)
import Path.IO (withSystemTempDir)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getModificationTime, setModificationTime)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.HUnit
import TestHelpers (withTestDir)

-- Exercise a complete, deterministic refresh without the CLI's exit deadline.
checkRelease :: (String -> IO (Maybe BL.ByteString)) -> IO FilePath -> IO ReleaseResult
checkRelease get executable = withSystemTempDir "release-check" $ \base -> do
  initializeCache base
  refreshIfDue base getCurrentTime get executable
  cachedRelease base executable

recordFile :: AbsDir -> FilePath
recordFile base = toFilePath base </> ".hledger-flow/cache/release-check.json"

testTime :: UTCTime
testTime = read "2026-11-16 12:00:00 UTC"

tests :: Test
tests = TestLabel "release notice" $ TestList
  [ "invalid or missing responses cannot confirm release status" ~: TestCase $
      forM_ [Nothing, Just "invalid",
             Just "{\"tag_name\":\"v999-rc1\"}", Just "{\"tag_name\":\"v999x\"}",
             Just "{\"tag_name\":\"v999\",\"prerelease\":true}",
             Just "{\"tag_name\":\"v999\",\"draft\":true}"] $ \body -> do
        notice <- checkRelease (const $ pure body) (fail "unexpected ownership check")
        CheckUnavailable @=? notice
  , "installed version matches or exceeds the latest stable release" ~: TestCase $
      forM_ ["v0", "v" ++ showVersion version] $ \tag -> do
        let response = Just (encode (object ["tag_name" .= tag]))
        notice <- checkRelease (const $ pure response) (fail "unexpected ownership check")
        UpToDate @=? notice
  , "upgrade notice keeps its meaning with colour disabled" ~: TestCase $ do
      let installed = T.pack (showVersion version)
      ("  ↑ hledger-flow 999 is available (installed " <> installed <> ").\n    upgrade advice")
        @=? renderUpdate False (makeVersion [999]) "upgrade advice"
  , "verified Homebrew ownership and published formula" ~: TestCase $
      withTestDir $ \dir -> do
        let keg = toFilePath dir </> "Cellar" </> "hledger-flow" </> "1.2.3"
            exe = keg </> "bin" </> "hledger-flow"
            receipt = keg </> "INSTALL_RECEIPT.json"
            fetch formula url
              | url == "https://api.github.com/repos/apauley/hledger-flow/releases/latest" =
                  pure $ Just "{\"tag_name\":\"v999.10.0\"}"
              | url == "https://raw.githubusercontent.com/FernTrail/homebrew-tap/main/Formula/hledger-flow.rb" =
                  pure formula
              | otherwise = assertFailure ("unexpected URL " ++ url) >> pure Nothing
            check advice formula = do
              notice <- checkRelease (fetch formula) (pure exe)
              UpdateAvailable (makeVersion [999, 10, 0]) advice @=? notice
            published = Just "  tag = \"v999.10.0\"\n"
            releases = "https://github.com/apauley/hledger-flow/releases/latest"
        createDirectoryIfMissing True (keg </> "bin")
        writeFile exe ""
        check releases published
        writeFile receipt "{\"source\":{\"tap\":\"other/tap\"}}"
        check releases published
        writeFile receipt "{\"source\":{\"tap\":\"ferntrail/tap\"}}"
        check releases Nothing
        check releases (Just "  tag = \"v999.9.0\"\n")
        check "Upgrade with: brew update && brew upgrade ferntrail/tap/hledger-flow" published
  , "cached formula advice follows the current executable" ~: TestCase $
      withTestDir $ \base -> do
        let cache = toFilePath base </> ".hledger-flow/cache"
        createDirectoryIfMissing True cache
        writeFile (recordFile base) "{\"lastSuccess\":{\"latestVersion\":\"999\",\"checkedAt\":\"2026-11-16T12:00:00Z\",\"formulaConfirmed\":true}}"
        notice <- cachedRelease base (pure $ toFilePath base </> "hledger-flow")
        UpdateAvailable (makeVersion [999]) "https://github.com/apauley/hledger-flow/releases/latest" @=? notice
  , "missing cache stays absent on read and corrupt JSON can be refreshed" ~: TestCase $
      withTestDir $ \base -> do
        let cache = toFilePath base </> ".hledger-flow/cache"
            readOnly = cachedRelease base (pure "hledger-flow")
        assertEqual "no local notice" CheckUnavailable =<< readOnly
        assertEqual "read-only lookup" False =<< doesDirectoryExist cache
        createDirectoryIfMissing True cache
        writeFile (recordFile base) "partial JSON"
        assertEqual "corrupt cache" CheckUnavailable =<< readOnly
        notice <- join $ prepareCheck base (pure testTime) (const $ pure $ Just "{\"tag_name\":\"v0\"}") (pure "hledger-flow")
        UpToDate @=? notice
        let ignore = cache </> ".gitignore"
        assertEqual "shared ignore file repaired" "*\n" =<< readFile ignore
        setModificationTime ignore testTime
        timestamp <- getModificationTime ignore
        initializeCache base
        assertEqual "incremental initializer preserves ignore mtime" timestamp =<< getModificationTime ignore
  , "refresh intervals and offline fallback" ~: TestCase $ withTestDir $ \base -> do
      calls <- newIORef (0 :: Int)
      let executable = pure (toFilePath base </> "hledger-flow")
          fetch body _ = modifyIORef' calls (+1) >> pure body
          newer = Just "{\"tag_name\":\"v999\"}"
          at seconds = pure (addUTCTime seconds testTime)
          refresh seconds body = refreshIfDue base (at seconds) (fetch body) executable
          notice = UpdateAvailable (makeVersion [999]) "https://github.com/apauley/hledger-flow/releases/latest"
      first <- join $ prepareCheck base (at 0) (fetch newer) executable
      notice @=? first
      assertEqual "fresh result" notice =<< join (prepareCheck base (at 86399) (fetch Nothing) executable)
      assertEqual "only initial request" 1 =<< readIORef calls
      refresh 86400 Nothing
      assertEqual "offline result" notice =<< cachedRelease base executable
      assertEqual "status reads and failure cooldown" notice =<< join (prepareCheck base (at 87299) (fetch newer) executable)
      assertEqual "one failed refresh" 2 =<< readIORef calls
      refresh 87300 (Just "{\"tag_name\":\"v0\"}")
      assertEqual "successful retry" UpToDate =<< cachedRelease base executable
      assertEqual "retry after 15 minutes" 3 =<< readIORef calls
  , "cold command finishes while another refresh is pending" ~: TestCase $
      withTestDir $ \base -> do
        started <- newEmptyMVar
        release <- newEmptyMVar
        calls <- newIORef (0 :: Int)
        let fetch _ = modifyIORef' calls (+1) >> putMVar started () >> takeMVar release
            start = prepareCheck base (pure testTime) fetch (pure "hledger-flow")
        finish <- start
        takeMVar started
        overlapping <- start
        assertEqual "contending check finishes" (Just CheckUnavailable) =<< timeout 1000000 overlapping
        assertEqual "cold wait is bounded" (Just CheckUnavailable) =<< timeout 1000000 finish
        assertEqual "one fetch" 1 =<< readIORef calls
        putMVar release (Just "{\"tag_name\":\"v0\"}")
        assertEqual "finished worker published" UpToDate =<< finish
  , "ready notice follows diagnostics and preserves the exit code" ~: TestCase $ do
      events <- newIORef ([] :: [String])
      let record msg = modifyIORef' events (++ [msg])
      result <- try $ finishCommand (record "notice") (record "diagnostic" >> throwIO (ExitFailure 23))
      Left (ExitFailure 23) @=? result
      readIORef events >>= assertEqual "order" ["diagnostic", "notice"]
  , "interrupt skips a ready notice" ~: TestCase $ do
      result <- try $ finishCommand (assertFailure "printed on interrupt") (throwIO UserInterrupt)
      Left UserInterrupt @=? result
  , "notice write failure preserves success and failure" ~: TestCase $ do
      let broken = ioError (userError "closed stderr")
      finishCommand broken (pure ())
      result <- try $ finishCommand broken (throwIO (ExitFailure 19))
      Left (ExitFailure 19) @=? result
  ]
