{-# LANGUAGE OverloadedStrings #-}

module ReleaseNotice.Unit (tests) where

import Control.Concurrent.MVar
import Control.Exception (AsyncException (UserInterrupt), throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (encode, object, (.=))
import Data.IORef
import qualified Data.Text as T
import Data.Version (makeVersion, showVersion)
import Paths_hledger_flow (version)
import GHC.Clock (getMonotonicTimeNSec)
import Hledger.Flow.ReleaseNotice (NoticeTiming (..), ReleaseResult (..), checkRelease, finishCommand, renderStatusRelease, startCheck)
import Path (toFilePath)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.HUnit
import TestHelpers (withTestDir)

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
  , "status footer keeps its meaning with colour disabled" ~: TestCase $ do
      let installed = T.pack (showVersion version)
      ("  ✓ hledger-flow " <> installed <> " is up to date.") @=? renderStatusRelease False UpToDate
      ("  ↑ hledger-flow 999 is available (installed " <> installed <> ").\n    upgrade advice")
        @=? renderStatusRelease False (UpdateAvailable (makeVersion [999]) "upgrade advice")
      "  ⚠ Could not confirm the latest hledger-flow release.\n    Set HLEDGER_FLOW_NO_UPDATE_CHECK=1 to skip this check."
        @=? renderStatusRelease False CheckUnavailable
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
  , "command finishes with worker still pending" ~: TestCase $ do
      started <- newEmptyMVar
      release <- newEmptyMVar
      finish <- startCheck PollAtExit (putMVar started () >> takeMVar release)
      takeMVar started
      success <- timeout 1000000 $ finishCommand finish (pure ())
      failure <- timeout 1000000 $ try (finishCommand finish (throwIO (ExitFailure 37)))
      Just () @=? success
      Just (Left (ExitFailure 37)) @=? failure
      putMVar release UpToDate
  , "status gives a pending check one more second" ~: TestCase $ do
      release <- newEmptyMVar
      finish <- startCheck WaitAfterStatus (takeMVar release)
      start <- getMonotonicTimeNSec
      done <- timeout 2500000 finish
      end <- getMonotonicTimeNSec
      putMVar release UpToDate
      Just () @=? done
      assertBool "returned before the one-second grace period" (end - start >= 990000000)
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
