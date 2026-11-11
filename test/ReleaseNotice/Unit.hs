{-# LANGUAGE OverloadedStrings #-}

module ReleaseNotice.Unit (tests) where

import Control.Concurrent.MVar
import Control.Exception (AsyncException (UserInterrupt), throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (encode, object, (.=))
import Data.IORef
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_hledger_flow (version)
import Hledger.Flow.ReleaseNotice (checkRelease, finishCommand, startCheck)
import Path (toFilePath)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.HUnit
import TestHelpers (withTestDir)

tests :: Test
tests = TestLabel "release notice" $ TestList
  [ "ignore malformed, nonstable and non-newer releases" ~: TestCase $
      forM_ [Nothing, Just "invalid", Just "{\"tag_name\":\"v0\"}",
             Just (encode (object ["tag_name" .= ("v" ++ showVersion version)])),
             Just "{\"tag_name\":\"v999-rc1\"}", Just "{\"tag_name\":\"v999x\"}",
             Just "{\"tag_name\":\"v999\",\"prerelease\":true}",
             Just "{\"tag_name\":\"v999\",\"draft\":true}"] $ \body -> do
        notice <- checkRelease (const $ pure body) (fail "unexpected ownership check")
        Nothing @=? notice
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
              assertBool ("expected " ++ T.unpack advice) (maybe False (T.isInfixOf advice) notice)
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
        check "brew update && brew upgrade ferntrail/tap/hledger-flow" published
  , "command finishes with worker still pending" ~: TestCase $ do
      started <- newEmptyMVar
      release <- newEmptyMVar
      finish <- startCheck (putMVar started () >> takeMVar release)
      takeMVar started
      success <- timeout 1000000 $ finishCommand finish (pure ())
      failure <- timeout 1000000 $ try (finishCommand finish (throwIO (ExitFailure 37)))
      Just () @=? success
      Just (Left (ExitFailure 37)) @=? failure
      putMVar release Nothing
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
