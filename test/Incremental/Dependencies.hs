{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Incremental.Dependencies (tests) where

import Control.Monad (forM_)
import Control.Exception (bracket)
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, utf8, latin1)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hledger.Flow.Import.Dependencies
import Hledger.Flow.Import.Fingerprint (digestBytes)
import Hledger.Flow.Import.ImportHelpers (extractImportDirs)
import Hledger.Flow.Types (HledgerConf(..))
import Path
import qualified Path.IO as P
import qualified System.Directory as D
import System.Exit (ExitCode(..))
import Data.Time (addUTCTime)
import Test.HUnit hiding (path)
import TestHelpers (withTestDir)

fallback :: Show a => ReuseDecision a -> Assertion
fallback (Fallback _) = pure ()
fallback r = assertFailure ("expected fallback, got " ++ show r)

tests :: Test
tests = TestList
  [ "include expansion and selection" ~: withTestDir (\base -> do
      let root = base </> [relfile|main.rules|]
          mid = base </> [relfile|middle.rules|]
          leaf = base </> [relfile|empty.rules|]
          mainBytes = "include  middle.rules\r\ninclude empty.rules\n"
          midBytes = "include empty.rules"
          write p = B.writeFile (toFilePath p)
      write root mainBytes
      write mid midBytes
      write leaf ""
      forM_ [V1524,V1996] $ \release -> do
        tree <- discoverRules base release root
        tree @?= Reusable [(root,digestBytes mainBytes),(mid,digestBytes midBytes),(leaf,digestBytes ""),(leaf,digestBytes "")]
      ruleIncludes V1524 root "include   a  \r\n include b\ninclude\tc\n" @?= Reusable ["a  "]
      ruleIncludes V1996 root "include   a  \r\n include b\ninclude\tc\n" @?= Reusable ["a"]
      fallback (nestedRuleIncludes V1524 mid "include final.rules\r\n")
      nestedRuleIncludes V1996 mid "include final.rules\r\n" @?= Reusable ["final.rules"]
      write root "include middle.rules\r\n"
      write mid "include main.rules\n"
      native <- discoverRules base V1524 root
      case native of
        Fallback CyclicRules{} -> assertFailure "top-level and native reads have different include targets"
        Fallback _ -> pure ()
        _ -> assertFailure "expected ordinary conversion for native line endings"
      write root mainBytes
      write mid "include missing.rules"
      discoverRules base V1996 root >>= fallback
      write mid "include main.rules"
      discoverRules base V1996 root >>= fallback
      write mid (T.encodeUtf8 ("include " <> T.pack (toFilePath leaf)))
      discoverRules base V1996 root >>= fallback
#ifndef mingw32_HOST_OS
      D.removeFile (toFilePath mid)
      D.createFileLink (toFilePath leaf) (toFilePath mid)
      tree <- discoverRules base V1996 root
      tree @?= Reusable [(root,digestBytes mainBytes),(mid,digestBytes ""),(leaf,digestBytes "")]
      D.removeFile (toFilePath mid)
      D.createFileLink (toFilePath root) (toFilePath mid)
      write root "include middle.rules"
      cycleResult <- discoverRules base V1524 root
      case cycleResult of
        Fallback CyclicRules{} -> pure ()
        _ -> assertFailure "a nested symlink resolving the same relative include repeats the cycle"
#endif
      let imports = base </> [reldir|import|]
          csv = imports </> [relfile|alice/bank/acct/1-in/2026/01_rfo-special.csv|]
          bank = imports </> [relfile|bank.rules|]
          override = imports </> [relfile|alice/bank/acct/rfo-special.rules|]
      dirs <- either (fail . T.unpack) pure (extractImportDirs imports csv)
      P.createDirIfMissing True (parent csv)
      write bank ""
      selectRules csv dirs >>= (@?= Reusable (Just bank))
      write override ""
      D.getModificationTime (toFilePath bank) >>= D.setModificationTime (toFilePath override) . addUTCTime (-1000)
      selectRules csv dirs >>= (@?= Reusable (Just override))
      D.removeFile (toFilePath override)
      selectRules csv dirs >>= (@?= Reusable (Just bank)))
  , "config syntax and whole-file digest" ~: withTestDir (\base -> do
      let file = base </> [relfile|hledger.conf|]
          ok release s = checkConfig release file s @?= Reusable ()
          no release s = fallback (checkConfig release file s)
      forM_ [V1524,V1996] $ \release -> do
        ok release "# comment\n[balance] --period=thismonth\n[register]\n@file\n[general]\n[print]\n"
        forM_ ["--period=thismonth", "[general] --debug", "[print]\n@indirect", "[txns]", "[bal]", "[b]", "[alias]", "[missing", "[general", "[balance]\n[print] --period=thismonth"] (no release)
      ok V1996 "[print] --layout=hledger1 # layout\n"
      no V1524 "[print]\n--layout=hledger1"
      no V1996 "[general]\n--layout=hledger1"
      ok V1996 "[holdings]"
      no V1524 "[print]\n [balance]\n--empty"
      no V1524 "[holdings]"
      ok V1524 "[demo]"
      no V1996 "[demo]"
      forM_ ["[balance]\n--period=thismonth", "[balance]\n--period=thisyear"] $ \bytes -> do
        B.writeFile (toFilePath file) bytes
        discoverConfig V1996 (Just (GivenConf (toFilePath file))) >>= (@?= Reusable (Just (file,digestBytes bytes)))
      fallback (checkArguments ["print","@file"])
      discoverConfig V1996 (Just (GivenConf (toFilePath base))) >>= fallback
#ifndef mingw32_HOST_OS
      D.removeFile (toFilePath file)
      D.createFileLink "hledger.conf" (toFilePath file)
      D.withCurrentDirectory (toFilePath base) (discoverConfig V1996 Nothing >>= fallback)
#endif
      )
  , "directives eligibility" ~: withTestDir (\base -> do
      let file = base </> [relfile|directives.journal|]
          check = checkDirectives V1996 file . T.encodeUtf8
      forM_ ["account assets:bank:current  ; type: C","commodity 1,000.00000000 BTC  ; lots:","commodity €1,000.00","P 2000-01-01 EUR €1","P 2000/01/01 BTC 12.30 EUR","; tags\n# prices\n* prices\n\n"] $ \s -> check s @?= Reusable ()
      forM_ ["account assets:any\n    note", "comment\ncommodity €1\nend comment","include *.journal","P today EUR €1","P 2000-01-01 EUR €1 trailing","commodity 1,234 ABC extra", "2025/01/01 A"] (fallback . check))
  , "converter identity" ~: bracket getLocaleEncoding setLocaleEncoding (\_ -> withTestDir (\base -> do
      setLocaleEncoding utf8
      let real = base </> [relfile|actual|]
          second = base </> [relfile|other|]
          native = B.pack [127,69,76,70]
      B.writeFile (toFilePath real) native
      B.writeFile (toFilePath second) native
#ifndef mingw32_HOST_OS
      let link = base </> [relfile|hledger|]
      D.createFileLink (toFilePath real) (toFilePath link)
      let executable = link
#else
      let executable = real
#endif
      probes <- newIORef (0 :: Int)
      let version _ = modifyIORef' probes (+1) >> pure (ExitSuccess,"hledger 1.99.6-gc3a608c2b-20261101, mac-aarch64")
      probe <- converterProbeWith executable version
      first <- probe
      case first of
        Reusable c -> converterRelease c @?= V1996
        Fallback why -> assertFailure (show why)
      probe >>= (@?= first)
      readIORef probes >>= (@?= 1)
      B.appendFile (toFilePath real) "replacement"
      replaced <- probe
      assertBool "same version different binary" (replaced /= first)
#ifndef mingw32_HOST_OS
      D.removeFile (toFilePath link)
      D.createFileLink (toFilePath second) (toFilePath link)
      retargeted <- probe
      assertBool "retargeted symlink" (retargeted /= first)
#endif
      forM_ ["h.cmd","h.BAT","scoop.exe","wrapped","unknown"] $ \name -> do
        name' <- parseRelFile name
        let path = base </> name'
        B.writeFile (toFilePath path) (if name == "wrapped" then "#!/bin/sh\n" else if name == "unknown" then "hledger" else native)
        if name == "scoop.exe" then B.writeFile (toFilePath (base </> [relfile|scoop.shim|])) "path = hidden" else pure ()
        converterProbeWith path version >>= (>>= fallback)
      converterProbeWith real (const (pure (ExitSuccess,"hledger 1.52.40"))) >>= (>>= fallback)
      setLocaleEncoding latin1
      converterProbeWith real version >>= (>>= fallback)))
  ]
