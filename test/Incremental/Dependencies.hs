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
      forM_ [V1524,V1996,OtherSupported] $ \release -> do
        tree <- discoverRules base release root
        tree @?= Reusable [(root,digestBytes mainBytes),(mid,digestBytes midBytes),(leaf,digestBytes ""),(leaf,digestBytes "")]
      ruleIncludes V1524 root "include   a  \r\n include b\ninclude\tc\n" @?= Reusable ["a  "]
      ruleIncludes V1996 root "include   a  \r\n include b\ninclude\tc\n" @?= Reusable ["a"]
      fallback (nestedRuleIncludes V1524 mid "include final.rules\r\n")
      nestedRuleIncludes V1996 mid "include final.rules\r\n" @?= Reusable ["final.rules"]
      forM_ ["include filename.rules  ", " include file.rules", "include\tfile.rules", "Include file.rules"] $
        fallback . ruleIncludes OtherSupported root
      fallback (nestedRuleIncludes OtherSupported mid "include final.rules\r\n")
      fallback (nestedRuleIncludes OtherSupported mid (B.pack [0xef,0xbb,0xbf] <> "include final.rules"))
      ruleIncludes OtherSupported root "include  shopping categories.rules\r\n" @?= Reusable ["shopping categories.rules"]
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
      forM_ [V1524,V1996,OtherSupported] $ \release -> do
        ok release "# comment\n[balance] --period=thismonth\n[register]\n@file\n[general]\n[print]\n"
        ok release "--invert\n[print] -x --round soft --layout=55 # literal values\n"
        forM_ ["not:archive", "--layout=hledger1 not:archive", "--unknown", "--round",
               "--alias /old/=new", "--begin=2025-01-01", "--period=2025", "date:2025..2026",
               "--forecast=2025", "--forecast=2025..2026", "--value=then", "--value=2025-06-30,EUR",
               "--today=2026-06-30\ndate:thismonth", "--today 2026-06-30 -V", "-B", "desc:\"in 30 days\"",
               "desc:today", "acct:next", "--period=\"monthly in 2025\"", "desc:\"a # b\"", "--round=soft # date:today"] $
          ok release . ("[print]\n" <>)
        forM_ ["[general] --debug", "[txns]", "[bal]", "[b]", "[alias]", "[missing", "[general",
               "[future-command] --period=thismonth", "[balance] date:today"] (ok release)
        forM_ ["--period=thismonth", "--begin\ntoday", "-p 'this month'", "-pthismonth", "--begin today", "-etomorrow",
               "date:today", "not:date:thisyear", "date2:'last month'", "--period=1/1..4/1",
               "--forecast", "--forecast=thismonth", "--forecast=2025..", "--value=now", "--exchange=EUR,now", "-XEUR,now",
               "-V", "-X USD", "--value=end", "--value=end,EUR", "date:'-7 days..'", "-b '2 weeks ago'",
               "expr:'tag:recurring and date:thismonth'", "expr:'not:(date:thisyear)'", "--today=2026-01-01\n--forecast"] $ \setting ->
          case checkConfig release file ("[print]\n" <> setting) of
            Fallback reason -> reasonScope reason @?= "non-deterministic hledger config entry"
            _ -> assertFailure ("expected clock dependency: " ++ show setting)
        forM_ ["--file=extra.journal", "-fextra.journal", "--rules-file extra.rules", "@indirect",
               "--new", "--output-file saved.txt", "-osaved.txt", "--locations"] $
          no release . ("[print]\n" <>)
      ok V1996 "[print]\n [balance]\n--period=thismonth"
      no V1524 "[print]\n [balance]\n--period=thismonth"
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
          check = journalIncludes file . T.encodeUtf8
      forM_ ["account assets:bank:current  ; type: C","commodity 1,000.00000000 BTC  ; lots:","commodity €1,000.00","P 2000-01-01 EUR €1","P 2000/01/01 BTC 12.30 EUR","; tags\n# prices\n* prices\n\n"] $ \s -> check s @?= Reusable []
      check "include accounts.journal  ; declarations\ninclude\ttax/vat codes.journal" @?= Reusable ["accounts.journal","tax/vat codes.journal"]
      check "include vat-codes\ninclude vat.CUSTOM\ninclude journal:vat.CSV" @?= Reusable ["vat-codes","vat.CUSTOM","vat.CSV"]
      forM_ ["", "journal:", "vat.CsV", "vat.tsv", "vat.ssv", "vat.rules", "vat.timedot", "vat.timeclock"] $
        fallback . check . ("include " <>)
      forM_ ["account assets:any\n    note", "comment\ncommodity €1\nend comment","include *.journal","P today EUR €1","P 2000-01-01 EUR €1 trailing","commodity 1,234 ABC extra", "2025/01/01 A"] (fallback . check))
  , "unmirrorable journal includes" ~: withTestDir (\tmp -> do
      let base = tmp </> [reldir|ledger|]
          root = base </> [relfile|directives.journal|]
          nested = base </> [relfile|sub/accounts.journal|]
          write path = B.writeFile (toFilePath path)
      P.createDirIfMissing True (parent nested)
      write root "include sub/accounts.journal\n"
      write nested "include ../directives.journal\n"
      discoverDirectives root >>= fallback
      write nested "include ../../outside.journal\n"
      write (tmp </> [relfile|outside.journal|]) "account expenses:cafe\n"
      discoverDirectives root >>= fallback
#ifndef mingw32_HOST_OS
      let link = base </> [relfile|alias.journal|]
      D.createFileLink (toFilePath nested) (toFilePath link)
      write root "include alias.journal\n"
      write nested "include prices.journal\n"
      write (base </> [relfile|prices.journal|]) "; different directory\n"
      write (parent nested </> [relfile|prices.journal|]) "; canonical directory\n"
      discoverDirectives root >>= fallback
#endif
      )
  , "converter identity" ~: bracket getLocaleEncoding setLocaleEncoding (\_ -> withTestDir (\base -> do
      setLocaleEncoding utf8
      let real = base </> [relfile|actual|]
          second = base </> [relfile|other|]
          native = B.pack [127,69,76,70]
      B.writeFile (toFilePath real) (native <> "build A")
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
      let replacement = toFilePath (base </> [relfile|replacement|])
      previousTime <- D.getModificationTime (toFilePath real)
      B.writeFile replacement (native <> "build B")
      D.setModificationTime replacement previousTime
      D.renameFile replacement (toFilePath real)
      -- Compare the key persisted between imports, which excludes file stamps.
      -- Both executables have the same path, version, length and mtime.
      let checkReplacement result = case (first, result) of
            (Reusable old, Reusable new) ->
              assertBool "replacement changes saved converter key" (converterKey old /= converterKey new)
            _ -> assertFailure ("expected two eligible builds: " ++ show (first, result))
      probe >>= checkReplacement
      freshProbe <- converterProbeWith executable version
      freshProbe >>= checkReplacement
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
      let identify v = converterProbeWith real (const (pure (ExitSuccess,"hledger " <> v))) >>= id
      forM_ [("1.52.4",V1524),("1.99.6",V1996),("1.52.40",OtherSupported),
             ("1.99-g01a4b4866-20261113, mac-aarch64",OtherSupported),("1.100",OtherSupported),("2.0",OtherSupported)] $ \(v, expected) ->
        (fmap converterRelease <$> identify v) >>= (@?= Reusable expected)
      forM_ ["1.52.3","1.52","1.9.40","1.99.x","1.99.","1..99","unknown"] $ \v -> identify v >>= fallback
      setLocaleEncoding latin1
      converterProbeWith real version >>= (>>= fallback)))
  ]
