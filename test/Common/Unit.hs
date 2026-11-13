{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Common.Unit where

import Hledger.Flow.BaseDir (relativeToBase')
import Hledger.Flow.Common
import Data.Aeson (eitherDecodeFileStrict)
import Hledger.Flow.DocHelpers (DocLink (..), docSlug, docURL)
import Hledger.Flow.RuntimeOptions (RuntimeOptions (hledgerConf))
import Hledger.Flow.Types (HledgerConf (..), HledgerInfo (hlVersion))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Path ((</>), reldir)
import Test.HUnit
import TestHelpers (defaultHlInfo, defaultOpts, testRoot)
import qualified System.FilePath as FilePath

testDocURL :: Test
testDocURL = TestCase $
  assertEqual "Input files documentation URL"
    "https://hledger-flow.github.io/go/input-files/"
    (docURL InputFiles)

testDocLinksHaveRedirects :: Test
testDocLinksHaveRedirects = TestCase $ do
  redirects <- either fail pure =<< eitherDecodeFileStrict "site/go-links.json" :: IO (Map.Map T.Text T.Text)
  let missing = [docSlug l | l <- [minBound .. maxBound], docSlug l `Map.notMember` redirects]
  assertEqual "Every DocLink needs a redirect in site/go-links.json" [] missing

testShowCmdArgs :: Test
testShowCmdArgs =
  TestCase
    ( do
        let opts = ["--number", "/tmp/file with spaces"]
        let expected = "--number '/tmp/file with spaces'"
        let actual = showCmdArgs opts
        assertEqual "Convert command-line arguments to text" expected actual
        mapM_ (\(arg, quoted) -> assertEqual ("Quote literal argument " ++ show arg) quoted (escapeArg arg))
          ([ ("", "''"),
            ("Morgan's Saturday", "'Morgan'\"'\"'s Saturday'"),
            ("O'Brien", "'O'\"'\"'Brien'"),
            ("$(echo injected);*", "'$(echo injected);*'"),
            ("a\tb\nc", "'a\tb\nc'"),
            ("a\"b", "'a\"b'"),
            ("a\\b", "'a\\b'"),
            ("--file=import/2016-include.journal", "--file=import/2016-include.journal")
          ] :: [(T.Text, T.Text)])
    )

testHledgerConfArgs :: Test
testHledgerConfArgs =
  TestCase
    ( do
        let optsWithoutConf = defaultOpts (testRoot </> [reldir|base|])
        assertEqual "No hledger config should produce no hledger config args" [] (hledgerConfArgs optsWithoutConf)

        let optsWithConf = optsWithoutConf {hledgerConf = Just (GivenConf "/tmp/hledger.conf")}
        assertEqual "Configured hledger config should produce --conf args" ["--conf", "/tmp/hledger.conf"] (hledgerConfArgs optsWithConf)

        let optsWithFoundConf = optsWithoutConf {hledgerConf = Just (FoundConf "/home/me/.hledger.conf")}
        assertEqual "hledger finds its default config itself" [] (hledgerConfArgs optsWithFoundConf)
    )

testHledgerFileArgument :: Test
testHledgerFileArgument = TestList
  [ "1.51 reads a literal filename" ~: argument "hledger 1.51.2, mac-aarch64" ~?= "April [1].csv",
    "1.52 expands file globs" ~: argument "hledger 1.52, mac-aarch64" ~?= "April [[-[]1].csv",
    "development versions expand file globs" ~: argument "hledger 1.99-g123abc" ~?= "April [[-[]1].csv"
  ]
  where
    argument version = hledgerFileArgument (defaultHlInfo {hlVersion = version}) "April [1].csv"

testHomePaths :: Test
testHomePaths =
  let homes = ["/home/me/", "/real/me"]
      native = T.pack . FilePath.joinPath
      quoted parts = "\"" <> native ("$HOME" : parts) <> "\""
      displayedProgram = if FilePath.isPathSeparator '\\' then "~\\'bin\\hledger'" else "~/bin/hledger"
   in TestList
        [ "home itself" ~: tildePath homes "/home/me" ~?= "~",
          "below home, trailing slash on HOME" ~: tildePath homes "/home/me/fin" ~?= FilePath.joinPath ["~", "fin"],
          "resolved spelling" ~: tildePath homes "/real/me/fin" ~?= FilePath.joinPath ["~", "fin"],
          "a sibling with the same prefix" ~: tildePath homes "/home/meg/fin" ~?= "/home/meg/fin",
          "outside home" ~: tildePath homes "/srv/fin" ~?= "/srv/fin",
          "command line" ~: showCmdArgsAt homes ["/home/me/bin/hledger", "--file", "/home/me/my fin/a.csv", "/srv/b"]
            ~?= displayedProgram <> " --file ~" <> T.singleton FilePath.pathSeparator <> "'" <> native ["my fin", "a.csv"] <> "' /srv/b",
          "posix command path" ~: homeShellPath Posix homes "/home/me/my fin" ~?= Just (quoted ["my fin"]),
          "home itself in a command" ~: homeShellPath Posix homes "/real/me" ~?= Just "\"$HOME\"",
          "powershell command path" ~: homeShellPath PowerShell homes "/home/me/Lee's fin" ~?= Just (quoted ["Lee's fin"]),
          "outside home in a command" ~: homeShellPath Posix homes "/srv/fin" ~?= Nothing,
          "posix specials fall back" ~: map (\c -> homeShellPath Posix homes ("/home/me/a" ++ [c])) "$`\"!" ~?= replicate 4 Nothing,
          "trailing backslash is special unless it separates paths" ~: homeShellPath Posix homes "/home/me/a\\"
            ~?= if FilePath.isPathSeparator '\\' then Just (quoted ["a"]) else Nothing,
          "powershell specials fall back" ~: map (\c -> homeShellPath PowerShell homes ("/home/me/a" ++ [c])) "$`\"\x201c\x201d\x201e" ~?= replicate 6 Nothing,
          "posix-only specials are fine in powershell" ~: homeShellPath PowerShell homes "/home/me/a!" ~?= Just (quoted ["a!"])
        ]

testRelativeToBase :: Test
testRelativeToBase =
  TestCase
    ( do
        let expected = "file1.journal"
        let relativeWithTrailingSlash = relativeToBase' "./base/dir/" "./base/dir/file1.journal"
        assertEqual "relative base dir with trailing slash" expected relativeWithTrailingSlash

        let relativeNoTrailingSlash = relativeToBase' "./base/dir" "./base/dir/file1.journal"
        assertEqual "relative base dir without a trailing slash" expected relativeNoTrailingSlash

        let absoluteWithTrailingSlash = relativeToBase' "/base/dir/" "/base/dir/file1.journal"
        assertEqual "absolute base dir with trailing slash" expected absoluteWithTrailingSlash

        let absoluteNoTrailingSlash = relativeToBase' "/base/dir" "/base/dir/file1.journal"
        assertEqual "absolute base dir without a trailing slash" expected absoluteNoTrailingSlash

        let absoluteTwiceNoTrailingSlash = relativeToBase' "/base/dir" "/base/dir"
        assertEqual "absolute base dir without a trailing slash supplied twice" "./" absoluteTwiceNoTrailingSlash

        let absoluteTwiceWithTrailingSlash = relativeToBase' "/base/dir/" "/base/dir/"
        assertEqual "absolute base dir with a trailing slash supplied twice" "./" absoluteTwiceWithTrailingSlash

        let absoluteTwiceNoTrailingSlashOnSecondParam = relativeToBase' "/base/dir/" "/base/dir"
        assertEqual "absolute base dir supplied twice, but the second param has no slash" "./" absoluteTwiceNoTrailingSlashOnSecondParam

        let mismatch = relativeToBase' "/base/dir" "/unrelated/dir/file1.journal"
        assertEqual "A basedir with no shared prefix should return the supplied file unchanged" "/unrelated/dir/file1.journal" mismatch

        assertEqual "Do not strip part of a sibling directory name" "base.other/file"
          (relativeToBase' "base" "base.other/file")
        assertEqual "Retain the trailing separator for a parent directory" "../"
          (relativeToBase' "base" "base/..")
        if FilePath.pathSeparator == '\\'
          then assertEqual "A UNC share is not relative to the current drive root" "//server/share/file"
                 (relativeToBase' "/" "//server/share/file")
          else return ()
    )

testExtractDigits :: Test
testExtractDigits =
  TestCase
    ( do
        let txt1 = "A number: 321\nAnother number is 42, so is 0"

        let expected1 = Right 321420
        let actual1 = extractDigits txt1
        assertEqual "Extract digits from text 1" expected1 actual1

        let txt2 = "No numbers in this line"

        let expected2 = Left "input does not start with a digit"
        let actual2 = extractDigits txt2
        assertEqual "Extract digits from text 2" expected2 actual2
    )

-- Grouping requires an ordering on the key alone.
data Entry = Entry Char Int deriving (Eq, Show)

testGroupValuesBy :: Test
testGroupValuesBy = TestCase $ do
  let key (Entry k _) = k
      a1 = Entry 'a' 1
      a2 = Entry 'a' 2
      b1 = Entry 'b' 1
      b2 = Entry 'b' 2
  assertEqual "interleaved keys retain input order and duplicates"
    (Map.fromList [('a', [a1, a2]), ('b', [b2, b1, b2])])
    (groupValuesBy key [b2, a1, b1, a2, b2])

tests :: Test
tests = TestList [testDocURL, testDocLinksHaveRedirects, testShowCmdArgs, testHledgerConfArgs, testHledgerFileArgument, testHomePaths, testRelativeToBase, testExtractDigits, testGroupValuesBy, testTrimToolOutput]

testTrimToolOutput :: Test
testTrimToolOutput =
  TestList
    [ "long line" ~: trimToolOutput (T.replicate 400 "x") ~?= T.replicate 300 "x" <> "…",
      "many lines" ~: T.lines (trimToolOutput (T.unlines (map (T.pack . show) [1 .. 25 :: Int])))
        ~?= map (T.pack . show) [1 .. 20 :: Int] ++ ["… (5 more lines)"],
      "short output unchanged" ~: trimToolOutput "a\nb\n" ~?= "a\nb\n"
    ]
