{-# LANGUAGE OverloadedStrings #-}

-- | Status as text for people and JSON for scripts and agents.
module Hledger.Flow.Status.Render
  ( useColor,
    renderStatus,
    renderNext,
    renderEnvironment,
    statusJSON,
    tildePath,
    nextStepHere,
    printStatus,
    printNext,
  )
where

import Control.Exception (IOException, catch)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Hledger.Flow.Common (findHledger, nativeShell)
import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Environment (coresSetting, environmentJSON, environmentSettings, renderSettings)
import Hledger.Flow.Internals (SystemInfo, systemInfo)
import Hledger.Flow.Status.Gather (gatherStatus)
import Hledger.Flow.Types (BaseDir)
import Path.IO (getCurrentDir, getHomeDir)
import qualified Data.List as List
import qualified Data.Text as T
import Hledger.Flow.PathHelpers (AbsDir)
import Hledger.Flow.Status
import Hledger.Flow.Types (HledgerInfo (..))
import Path (toFilePath)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleIntensity (..), ConsoleLayer (..), SGR (..), hSupportsANSIColor, setSGRCode)
import System.Environment (lookupEnv)
import qualified System.FilePath as FilePath
import System.IO (Handle, stdout)

-- | Colour only on a terminal, and only when NO_COLOR is unset or empty.
useColor :: Handle -> IO Bool
useColor h = do
  noColor <- lookupEnv "NO_COLOR"
  supported <- hSupportsANSIColor h
  return (supported && maybe True null noColor)

-- | An absolute path, with the home directory shown as @~@.
tildePath :: Maybe AbsDir -> FilePath -> FilePath
tildePath home path = case home of
  Just h | Just rest <- List.stripPrefix (toFilePath h) path -> "~" FilePath.</> rest
  _ -> path

paint :: Bool -> [SGR] -> T.Text -> T.Text
paint False _ txt = txt
paint True sgr txt = T.pack (setSGRCode sgr) <> txt <> T.pack (setSGRCode [Reset])

green, red, yellow, bold :: Bool -> T.Text -> T.Text
green c = paint c [SetColor Foreground Vivid Green]
red c = paint c [SetColor Foreground Vivid Red]
yellow c = paint c [SetColor Foreground Vivid Yellow]
bold c = paint c [SetConsoleIntensity BoldIntensity]

baseText :: LedgerStatus -> FilePath
baseText = FilePath.dropTrailingPathSeparator . toFilePath . lsBaseDir

renderStatus :: Bool -> Maybe AbsDir -> LedgerStatus -> NextStep -> T.Text
renderStatus color home s next =
  T.unlines (header : accountBlock ++ misplacedBlock) <> renderNext color next
  where
    header = bold color "Ledger: " <> T.pack (tildePath home (baseText s)) <> "   (" <> hledgerText <> ")"
    hledgerText = case lsHledger s of
      Just hl -> "hledger " <> hledgerVersionNumber (hlVersion hl) <> " found"
      Nothing -> red color "hledger not found"
    rows = [(asAccount a, counts a, state a) | a <- lsAccounts s]
    width f = maximum (0 : map (T.length . f) rows)
    accountBlock
      | null rows = []
      | otherwise =
          "" : ["  " <> T.justifyLeft (width fst3) ' ' name <> "   " <> T.justifyLeft (width snd3) ' ' n <> "   " <> st | (name, n, st) <- rows]
    fst3 (x, _, _) = x
    snd3 (_, x, _) = x
    counts a = plural (asStatements a) "statement" <> years (asYears a)
    years [] = ""
    years [y] = " (" <> tshow y <> ")"
    years (y : ys) = " (" <> tshow y <> "–" <> tshow (last ys) <> ")"
    state a
      | asWithoutRules a > 0 && not (asConstruct a) = red color "✗ no rules file"
      | otherwise =
          let missing = asJournalsMissing a
              outdated = asJournalsOutdated a
              imported = asStatements a - missing
              parts =
                [plural imported "journal" | imported > 0]
                  ++ [tshow missing <> " not imported" | missing > 0]
                  ++ [tshow outdated <> " outdated" | outdated > 0]
              txt = T.intercalate ", " parts
           in if missing + outdated > 0 then yellow color ("• " <> txt) else green color ("✓ " <> txt)
    misplacedBlock = case length (lsMisplaced s) of
      0 -> []
      n -> ["", "  " <> yellow color ("⚠ " <> plural n "file" <> " not where hledger-flow looks")]

renderNext :: Bool -> NextStep -> T.Text
renderNext color next =
  T.unlines $
    ""
      : (bold color "Next:" <> " " <> nsSummary next)
      : map ("      " <>) (["Run: " <> c | Just c <- [nsCommand next]] ++ nsDetail next ++ ["Docs: " <> d | Just d <- [nsDocs next]])

tshow :: Show a => a -> T.Text
tshow = T.pack . show

statusJSON :: SystemInfo -> Maybe FilePath -> LedgerStatus -> NextStep -> Value
statusJSON sys conf s next =
  object
    [ "schemaVersion" .= (1 :: Int),
      "environment" .= environmentJSON sys conf,
      "baseDir" .= baseText s,
      "hledger" .= fmap hledgerJSON (lsHledger s),
      "ledgerFile" .= fmap (\lf -> object ["value" .= lfValue lf, "exists" .= lfExists lf]) (lsLedgerFile s),
      "accounts" .= map accountJSON (lsAccounts s),
      "misplaced" .= [object ["path" .= mpPath m, "suggestion" .= mpSuggestion m] | m <- lsMisplaced s],
      "manualJournals" .= lsManualJournals s,
      "allYearsJournalExists" .= lsAllYearsExists s,
      "next"
        .= object
          [ "summary" .= nsSummary next,
            "command" .= nsCommand next,
            "detail" .= nsDetail next,
            "docs" .= nsDocs next
          ]
    ]
  where
    hledgerJSON hl = object ["path" .= toFilePath (hlPath hl), "version" .= hledgerVersionNumber (hlVersion hl)]
    accountJSON a =
      object
        [ "account" .= asAccount a,
          "statements" .= asStatements a,
          "years" .= asYears a,
          "rulesFile" .= asRulesFile a,
          "rulesCandidates" .= asRulesCandidates a,
          "statementsWithoutRules" .= asWithoutRules a,
          "hooks" .= object ["preprocess" .= asPreprocess a, "construct" .= asConstruct a],
          "journalsMissing" .= asJournalsMissing a,
          "journalsOutdated" .= asJournalsOutdated a
        ]

nextStepHere :: LedgerStatus -> IO NextStep
nextStepHere status = do
  year <- currentYear
  cwd <- getCurrentDir
  return (nextStep year nativeShell cwd status)

-- | The versions and paths status ran with, with full paths for bug reports.
renderEnvironment :: Bool -> SystemInfo -> Maybe FilePath -> LedgerStatus -> T.Text
renderEnvironment color sys conf s =
  T.unlines ("" : bold color "Environment:" : map ("  " <>) (renderSettings (red color) settings))
  where
    settings = environmentSettings sys (lsHledger s) conf (lsBaseDir s) ++ [coresSetting sys]

-- | Print the ledger's status as text, or as JSON for scripts and agents.
printStatus :: Maybe FilePath -> Maybe FilePath -> Bool -> BaseDir -> IO ()
printStatus hledgerPath conf json base = do
  hledger <- findHledger hledgerPath
  status <- gatherStatus hledger base
  next <- nextStepHere status
  sys <- systemInfo
  if json
    then TL.putStrLn (encodeToLazyText (statusJSON sys conf status next))
    else do
      color <- useColor stdout
      home <- (Just <$> getHomeDir) `catch` (\e -> const (return Nothing) (e :: IOException))
      T.putStr (renderStatus color home status next <> renderEnvironment color sys conf status)

-- | Print only the next step for a ledger.
printNext :: Maybe FilePath -> BaseDir -> IO ()
printNext hledgerPath base = do
  hledger <- findHledger hledgerPath
  status <- gatherStatus hledger base
  next <- nextStepHere status
  color <- useColor stdout
  T.putStr (renderNext color next)
