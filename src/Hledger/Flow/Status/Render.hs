{-# LANGUAGE OverloadedStrings #-}

-- | Status as text for people and JSON for scripts and agents.
module Hledger.Flow.Status.Render
  ( useColor,
    yellow,
    renderStatus,
    renderNext,
    renderEnvironment,
    statusJSON,
    nextStepHere,
    printStatus,
    printNext,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Hledger.Flow.Common (findHledger, findHledgerConf, homeDirs, nativeShell, tildePath)
import Hledger.Flow.DateTime (currentYear)
import Hledger.Flow.Environment (coresSetting, environmentJSON, environmentSettings, renderSettings)
import Hledger.Flow.Internals (SystemInfo, systemInfo)
import Hledger.Flow.Status.Gather (gatherStatus, gatherStatusWithConfig)
import Hledger.Flow.Types (BaseDir)
import Path.IO (getCurrentDir)
import qualified Data.Text as T
import Hledger.Flow.Status
import Hledger.Flow.ReportReview
import Hledger.Flow.StatementScan (StatementYear (..))
import Hledger.Flow.LedgerSelection (Coverage (..))
import Hledger.Flow.Types (HledgerConf, HledgerInfo (..))
import Path (toFilePath)
import Hledger.Flow.Logging (bold, green, red, useColor, yellow)
import qualified System.FilePath as FilePath
import System.IO (stdout)


baseText :: LedgerStatus -> FilePath
baseText = FilePath.dropTrailingPathSeparator . toFilePath . lsBaseDir

renderStatus :: Bool -> [FilePath] -> LedgerStatus -> NextStep -> T.Text
renderStatus color homes s next =
  T.unlines (header : accountBlock ++ misplacedBlock ++ noteBlock ++ reviewBlock) <> renderNext color next
  where
    header = bold color "Ledger: " <> T.pack (tildePath homes (baseText s)) <> "   (" <> hledgerText <> ")"
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
    noteBlock = case T.lines <$> ledgerFileNote nativeShell homes s of
      Just (first : rest) -> "" : ("  Note: " <> first) : map ("        " <>) rest
      _ -> []
    reviewBlock = case maybe [] reviewItems (lsReview s) of
      [] -> []
      items -> "" : "  To review:" : map showReview (take 5 items)
        ++ ["    and " <> tshow (length items - 5) <> " more" | length items > 5]
    showReview item =
      let label = case itemKind item of
            UnknownTransactions -> "unknown transaction"
            TransferBalances -> "transfer account to check"
          count = case itemKind item of
            TransferBalances -> fmap (\n -> plural n "transfer account" <> " to check") (itemCount item)
            UnknownTransactions -> fmap (\n -> plural n label) (itemCount item)
       in "    " <> maybe "count unavailable" id count <> "  " <> T.pack (itemPath item)
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

statusJSON :: SystemInfo -> Maybe HledgerConf -> LedgerStatus -> NextStep -> Value
statusJSON sys conf s next =
  object
    [ "schemaVersion" .= (1 :: Int),
      "environment" .= environmentJSON sys conf,
      "baseDir" .= baseText s,
      "hledger" .= fmap hledgerJSON (lsHledger s),
      "ledgerFile" .= fmap (\lf -> object ["value" .= lfValue lf, "exists" .= lfExists lf, "coversLedger" .= (lfCoverage lf == CoversLedger)]) (lsLedgerFile s),
      "aggregateJournal" .= fmap toFilePath (lsAggregateJournal s),
      "ledgerFileNote" .= ledgerFileNote nativeShell [] s,
      "accounts" .= map accountJSON (lsAccounts s),
      "misplaced" .= map misplacedJSON (lsMisplaced s),
      "reportsCurrent" .= maybe False (const True) (lsReview s),
      "review" .= fmap reviewJSON (lsReview s),
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
    reviewJSON review = object ["unknownTransactions" .= category (reviewUnknowns review),
                                "transferAccounts" .= category (reviewTransfers review)]
    category review = object ["count" .= reviewCount review, "files" .= reviewFiles review]
    misplacedJSON item = object
      [ "path" .= mpPath item,
        "suggestion" .= mpSuggestion item,
        "year" .= (case mpYear item of SingleYear year -> Just year; _ -> Nothing),
        "years" .= (case mpYear item of SpanningYears years -> years; _ -> []),
        "destinationExists" .= mpDestinationExists item,
        "command" .= misplacedMoveCommand nativeShell [] (lsBaseDir s) item
      ]
    hledgerJSON hl = object ["path" .= toFilePath (hlPath hl), "version" .= hledgerVersionNumber (hlVersion hl)]
    accountJSON a =
      object
        [ "account" .= asAccount a,
          "statements" .= asStatements a,
          "years" .= asYears a,
          "rulesFile" .= asRulesFile a,
          "rulesCandidates" .= asRulesCandidates a,
          "statementsWithoutRules" .= asWithoutRules a,
          "otherRules" .= asOtherRules a,
          "rulesTargetOccupied" .= asRulesTargetOccupied a,
          "hooks" .= object ["preprocess" .= asPreprocess a, "construct" .= asConstruct a],
          "journalsMissing" .= asJournalsMissing a,
          "journalsOutdated" .= asJournalsOutdated a
        ]

-- | The next step for people to read with @homes@, or for scripts with @[]@ (absolute paths).
nextStepHere :: [FilePath] -> LedgerStatus -> IO NextStep
nextStepHere homes status = do
  year <- currentYear
  cwd <- getCurrentDir
  return (nextStep year nativeShell homes cwd status)

-- | The versions and paths status ran with, for bug reports. Paths under the home folder show as @~@.
renderEnvironment :: Bool -> [FilePath] -> SystemInfo -> Maybe HledgerConf -> LedgerStatus -> T.Text
renderEnvironment color homes sys conf s =
  T.unlines ("" : bold color "Environment:" : map ("  " <>) (renderSettings (red color) settings))
  where
    settings = environmentSettings homes sys (lsHledger s) conf ++ [coresSetting sys]

-- | Print the ledger's status as text, or as JSON for scripts and agents.
printStatus :: Maybe FilePath -> Maybe FilePath -> Bool -> BaseDir -> IO ()
printStatus hledgerPath confOpt json base = do
  hledger <- findHledger hledgerPath
  conf <- findHledgerConf confOpt
  status <- gatherStatusWithConfig hledger conf base
  sys <- systemInfo
  if json
    then do
      next <- nextStepHere [] status
      TL.putStrLn (encodeToLazyText (statusJSON sys conf status next))
    else do
      color <- useColor stdout
      homes <- homeDirs
      next <- nextStepHere homes status
      T.putStr (renderStatus color homes status next <> renderEnvironment color homes sys conf status)

-- | Print only the next step for a ledger.
printNext :: Maybe FilePath -> BaseDir -> IO ()
printNext hledgerPath base = do
  hledger <- findHledger hledgerPath
  status <- gatherStatus hledger base
  homes <- homeDirs
  next <- nextStepHere homes status
  color <- useColor stdout
  T.putStr (renderNext color next)
