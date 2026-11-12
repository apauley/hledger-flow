---
title: Status and next steps
description: See where your ledger stands, what to do next, and the JSON status for scripts and agents.
---

To see where you stand at any time, run `hledger-flow status` (or just
`hledger-flow`) anywhere in your ledger. It lists your accounts, flags
statements in the wrong place, and suggests what to do next.
`hledger-flow init` starts a new ledger.

## Status for scripts and agents

`hledger-flow status --json` prints the same status as one JSON object:

- `schemaVersion`: currently `1`.
- `baseDir`: the ledger's absolute path.
- `environment`: `{"hledgerFlowVersion", "commit", "compiler", "os", "arch",
  "cores", "availableCores", "hledgerConf"}`, for bug reports.
- `hledger`: `{"path", "version"}`, or `null` when hledger isn't found.
- `ledgerFile`: `{"value", "exists"}` for `LEDGER_FILE`, or `null` when unset.
- `accounts`: one object per `owner/bank/account` with statements:
  `account`, `statements`, `years`, `rulesFile` (the account's default rules
  file, or `null`), `rulesCandidates`, `statementsWithoutRules`,
  `hooks` (`preprocess`, `construct`), `journalsMissing`, `journalsOutdated`.
- `misplaced`: `{"path", "suggestion"}` for statement files import will never read.
- `manualJournals`: the number of `_manual_` journals.
- `allYearsJournalExists`: whether import has created `all-years.journal`.
- `next`: `{"summary", "command", "detail", "docs"}`. `command` is `null`
  when the step isn't a single command, such as moving files.

Paths under `import/` are relative to `baseDir`. Fields may be added within
`schemaVersion` 1; renaming or removing a field increments it. Status exits 0
whenever it could inspect the ledger, even when it found problems.

You can run the suggested command from the directory where you requested
status. Commands use PowerShell syntax on Windows and POSIX shell syntax on
macOS and Linux. Status recommends a full import when conversion inputs have
changed, generated files have lost their source statements, or journal
includes need updating.
