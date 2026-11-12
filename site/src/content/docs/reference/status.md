---
title: Status and next steps
description: See where your ledger stands, what to do next, and the JSON status for scripts and agents.
---

To see where you stand at any time, run `hledger-flow status` (or just
`hledger-flow`) anywhere in your ledger. It lists your accounts, flags
statements in the wrong place, and suggests what to do next.
`hledger-flow init` starts a new ledger.

## hledger config file

hledger 1.40 and later read default options from a config file: the one you
give with `--hledger-conf`, or else the first it finds of `hledger.conf` in
the current directory or a parent, `~/.hledger.conf`, and `hledger.conf` in
your XDG config directory. hledger-flow runs hledger from the directory you
run it in, so that file applies to import and reports too. Options in it can
change the journals import generates, such as `--layout` under `[print]`.

The `hledger.conf` line in the Environment section at the end of status names
the file, marked `(found by hledger)` when you didn't give it.

## Status for scripts and agents

`hledger-flow status --json` prints the same status as one JSON object:

- `schemaVersion`: currently `1`.
- `baseDir`: the ledger's absolute path.
- `environment`: `{"hledgerFlowVersion", "commit", "compiler", "os", "arch",
  "cores", "availableCores", "hledgerConf"}`, for bug reports. `hledgerConf`
  is the [hledger config file](#hledger-config-file) path, or `null`.
- `hledger`: `{"path", "version"}`, or `null` when status cannot locate hledger
  or read its version. Pressing Ctrl-C during the version check cancels
  `status`. In JSON mode the command exits unsuccessfully without emitting
  a JSON object.
- `ledgerFile`: `{"value", "exists"}` for `LEDGER_FILE`, or `null` when unset.
- `accounts`: one object per `owner/bank/account` with statements:
  `account`, `statements`, `years`, `rulesFile` (the account's default rules
  file, or `null`), `rulesCandidates`, `statementsWithoutRules`,
  `hooks` (`preprocess`, `construct`), `journalsMissing`, `journalsOutdated`.
  For journals generated with CSV rules, status follows each `include`
  relative to the rules file that contains it, including nested includes.
  A shared rules file newer than the journal counts toward `journalsOutdated`.
  A missing or unreadable include, or an include cycle, also marks the journal
  outdated. Fix the rules if needed, then run `hledger-flow import`.
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
macOS and Linux.

## Paths in your home folder

Status, init, import and report show paths in your home folder as `~`, such
as `~/finances`. In commands to paste they write the home folder as `$HOME`,
which the shell expands where a quoted `~` wouldn't, such as
`export LEDGER_FILE="$HOME/finances/all-years.journal"`. A path whose rest
contains a character that is special inside double quotes, such as `$`,
appears in full instead. `status --json` always gives full paths, and
hledger's own messages are shown as hledger printed them. Status recommends a full import when conversion inputs have
changed, generated files have lost their source statements, or journal
includes need updating.
