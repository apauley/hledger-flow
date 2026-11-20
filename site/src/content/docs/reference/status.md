---
title: Status and next steps
description: See where your ledger stands, what to do next, and the JSON status for scripts and agents.
---

To see where you stand at any time, run `hledger-flow status` (or just
`hledger-flow`) anywhere in your ledger. It lists your accounts, flags
statements in the wrong place, and suggests what to do next.
Status describes statement and rules file locations relative to the ledger root.
Run status from that root for shorter move and rename commands with relative
paths. From another directory, the commands use paths that work from there.
Once the import is up to date, status suggests adding
[market prices](/reference/price-files/) if the imported journal uses more
than one commodity and has no market price directives. It points you to
one [pair file](/reference/price-files/) per currency pair, included from
the year’s `prices.journal`.
Interactive status also shows a [cached upgrade notice](/reference/release-notices/) when a previous import or report found a newer release.
Outside a ledger, the welcome screen highlights commands on colour terminals and shows
the preview command under **Next**. Set `NO_COLOR=1` for plain text.

`hledger-flow init` starts a ledger with a working example statement and
rules, plus starter journals for [manual entries](/reference/manual-journals/),
[opening and closing balances](/reference/balances/), directives and prices.
To preview the files in a new ledger, run `hledger-flow init --dry-run` in the
current folder or pass a destination, such as `hledger-flow init --dry-run ~/finances`.
The preview lists each starter file it would create or keep. Run the same command
without `--dry-run` to create the ledger and see which files were created or kept.
The generated `directives.journal` defines common currencies, BTC and satoshis,
and standard account types. It links to the hledger manual for each section.
Commodity aliases take effect in hledger versions that support them.
Run the suggested import and report to try it. The example starts in
`import/<owner>/mybank/checking/`. Init uses your username for `<owner>` and
falls back to `me` when it cannot use that name. Status guides you to
rename the example folders and replace the sample CSV and rules. When a real
account needs rules, run the suggested `hledger-flow rules` command to create
a starter.

## Journal used for reports

`report` reads the ledger's `all-years.journal` by default. An existing
`LEDGER_FILE` can select a journal that includes this ledger's generated
`import/all-years.journal` and its `directives.journal` when present. This also
works with a renamed root journal. Owner reports still use their own indexes.

If the configured journal is unrelated or cannot be read, reports use the
local root and explain why. A path that names a missing file needs to be fixed
or unset before reporting. An unset or unrelated `LEDGER_FILE` appears as a
note in status so you can keep following `Next:`. Use the optional setup or
explicit balance command in the note to explore with hledger itself.
For a custom journal, hledger-flow caches the include list in
`.hledger-flow/cache/ledger-coverage.json` so later runs can skip asking
hledger to parse the journal again. If an included file changes during the
coverage check, the next command checks coverage again. The same cache
directory holds the last report run's input and review-file list in
`reports.json`. These local files contain absolute journal paths and
modification times. The generated `.gitignore` ignores the directory's
contents. Deleting the files forces a new coverage check and clears the
review summary until the next report.

## hledger config file

hledger 1.40 and later read default options from a config file: the one you
give with `--hledger-conf`, or else the first it finds of `hledger.conf` in
the current directory or a parent, `~/.hledger.conf`, and `hledger.conf` in
your XDG config directory. hledger-flow runs hledger from the directory you
run it in, so that file applies to import and reports too. Import runs
`hledger print`, so options under `[print]` apply to journal generation.

Import and report select their journal inputs themselves. An active
`-f` or `--file` setting, or an argument file such as `@args.txt`, stops
the affected command before it writes anything. The message names the
config file, section and line. Remove the setting or use `--hledger-conf`
to select a config for this workflow. Status carries an explicit config
forward in its suggested commands.

The `hledger.conf` line in the Environment section at the end of status names
the file, marked `(found by hledger)` when you didn't give it.

Import checks the hledger version before writing journals when a statement path
contains glob characters such as `[`, which need version-specific escaping.
`--show-options` always reads the version. You can give an absolute executable
path with `--hledger-path` even when `PATH` is unset.

## Review unknown transactions

After a complete report run, status lists current reports that need review.
Open your owner's `unknown-transactions.txt` file to see the unclassified
transactions and add matching rules to the relevant account's rules file.
Run import and report again to see what remains.

## Review transfers

Open `transfer-balance.txt` under the aggregate report folder. An account can
appear in the quarterly table even when it nets to zero over the whole year.
Check whether both sides of each transfer have arrived. See the
[transfer guide](/guides/transfers/) for offsetting entries.

Status uses only the review files from the latest successful run. Archived
reports for retired years stay on disk. When status detects changed inputs
or an incomplete run, it asks you to run report again. For unsupported review
formats such as CSV with data, status shows the path with an unavailable count.

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
- `ledgerFile`: `{"value", "exists", "coversLedger"}` for `LEDGER_FILE` or
  `null` when unset.
- `aggregateJournal`: the selected report journal's absolute path, or `null`.
- `ledgerFileNote`: optional guidance about `LEDGER_FILE`, or `null`.
- `accounts`: one object per `owner/bank/account` with statements:
  `account`, `statements`, `years`, `rulesFile` (the account's default rules
  file, or `null`), `rulesCandidates`, `statementsWithoutRules`, `otherRules`, `rulesTargetOccupied`,
  `hooks` (`preprocess`, `construct`), `journalsMissing`, `journalsOutdated`.
  For journals generated with CSV rules, status follows each `include`
  relative to the rules file that contains it, including nested includes.
  A shared rules file newer than the journal counts toward `journalsOutdated`.
  A missing or unreadable include, or an include cycle, also marks the journal
  outdated. Fix the rules if needed, then run `hledger-flow import`.
- `misplaced`: `{"path", "suggestion", "year", "years", "destinationExists", "command"}`
  for statements outside an input folder. `year` is set for a known single
  year, `years` lists years when the statement spans several, and `command`
  is set when there is a complete, unoccupied destination.
- `manualJournals`: the number of `_manual_` journals.
- `allYearsJournalExists`: whether import has created `all-years.journal`.
- `reportsCurrent`: whether the latest completed report run has current inputs.
- `review`: `null` when stale, otherwise `unknownTransactions` and
  `transferAccounts` objects, each containing `count` and `files`. A
  category's count is `null` if any current file has an unsupported layout.
- `next`: `{"summary", "command", "detail", "docs"}`. `command` is `null`
  when the step isn't a single command, such as moving files.

Paths under `import/` are relative to `baseDir`. Fields may be added within
`schemaVersion` 1; renaming or removing a field increments it. Status exits 0
whenever it could inspect the ledger, even when it found problems.

You can run the suggested command from the directory where you requested
status. Commands use POSIX shell syntax on Linux and macOS and PowerShell syntax
on Windows.

## Paths in your home folder

Status, init, import and report show paths in your home folder as `~`. For
example, a `LEDGER_FILE` notice might show `~/finances/all-years.journal`.
In commands to paste they write the home folder as `$HOME`,
which the shell expands where a quoted `~` wouldn't, such as
`export LEDGER_FILE="$HOME/finances/all-years.journal"`. A path whose rest
contains a character that is special inside double quotes, such as `$`,
appears in full instead. `status --json` always gives full paths, and
hledger's own messages are shown as hledger printed them. Status recommends a full import when conversion inputs have
changed, generated files have lost their source statements, or journal
includes need updating.
