# Changelog for [hledger-flow](https://github.com/apauley/hledger-flow)

<!--
How to keep this ChangeLog

- During development, keep "## Unreleased" first and leave it empty until
  something changes for someone installing, building or running the
  hledger-flow executable. Add or update the entry in the same commit as
  the change.
- Say what users can now do, what behaves differently or what action they
  need to take. Name the affected platform, command, option, file or output
  when it helps readers recognise the change.
- Keep each entry brief. Put commands and step-by-step instructions in the
  relevant guide. Link directly to that section when readers need details.
  Use a full URL so the link also works in the GitHub release notes.
- Keep one entry per user-visible change. If several changes add ways to do
  the same thing, update the existing entry. For example, list new installation
  options together and link each option to its instructions.
- Order entries by impact on users, highest first.
  1. Changes users must act on, saying what they need to do.
  2. New features and capabilities.
  3. Behaviour changes and fixes, the most noticeable first.
  4. Build, packaging and dependency changes.
- Skip changes that affect only maintainers, such as refactoring, changes
  to the internal Haskell API, test infrastructure, CI or release automation.
  hledger-flow supports only users who run the executable, not library users. A new Windows download belongs
  here because Windows users can install it. Automating publication to an
  existing destination belongs in release-procedure.md.
-->

## Unreleased

- Statements with brackets in their filenames, such as `April [1].csv`, now
  import exactly once alongside `April 1.csv`. If your ledger was affected,
  run `hledger-flow import` to regenerate the journals.
- If a statement is newer than its cached CSV and its preprocess hook is
  missing, import stops and names both files. Restore the hook or move the
  cached CSV aside, then run `hledger-flow import`.
- The documentation has moved to the [project site](https://hledger-flow.github.io/).
  Links in hledger-flow's messages and `--help` now open the relevant section
  there. Links printed by earlier versions still lead to the right page.
  The [tutorial](https://hledger-flow.github.io/tutorial/) is rewritten for
  this release. It shows how far hledger's rules files go before you need a
  preprocess or construct script.
- `report` now writes aggregate annual and monthly income statements and
  balance sheets even with zero or one owner, so they include root-level
  manual entries. Owner reports still show only that owner's entries.
- `import --new-files-only` now picks up statement corrections made later in
  the same second as the previous output.
- `import` now finishes with the statement summary. Run `hledger-flow status`
  for a suggested next step.
- `import` is faster: about a third faster when converting every statement,
  and about four times faster with `--new-files-only` when most statements
  are up to date. Parallel imports and reports start the next file as soon as
  a worker is free, and by default run one file per core instead of 20.
  `--batch-size` still sets the number running at once.
- Cached imports recheck apparent orphans before moving them to trash. A failed
  hledger version check reports the command and exit code before conversions
  begin. An explicit `--hledger-path` also works when `PATH` is unset.
- Dots before a statement's rules override are now supported. For example,
  `2024.04_rfo-card.csv` uses `rfo-card.rules` when present.
- If writing a rules-generated journal or an include file fails, the previous
  complete copy stays in place for the next import.
- Running `import` from a folder below a statement year now limits conversion
  and orphan cleanup to that account. Sibling accounts keep their outputs.
- `status` and `--show-options` now name the
  [hledger config file](https://hledger-flow.github.io/reference/status/#hledger-config-file)
  that hledger finds by itself, such as `~/.hledger.conf`, instead of showing
  none. Options in that file apply to import and reports, and can change the
  generated journals.
- `status`, `init`, `--show-options` and import and report messages show
  paths in your home folder as `~`. The commands they suggest write the home
  folder as `"$HOME/…"`, so you can still paste them. `status --json` keeps
  [full paths](https://hledger-flow.github.io/reference/status/#paths-in-your-home-folder).
- Preparing a directory for a future owner no longer blocks reports for owners
  who already have journals.
- `status` marks journals outdated when a shared CSV rules file changes,
  including rules included by another rules file.
- Owners, banks and accounts named `import` are supported when importing
  statements and checking their status from the ledger root.
- Ctrl-C during hledger version detection now cancels `status`, including
  `status --json`.
- Failed imports and reports finish writing their diagnostics before exiting,
  including when output is piped to a slow reader.
- `import --start-year` now rejects mistyped years such as `24` before
  importing. Use four digits or `current`.
- `status --rules-template` gives every CSV column a distinct field name,
  even when the header already contains a name such as `Reference_2`.
- Local Linux x86-64 Snap builds now package the hledger-flow version named
  on the snap and verify the executable before installation.

## 0.19.0

- New `init` and `status` commands. `status` lists your accounts, flags
  misplaced statements, and tells you when to import after changing rules,
  adding manual journals or deleting statements.
  Running `hledger-flow` alone does the same inside a ledger, and says how to
  start one elsewhere. See the [quick-start](https://github.com/apauley/hledger-flow/blob/master/QUICKSTART.md#import-your-first-statement).
- `status --json` gives scripts and AI agents the same facts and next step
  ([fields](https://github.com/apauley/hledger-flow/blob/master/docs/README.md#status-for-scripts-and-agents)).
  `status --rules-template owner/bank/account` prints a starter rules file
  from the latest statement or its matching preprocessed CSV.
- `--show-options` now prints the hledger-flow version and commit, the full
  path and version of hledger, and each setting with the flag that changes it.
  `status` always ends with these versions and paths, and `status --json` has
  them under `environment`.
- Interactive `import` and `report` runs can show an [upgrade notice](https://github.com/apauley/hledger-flow/blob/master/docs/README.md#release-notices) without waiting for the network. Set `HLEDGER_FLOW_NO_UPDATE_CHECK=1` to turn it off.
- `import` and `report` print a short summary and the next step. The report
  summary counts how many empty reports were skipped. Both `status` and
  `report` accept `~/` paths in `LEDGER_FILE`. Timings and progress lines
  moved to `--verbose`. Errors no longer show Haskell
  backtraces, and a failing hledger conversion names the rules file to check
  without echoing the whole statement.
- Linux x86-64 users can [build and install a strictly confined snap](https://github.com/apauley/hledger-flow/blob/master/snap/README.md)
  that includes the hledger 1.99.6 pre-release for imports, reports, and account scripts.

## 0.18.0

- If you keep handwritten transactions in a journal under `3-journal`
  without a matching input in `1-in`, move them to `_manual_`. `import`
  now treats `1-in` as the sole source of generated files. After
  successful conversion, orphaned preprocessed CSVs and journals in the
  selected account/year scope move to `.trash` instead of being counted
  again after an input is renamed or removed. Add `.trash/` to your
  `.gitignore`. See [generated files and recovery](https://github.com/apauley/hledger-flow/blob/master/docs/README.md#generated-files-and-recovery).
  Fixes [#108](https://github.com/apauley/hledger-flow/issues/108).
- Windows imports and reports now use UTF-8, including for account names
  such as `Morgan’s Current`. If your CSV statements use a legacy encoding,
  specify it in their rules file. Run `chcp 65001` before using standalone
  `hledger` to read the generated journals on Windows.
- You can now install hledger-flow on macOS and Linux with [Homebrew](https://github.com/apauley/hledger-flow/blob/master/README.md#install-with-homebrew).
  Each binary archive now includes a [quick-start](https://github.com/apauley/hledger-flow/blob/master/QUICKSTART.md)
  covering installation, download verification and a first import.
- Windows imports now run `preprocess.ps1` and `construct.ps1` directly
  with PowerShell. Native hooks can use `preprocess.exe` and `construct.exe`.
  Existing extensionless hooks take precedence. See
  [Windows import hooks](https://github.com/apauley/hledger-flow/blob/master/docs/README.md#windows-import-hooks).
- Manual-only years and ledgers now get include files without dummy inputs,
  including `_manual_` files at the import, owner, bank, and account levels.
  Fixes [#103](https://github.com/apauley/hledger-flow/issues/103).
- Owner reports now load `directives.journal` when present, so custom account
  types and currency formatting apply to individual owners as well as combined
  reports. Fixes [#112](https://github.com/apauley/hledger-flow/issues/112).
- Imports and reports now find `hledger` through relative `PATH` entries
  such as `.` and `../bin`, including symlinked executables.
  Fixes [#95](https://github.com/apauley/hledger-flow/issues/95).
- Imports reject input filenames that produce the same generated output
  before converting or moving files.
- Parallel imports and reports now stop the current batch when any task
  fails, even if an earlier task is still waiting on an external command.
- Commands printed in reports now preserve apostrophes and shell-special
  characters in filenames when replayed in a POSIX shell or Git Bash.
- The Windows executable now shows its product name and version in its
  file properties.

## 0.17.0

- If you use a custom `preprocess` hook, write to the path supplied as its
  second argument. It now points into a temporary directory and keeps the
  final filename and extension. Failed hooks preserve the previous
  preprocessed CSV or constructed journal.
- If you use the `Hledger.Flow.*` library, update calls that use Turtle
  types to use `path` and `Text` types. The `TurtlePath` alias has been
  removed.
- Building from source now requires GHC 9.10 or 9.12. Stack builds use
  LTS 24.65 (GHC 9.10.3), and Hackage builds use tested dependency bounds.
- Windows x64 users can download `hledger-flow-windows-x64.zip` and extract
  `hledger-flow.exe`.
- macOS release binaries are signed with a Developer ID and submitted to
  Apple for notarization so Gatekeeper can verify them.
- You can now use the GitHub CLI to verify a downloaded release archive and
  check that it was built from the release tag. See “Verifying a download”
  in the README.
- Sequential imports now write the same journal as parallel imports and
  keep diagnostics visible. Previously, `--sequential import` could print
  transactions to the terminal and overwrite the journal with an empty file.
- Importing a single account or year now refreshes the root ledger from
  journals already on disk, including newly imported years. Statements
  outside the selected directory are left for a later import.
- When an owner, bank, or account is named `1-in` or `2-preprocessed`,
  generated files now go under the correct account and the root ledger
  includes its journals.
- On Windows, imports now handle native path separators, preserve the year
  in include filenames, and write relative journal paths with forward slashes.
- Include generation stays within the requested directory, and an empty
  import index can now be read by hledger.
- Reports use the full owner directory name. Owners such as `gawie` and
  `gawie.jr` each get reports from their own journal.
- If an owner is named `all` in any capitalization, aggregate reports now go
  in `reports/_all_/` so both sets of reports are available. Otherwise,
  aggregate reports go in `reports/all/`.
- Reports now discover owners when `import` is a Windows directory junction,
  as they do for a Unix symbolic link.
- Parallel imports now report an error for a zero or negative `--batch-size`
  instead of hanging. Sequential imports still ignore the batch size.

## 0.16.4

- Add a new command-line option: `--hledger-conf /path/to/hledger/conf`.
  This allows specifying a hledger config file, causing hledger-flow to pass
  `--conf /path/to/hledger/conf` to any hledger command it invokes.

## 0.16.3

- Change `--new-files-only` to use modification time comparison instead of existence check.
  Previously, this flag skipped regeneration only if output files existed.
  Now it compares source and target modification times, allowing automatic
  reprocessing when source files are updated. Note: changes to rules files
  or construct/preprocess scripts are not tracked.

## 0.16.2

 - Add a Github Actions workflow, contributed by [Udit Desai](https://github.com/apauley/hledger-flow/issues?q=author%3Adesaiuditd)
 - Update Stack resolver to LTS Haskell 24.26 (ghc-9.10.3)

## 0.16.1

- Fix documentation URL: https://github.com/apauley/hledger-flow/tree/master/docs#feature-reference

## 0.16.0

- Switched back to `hledger print` during statement import [#126](https://github.com/apauley/hledger-flow/issues/126)
- Move feature reference to https://github.com/apauley/hledger-flow/docs
- Fix preprocessing logic for CSV files [#123](https://github.com/apauley/hledger-flow/issues/123)
- Add --ascii-reports flag [#115](https://github.com/apauley/hledger-flow/pull/115)

Other changes:
 - Switched to Stackage lts-24.8 (GHC 9.10.2)

## 0.15.0

Made some changes that will result in formatting changes of generated files:

 - Removed the obsolete exclamation mark from the `include` directive
 - Switched from `hledger print` to `hledger import` during statement import.

   `hledger import` uses your preferred commodity styles from your
   [directives.journal](https://github.com/apauley/hledger-flow#hledger-directives) to generate journals.

Other changes:

 - Switched to GHC 9.0.1

## 0.14.4

Add an option to process files in batches.

If the number of input files processed by `hledger-flow` grows large, and you have resource-intensive processing scripts, your system resources can be overwhelmed.

With this change the input files will be processed in batches, by default 200 at a time. The batch size can be set from the command-line.

https://github.com/apauley/hledger-flow/issues/93

https://github.com/apauley/hledger-flow/pull/94

## 0.14.3

Ensure that generated include files only contain files ending with .journal

Fixes [#92](https://github.com/apauley/hledger-flow/issues/92)

## 0.14.2

Add an optional `--start-year` command-line option for imports:

Import only from the specified year and onwards,
ignoring previous years. Valid values include a 4-digit
year or 'current' for the current year.

An implementation for [this feature request](https://github.com/apauley/hledger-flow/issues/81)

## 0.14.1

- Make `--enable-future-rundir` the default, and deprecate the command-line option. To be removed in a future release.
- Ensure that the deepest rundir is the account directory, because the program doesn't generate include files correctly in directories below the account level.

## 0.14.0

- Add a new performance-related command-line option to import: `--new-files-only`. [PR #89](https://github.com/apauley/hledger-flow/pull/89)

  Don't regenerate transaction files if they are
  already present. This applies to hledger journal
  files as well as files produced by the preprocess and
  construct scripts.

- Generate monthly versions of the income statement in reports. A contribution by [Max Linke](https://github.com/apauley/hledger-flow/pull/88)

- Switch some usages of system-filepath over to [path](https://github.com/apauley/hledger-flow/pull/87)

  hledger-flow started as a collection of bash scripts that I translated into Haskell with the help of [Turtle](https://hackage.haskell.org/package/turtle).

  Turtle uses the now deprecated [system-filepath](https://hackage.haskell.org/package/system-filepath) to represent all paths.

  I've had many filepath-related issues in hledger-flow.
  They were related to issues such as that 2 instances of the same directory would not be treated as equal, because one could have a trailing slash and the other not.
  Another issue that popped up was knowing wether a path is a file or a directory, and if it is absolute or relative.

  All of these issues are articulated in the `path` library:
  https://github.com/commercialhaskell/path


## 0.13.2

Improve support for importing a subset of journals: start importing only from the directory given as argument,
or the current directory, and generate only the relevant include files.

This is a behavioural change and (for now) it needs to be enabled with the --enable-future-rundir switch.
This will become the default behaviour in 0.14.x, at which time the switch will be removed.

Reports:
Use the LEDGER_FILE env var (if set) when generating reports.
Default to the top-level all-years.journal if not set.

Build with Stackage Nightly 2020-03-10 (ghc-8.8.3)

## 0.13.1

- Automatically add [include lines for yearly price files](https://github.com/apauley/hledger-flow/#price-files) if they are present on disk.
- Minor report changes - do not assume too many extra options for default reports.

## 0.13.0

- Add an experimental rundir option for imports

The experimental rundir is an attempt to restrict hledger-flow into processing just a subset of files, primarily to quickly get feedback/failures while adding new accounts to an existing set of accounts.

The use case has been described in [issue 64](https://github.com/apauley/hledger-flow/issues/64).

It is experimental, because the only problem it currently solves is getting hledger-flow to fail fast.
One of the current side effects of doing so is that the generated include files are then written to only
include the subset of files that were processed.

But as soon as you do a full run again, the include files will be correctly re-generated as before.

## 0.12.4.0

- Update usage of hledger to reflect updated command-line flags of hledger version 1.15
  https://github.com/apauley/hledger-flow/issues/73
- Compile with stackage lts-14.9

## 0.12.3.1

Fixed a bug where:

Given:
- A run of `hledger-flow import`

When:
- specifying a relative import base directory
- but specifically without any relative prefixes such as `./` or `../`

Then:
- the account-level include files pointing to the real journal entries would have incorrect paths

https://github.com/apauley/hledger-flow/issues/65

## 0.12.3

Add more reports:

- Balance Sheet per owner per year, and for all owners per year
- Unknown transactions per owner per year
- A transfer balance overview per year

## 0.12.2.1

Fix resolver extraction and hledger-flow --version in release-tarball script

## 0.12.2

Slightly smarter reporting.

- Get the available report years for each individual owner. Only generate reports for those years.
- Create uniform output directories.
- Add system info to version output

## 0.12.1

Generate some reports per owner.

Report generation is still a work-in-progress.

https://github.com/apauley/hledger-flow/pull/57

## 0.12.0

- Re-organised the command-line interface:
  moved various command-line options out of subcommands, into the top-level.
- Added a [contributor's agreement](https://github.com/apauley/hledger-flow/blob/master/CONTRIBUTING.md)
  after receiving some more valued contributions from
  [jecaro](https://github.com/apauley/hledger-flow/pull/42)

## 0.11.3

- Detect the hledger-flow base directory correctly, even when in a subdirectory. Similar to how git behaves.
- Change the version subcommand into a flag - thanks to [jecaro](https://github.com/apauley/hledger-flow/pull/38) for the contribution.

## 0.11.2

- Improved display of external process output

## 0.11.1.2

- Exit with an error code when any external script fails - https://github.com/apauley/hledger-flow/issues/28
- Capture external process output when doing parallel processing, in order to better prevent mangled concurrent output.
- Allow users to specify a path to an hledger executable
- Display a user-friendly error message if hledger cannot be found - https://github.com/apauley/hledger-flow/issues/22

## 0.11.1.1

- Support input files from the year 2011 - https://github.com/apauley/hledger-flow/issues/27
  Use a more specific input-file pattern, so as not to match 2011-include.journal
- Print command-line options if requested - https://github.com/apauley/hledger-flow/issues/11
- Use the channel output functions consistently to avoid concurrency issues.

## 0.11.1

- Create statically linked executables on Linux - https://github.com/apauley/hledger-flow/releases
- Add an option to disable parallel processing
- Log the exit status of shell commands.
- Upgrade to LTS 13.16 for GHC 8.6.4.

## 0.11

- Change the name from `hledger-makeitso` to `hledger-flow`.

## 0.10

- Add a `version` subcommand.
  Create [issue #15](https://github.com/apauley/hledger-flow/issues/15)
  to change it into a `--version` flag later.
- Fix a minor issue where yearly include files were generated at the top-level
  of the directtory structure, even though the same content was available in the
  `import` directory.
  The top-level `all-years.journal` now just includes the years within the
  `import` directory.
- Upgrade to LTS 13.15 for GHC 8.6.4
- Add CircleCI and TravisCI build instructions. Switch the README.org to a
  README.md in order to better support CI status badges.


## 0.9.0.1

First hackage release. Minor changes to fix `stack sdist` warnings and errors, in preperation of
the hackage upload.

1bf817c "Merge pull request #9 from apauley/hackage-upload" Mar 31 21:51:38 2019 +0200

## 0.9

Process all statements in parallel.

This has a significant speed improvement on multi-processor machines when dealing with lots of input files.
a906bb5 "Merge pull request #8 from apauley/parallel-import" 2019-03-27 22:45:23 +0200

## 0.8

Generate an all-years.journal on each level which includes all the available years for that level. Replace the old makeitso.journal with the top-level version of this:

06f2127 "Merge pull request #5 from apauley/all-years-includes" 2019-03-22 00:09:27 +0200

## 0.7

Change the way include files are aggregated.

It used to be by owner/bank/account, now each of those levels (owner/bank/account) are aggregated per year:
eb17fed "Merge pull request #3 from apauley/annual-includes" 2019-03-12 23:09:17 +0200

## 0.6

61c71d6 "Upgrade to lts-13.9 (GHC 8.6.3)" 2019-03-01 11:31:23 +0200

## 0.5

3a7a39e "Upgrade to lts-13.6 (GHC 8.6.3)" 2019-02-16 09:54:49 +0200

## 0.4

213552d "Upgrade to lts-12.16 (GHC 8.4.4)" 2018-11-03 20:00:21 +0200

## 0.3

5e2d45f "Update from lts-12.1 to lts-12.11" 2018-10-01 23:07:21 +0200

## 0.2

First support for the construct script, when it was confusingly named an import script:

24ac4c7 "Support a fully custom import script" 2018-09-16 16:11:53 +0200

## 0.1

The first semi-useful version, replacing a previous bash script:

131f8af "Write journal" 2018-07-23 16:08:44 +0200
