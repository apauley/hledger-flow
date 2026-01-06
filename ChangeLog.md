# Changelog for [hledger-flow](https://github.com/apauley/hledger-flow)

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
- Added a [contributor's agreement](https://github.com/apauley/hledger-flow/blob/master/CONTRIBUTING.org)
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
