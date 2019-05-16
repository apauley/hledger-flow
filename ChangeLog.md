# Changelog for [hledger-flow](https://github.com/apauley/hledger-flow)

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
