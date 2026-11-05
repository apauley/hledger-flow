# hledger-flow

## What is it?

hledger-flow organises bank statements by owner, bank, account and year.
It uses [hledger](https://hledger.org/) and its CSV rules to generate one
journal per statement, then includes those journals and your manual entries
in `all-years.journal`. You keep the original statements so you can rebuild
the imported entries when your rules change.

It grew out of scripts I wrote while using
[adept's Full-fledged Hledger](https://github.com/adept/full-fledged-hledger/wiki).

## Who should use this?

**Choose hledger-flow** if you keep non-overlapping statements and want to
apply better rules to several years of transactions. It is especially useful
when you manage several accounts or owners and want to review the changes
to each statement's journal before committing them.

**Choose plain `hledger import`** if you want to append new transactions
and then review, split or annotate them directly in your journal. Your edits
survive the next import. This also suits rolling downloads such as “the last
90 days” from the same account. hledger remembers how far it got for each
source, which works well when dates and the order of same-day transactions
are stable. hledger-flow processes each statement independently, so overlapping
files can count the same purchase twice. You can keep the original CSVs with
either workflow. See [hledger's import guide](https://hledger.org/dev/hledger.html#import).

## Overview of the Basic Workflow

1. Save each statement under its [account and year](docs/README.md#input-files).
2. Add an [hledger rules file](docs/README.md#rules-files) for the account or bank.
   For most bank CSVs, rules are all you need.
3. Run `hledger-flow import` from the base directory. Use the generated
   `all-years.journal` as your `LEDGER_FILE`.
4. Commit the statements, rules, manual journals and generated journals to
   version control so you can review the effect of later rule changes.

### Example: reclassify purchases across three years

Suppose your 2023–2025 statements are already imported and committed.
Purchases from City Rail currently appear under `expenses:misc`. You also
recorded a one-off cash fare by hand in
`import/john/_manual_/2024/post-import.journal`. Keep handwritten adjustments
in a [manual journal](docs/README.md#manually-managed-journals), since each
full import rewrites the statement journals in `3-journal/`.

Add this rule to `import/mybank.rules`, the rules file used by those statements:

```rules
if %description City Rail
  account2 expenses:transport
```

From the base directory, rebuild all three years and review the changes:

```sh
hledger-flow import
git diff -- import/
hledger -f all-years.journal print expenses:transport
```

The diff should show the rule and the matching purchases moving from
`expenses:misc` to `expenses:transport` in each affected statement journal.
The cash fare stays in the manual journal. If you booked it to
`expenses:transport`, the last command shows it alongside the rebuilt entries.

## How do I install it?

| Installation method | Platforms | Choose this if you want to… |
| --- | --- | --- |
| [Download a GitHub release](#download-a-github-release) | Windows, macOS, Linux | Run a prebuilt executable. |
| [Install with Homebrew](#install-with-homebrew) | macOS, Linux | Install and upgrade with `brew`. |
| [Build from source](#build-from-source) | Windows, macOS, Linux | Compile the project yourself with Stack. |

### Download a GitHub release

1. Download the archive for your OS and processor from the [latest GitHub release](https://github.com/apauley/hledger-flow/releases/latest).
2. Extract it and copy `hledger-flow` (or `hledger-flow.exe` on Windows) to a directory on your `PATH`.
3. [Install hledger](https://hledger.org/install.html) for imports and reports.

The [quick-start guide](QUICKSTART.md) lists the archive names and walks through installation and your first import on each platform.

#### Verifying a download

From v0.17.0 onward, GitHub releases are immutable and the archives carry build
attestations. To verify a download, substitute its release tag and archive name
in both [GitHub CLI](https://cli.github.com/) commands:

```sh
gh release verify-asset v0.17.0 hledger-flow-macos-arm64.tar.gz -R apauley/hledger-flow
gh attestation verify hledger-flow-macos-arm64.tar.gz -R apauley/hledger-flow
```

The first command checks that the archive matches the published release. The
second checks its build provenance. The macOS binaries are signed with a
Developer ID and submitted to Apple for notarization.

### Install with Homebrew

On a [supported Mac or Linux system](https://github.com/FernTrail/homebrew-tap#supported-platforms),
check that `brew --version` works in your terminal. If you’re new to Homebrew,
[install Homebrew](https://brew.sh/) and follow the installer’s instructions to add `brew` to your `PATH`.

To set up FernTrail’s package repository (its *tap*) for the first time:

```sh
brew tap ferntrail/tap
```

Optionally, to inspect the package before installing it, trust just the
hledger-flow formula and show its details (Homebrew 6 or newer):

```sh
brew trust --formula ferntrail/tap/hledger-flow
brew info ferntrail/tap/hledger-flow
```

Install hledger-flow and its `hledger` dependency:

```sh
brew install ferntrail/tap/hledger-flow
```

If you don’t want Homebrew to install `hledger`, add `--without-hledger` to the install command.
The fully qualified install command also works on its own: Homebrew adds the tap
automatically and trusts only the named formula. See [Homebrew’s tap trust guide](https://docs.brew.sh/Tap-Trust).

For later releases, run `brew update` and `brew upgrade ferntrail/tap/hledger-flow`.

### Build from source

From a checkout of this repository, run `stack install`. Add the directory
reported by `stack path --local-bin` to your `PATH` and [install hledger](https://hledger.org/install.html).
See the [build instructions](CONTRIBUTING.md#build-the-project) for prerequisites and tests.

## Getting Started

Check that both commands are available with `hledger-flow --version` and
`hledger --version`. To select a specific copy of hledger, use
`hledger-flow --hledger-path /path/to/hledger import`. Windows users can
write account hooks in PowerShell (`preprocess.ps1` and `construct.ps1`)
or use native executables. See [Windows import hooks](docs/README.md#windows-import-hooks).

Have a look at the [detailed step-by-step instructions](step-by-step/README.md) and the [feature reference](docs/README.md).

You can see the example imported financial transactions as it was
generated by the step-by-step instructions here:

<https://github.com/apauley/hledger-flow-example>

## Compatibility with hledger

`hledger-flow` should work with any recent version of `hledger`.

## Compatibility with ledger-cli

`hledger-flow` uses `hledger` to produce journal's, so this page about [hledger and Ledger](https://hledger.org/ledger.html) should be relevant for all `hledger-flow` users.

That said, here are some observations that are specifically relevant to `hledger-flow`:

When writing out the journal include files, `hledger-flow` sorts the
include statements by filename.

[Ledger](https://www.ledger-cli.org/) fails any balance assertions
when the transactions aren't included in chronological order.

An easy way around this is to name your input files so that March's
statement is listed before December's statement.

Another option is to add `--permissive` to any
[ledger](https://www.ledger-cli.org/) command.

So you should easily be able to use both `ledger` and `hledger` on these
journals if you take care to [avoid the few incompatibilities](https://hledger.org/faq.html#how-is-hledger-different-from-ledger-) which exists
(eg in your rules files or manual journals).

## Project Goals

My `hledger` files started to collect a bunch of supporting code that
weren't really specific to my financial situation.

I want to extract and share as much as possible of that supporting code.

[Adept's](https://github.com/adept/full-fledged-hledger/wiki) goals
also resonated with me:

- Tracking expenses should take as little time, effort and manual work
  as possible
- Eventual consistency should be achievable: even if I can't record
  something precisely right now, maybe I would be able to do it later,
  so I should be able to leave things half-done and pick them up later
- Ability to refactor is a must. I want to be able to go back and change
  the way I am doing things, with as little effort as possible and
  without fear of irrevocably breaking things.

# Support

hledger-flow is supported by [FernTrail B.V.](https://ferntrail.tech/)

# Contributing to hledger-flow

Have a look at the [contribution guidelines](CONTRIBUTING.md).

# FAQ

## How do you balance transfers between 2 accounts when you have statements for both accounts?

### The Problem

In your primary bank account you've happily been classifying transfers to a
secondary account as just `Expenses:OtherAccount`.

But you've recently started processing the statements from the second account as
well so that you can classify those expenses more accurately.

And now the balances of these two accounts are all wrong when the statements of
each account deals with money transferred between these two accounts.

In `bank1.journal`, imported from `bank1.csv`:

    2018/11/09 Transfer from primary account to secondary account
        Assets:Bank1:Primary    $-200
        Assets:Bank2:Secondary

In `bank2.journal`, imported from `bank2.csv`:

    2018/11/09 Transfer from primary account to secondary account
        Assets:Bank2:Secondary  $200
        Assets:Bank1:Primary

### The Solution

As soon as you start importing statements for both accounts you will have to
introduce an intermediate account for classification between these two accounts.

I use `Assets:Transfers:*`.

And we may have reports looking at these transfers accounts at some point, you
should consider using the same names.

The above example then becomes as follows.

In `bank1.journal`, imported from `bank1.csv`:

    2019-05-18 Transfer from primary account to secondary account
        Assets:Bank1:Primary         $-200
        Assets:Transfers:Bank1Bank2

In `bank2.journal`, imported from `bank2.csv`:

    2019-05-18 Transfer from primary account to secondary account
        Assets:Bank2:Secondary       $200
        Assets:Transfers:Bank1Bank2

Any posting to `Assets:Transfers:*` indicates an in "in-flight" amount.
You would expect the balance of `Assets:Transfers` to be zero most of the time.
Whenever it isn't zero it means that you either don't yet have the other side of
the transfer, or that something is wrong in your rules.

You could theoretically just use `Assets:Transfers` without any subaccounts, but
I found it useful to use subaccounts. Because then the subaccounts can show me
where I should look for any missing transfer transaction.

I typically use sorted names as the subaccount (Python code sample):

``` python
"Assets:Transfers:" + "".join(sorted(["Bank2", "Bank1"]))
```

### External references

This approach is based on what is described in Full-fledged hledger:
<https://github.com/adept/full-fledged-hledger/wiki/Adding-more-accounts#lets-make-sure-that-transfers-are-not-double-counted>

The question was first asked in [issue \#51](https://github.com/apauley/hledger-flow/issues/51).
