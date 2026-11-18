---
title: Generate sample ledgers
description: Generate reproducible statements, rules and hooks to try features, share bug reports and benchmark imports.
---

Download [generate-sample-ledger.py](https://raw.githubusercontent.com/apauley/hledger-flow/master/bin/generate-sample-ledger.py)
and run it with Python 3.7 or later. The script uses only the standard library.
Run it without arguments for a summary of its usage.

Each command writes to a new directory, named by its first argument after the
mode. The examples write to `~/tmp/hlflow/<example>`, a directory that must not
exist yet. The script abbreviates home directory paths with `~` in its output
and filesystem errors. Run the examples from the folder containing the download.
On Windows you can use `py -3` in place of `python3`.

## Generate a sample with every feature

```sh
python3 generate-sample-ledger.py mixed ~/tmp/hlflow/sample --all-features
hledger-flow import ~/tmp/hlflow/sample
cd ~/tmp/hlflow/sample
hledger --no-conf -f all-years.journal balancesheet -X USD
hledger --no-conf -f all-years.journal incomestatement -X USD --yearly
```

This creates the smallest ledger that uses every feature of the generator:
one account of each import type, statements for 2024 and 2025, accounts in
USD, EUR and BTC, a credit card, salary, transfers, opening balances, price
files and `directives.journal`. It has a few rows per statement, so it
imports in seconds.

The script prints the full command that `--all-features` stands for, followed by
a summary.

```
Full command: python3 generate-sample-ledger.py mixed ~/tmp/hlflow/sample --plain 1 --advanced 1 --preprocess 1 --construct 1 --combined 1 --years 2 --statements 1 --transactions 4 --base-currency USD --other-currencies EUR,BTC --card-every 4 --opening-balances --income --transfers --prices --directives --seed 42
~/tmp/hlflow/sample: 5 accounts, 10 statements, 56 transactions
```

Copy the full command to change one setting. Options you add after `--all-features`
also override it, for example `--all-features --seed 7`.

## Share a sample

Every run prints its full command. The same command, seed and version of the
script produce the same statements, rules and journals on every platform, so
the command is all someone else needs to recreate your ledger. To report a bug, find a command
whose output shows the problem and include it in the report. Change `--seed`
to get different dates, amounts and prices with the same layout.

The hook scripts in the import types `preprocess`, `construct` and `combined`
are the one exception: generate them on the platform that will import the
data. See [Platforms](#platforms).

## Compare five import paths

```sh
python3 generate-sample-ledger.py separate ~/tmp/hlflow/comparison --accounts 10 --statements 12 --transactions 100
hledger-flow import ~/tmp/hlflow/comparison/plain
```

This creates five complete base directories under `~/tmp/hlflow/comparison/`.
Each has 10 accounts, 120 statements and 12,000 transactions. The relative
statement paths and the CSV bytes match across all five. Each raw row has a
transaction reference, a signed amount, a merchant description and a bank
category.

| Directory | How each account imports a row |
| --- | --- |
| `plain` | Rules use the bank category. |
| `advanced` | Shared rules match the description and assign an expense account. |
| `preprocess` | A hook classifies the merchant and writes a normalized CSV for the rules. |
| `construct` | A hook classifies the merchant and writes both journal postings. |
| `combined` | The preprocess hook classifies the merchant. The construct hook reads its CSV and writes the postings. |

Run import on any child directory. For example, to check and report on the
plain base after the import above, run the report with `LEDGER_FILE` unset.
This selects the generated journal even if your shell normally points at a
personal ledger:

```sh
cd ~/tmp/hlflow/comparison/plain
hledger --no-conf -f all-years.journal check
env -u LEDGER_FILE hledger-flow report
```

In native PowerShell, open a benchmark shell and run
`Remove-Item Env:LEDGER_FILE -ErrorAction SilentlyContinue` once. Then use
`hledger-flow report` or `hledger-flow report ~/tmp/hlflow/household` in the examples.

## Generate a mix in one directory

```sh
python3 generate-sample-ledger.py mixed ~/tmp/hlflow/household --plain 4 --advanced 2 --preprocess 2 --construct 1 --combined 1 --statements 12 --transactions 100
hledger-flow import ~/tmp/hlflow/household
env -u LEDGER_FILE hledger-flow report ~/tmp/hlflow/household
```

This also creates 10 accounts, 120 statements and 12,000 transactions, all
under `~/tmp/hlflow/household/import/`. With no type counts, `mixed` creates
one account of each type. Once you specify a count, omitted types get zero
accounts. For example, `mixed ~/tmp/hlflow/construct-only --construct 2`
creates just two construct accounts. In `separate` mode, the default is one
account per base. Both modes default to one year, two statements per account
per year, ten transactions per statement and seed 42.

## Add features

These options work in both modes. In `separate` mode, they apply to each base
on its own.

| Option | What it adds |
| --- | --- |
| `--years N` | Statements for N years, starting in 2024. `--statements` counts statements per year. |
| `--base-currency USD` | The currency of the first account and of prices. The default is USD. |
| `--other-currencies EUR,BTC` | Accounts cycle through the base currency, then these. Here `account-0001` is in USD, `account-0002` in EUR, `account-0003` in BTC, `account-0004` in USD again, and so on. Amounts are scaled to each currency, so a BTC account holds fractions of a bitcoin. |
| `--card-every N` | Every Nth account becomes a credit card, such as `card-0004`. Its rows post to `liabilities:examplebank:card-0004`, so purchases increase what you owe. |
| `--income` | A salary deposit from Acme Payroll in each statement of each asset account, posted to `income:salary`. |
| `--transfers` | Each account gets money from the first earlier asset account in the same currency, once in each statement. Both statements record the transfer through an account such as `assets:transfers:account-0001-card-0004`, so it balances to zero once both are imported. See [Transfers between your accounts](/guides/transfers/). A transfer to a card pays it off. |
| `--opening-balances` | A `2024-opening.journal` for each asset account. It holds at least everything the account will pay out, plus a random extra of up to half that, so balances stay positive and differ between accounts. Cards start at zero. See [Opening balances](/reference/balances/#opening-balances). |
| `--prices` | Daily prices of each other currency in the base currency, such as `prices/2024/USD-EUR.journal`. Each year's `prices/<year>/prices.journal` includes them, so reports can convert with `-X USD`. See [Price files](/reference/price-files/). |
| `--directives` | `directives.journal` with display formats for USD, EUR, GBP, CAD, BTC and any other currency you use. Each also declares its symbol, such as `$`, as an alias. See [hledger directives](/reference/directives/). |
| `--seed N` | Different dates, amounts and prices with the same layout. The default is 42. |

The generated statements use ISO codes such as `USD`. The aliases only apply
to amounts written with a symbol, such as `$5`, and need hledger 1.99.5 or
later. See [Commodity names](/reference/price-files/#commodity-names).

## Platforms

Generate on the platform that will import the data. On Linux and macOS,
generated hooks use `python3`. On native Windows they use PowerShell and
invoke `py -3` or `python` on PATH. Python is needed during import for the
three hook import types. You can move or copy an entire base on the same
platform. See [Windows import hooks](/reference/windows-hooks/) for script
execution settings.

## Compare import times

Keep the generator revision and the full command with the benchmark results.
To compare `master` with a feature branch, generate once and give each binary
a fresh copy of the base. Preserve executable permissions when copying hooks.
Use the same machine, hledger executable, hledger configuration, Python interpreter and import
concurrency (for example, `--batch-size 4`). Time only the import. Check the
journal and run reports afterwards.

For the timestamp-only check, finish a full import and time
`hledger-flow import --new-files-only`. To benchmark dependency tracking,
time the first `hledger-flow import --incremental` separately. It
creates the records. Run the same command again to measure reuse, then
add one statement and measure again. Check the regenerated statement
counts as well as the elapsed time.

The repository's [incremental benchmark driver](https://github.com/apauley/hledger-flow/blob/master/bin/bench-incremental.py)
uses this generator to compare both the archive and 1,000-statement layouts.
See [Incremental import](/reference/incremental-import/) for the reuse policies.

To isolate a comparison from personal hledger settings, create an empty
`~/tmp/hlflow/benchmark.conf` next to the generated base directories. Pass the
same file to both builds, for example
`hledger-flow --hledger-conf ~/tmp/hlflow/benchmark.conf import ~/tmp/hlflow/comparison/plain`.
Use the same option for reports.

These shapes each produce 120,000 transactions per base:

| Accounts | Statements per account | Rows per statement | Comparison |
| ---: | ---: | ---: | --- |
| 10 | 12 | 1,000 | Baseline |
| 10 | 120 | 100 | Same transactions split among more files |
| 120 | 1 | 1,000 | Same file count spread across more accounts |

To measure transaction volume, hold accounts and statements fixed and vary
`--transactions`. The generator assigns references before splitting rows into
files, so you can also compare individual postings by reference across imports.
