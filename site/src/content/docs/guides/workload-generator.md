---
title: Generate workloads for benchmarks
description: Generate reproducible statements to compare import paths and hledger-flow builds.
---

Download [generate-workload.py](https://raw.githubusercontent.com/apauley/hledger-flow/master/bin/generate-workload.py)
and run it with Python 3.7 or later. The script uses the standard library and
writes to a new directory. Run the examples from the folder containing the
download. On Windows you can use `py -3` in place of `python3`.

## Compare five import paths

```sh
python3 generate-workload.py separate comparison --accounts 10 --statements 12 --transactions 100 --seed 42
hledger-flow import comparison/plain
```

This creates five complete base directories under `comparison/`. Each has
10 accounts, 120 statements and 12,000 transactions. The relative statement
paths and the CSV bytes match across all five. Each raw row has a transaction
reference, a signed USD amount, a merchant description and a bank category.

| Directory | How each account imports a row |
| --- | --- |
| `plain` | Rules use the bank category. |
| `advanced` | Shared rules match the description and assign an expense account. |
| `preprocess` | A hook classifies the merchant and writes a normalized CSV for the rules. |
| `construct` | A hook classifies the merchant and writes both journal postings. |
| `combined` | The preprocess hook classifies the merchant. The construct hook reads its CSV and writes the postings. |

Run import on any child directory. For example, to check and report on the
plain workload after the import above, run the report with `LEDGER_FILE` unset.
This selects the generated journal even if your shell normally points at a
personal ledger:

```sh
cd comparison/plain
hledger --no-conf -f all-years.journal check
env -u LEDGER_FILE hledger-flow report
```

In native PowerShell, open a benchmark shell and run
`Remove-Item Env:LEDGER_FILE -ErrorAction SilentlyContinue` once. Then use
`hledger-flow report` or `hledger-flow report household` in the examples.

## Generate a mix in one directory

```sh
python3 generate-workload.py mixed household --plain 4 --advanced 2 --preprocess 2 --construct 1 --combined 1 --statements 12 --transactions 100 --seed 42
hledger-flow import household
env -u LEDGER_FILE hledger-flow report household
```

This also creates 10 accounts, 120 statements and 12,000 transactions, all
under `household/import/`. With no type counts, `mixed` creates one account
of each type. Once you specify a count, omitted types get zero accounts.
For example, `mixed construct-only --construct 2` creates just two construct
accounts. In `separate` mode, the default is one account per base. Both modes
default to two statements per account, ten transactions per statement and seed 1.

Generate on the platform that will import the data. On Linux and macOS,
generated hooks use `python3`. On native Windows they use PowerShell and
invoke `py -3` or `python` on PATH. Python is needed during import for the
three hook workloads. You can move or copy an entire base on the same
platform. See [Windows import hooks](/reference/windows-hooks/) for script
execution settings.

## Compare import times

Keep the generator revision and seed with the benchmark results. To compare
`master` with a feature branch, generate once and give each binary a fresh
copy of the base. Preserve executable permissions when copying hooks.
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
`benchmark.conf` next to the generated base directories. Pass the same file
to both builds, for example `hledger-flow --hledger-conf benchmark.conf import comparison/plain`.
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
