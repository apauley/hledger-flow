---
title: "Part 1: Try the example"
description: Start a ledger, follow the example account through import, report and status, then make the account your own.
---

This is the first part of a [six-part tutorial](/tutorial/).

## The Story of an African Chef

Meet Gawie de Groot. Gawie is a successful [mopane worm](https://en.wikipedia.org/wiki/Gonimbrasia_belina#As_food) chef, based in the northernmost section of the [Kruger National Park](https://en.wikipedia.org/wiki/Kruger_National_Park).

![](./img/mopane-worm-meal.jpg)

In times gone by he used to work in a big city as an IT professional.
But after a few years he decided to choose a quieter life in the bushveld.

He now works at a renowned bush restaurant aptly named the "`Grillerige Groen Goggatjie`".

Demand for his traditional African cuisine has sky-rocketed, with visitors from all over Zimbabwe, Mozambique and South Africa
coming to enjoy his dishes.

Business has been good the last few years, but Gawie wants to make sure that he'll also be OK when business is down.

It is time for Gawie to take charge of his finances.

## Start a ledger

Gawie [installed hledger-flow](/quickstart/) and runs it to see what it
says:

```bash
hledger-flow
```

```text
hledger-flow turns bank statements into hledger journals and reports.

Commands:
  status   Show what's in your ledger and what to do next
  init     Start a new ledger in a folder
  rules    Create a starter rules file for an account
  import   Turn statements into journals
  report   Generate reports from your journals

Run `hledger-flow --help` for options and examples.

No hledger-flow ledger here or in any parent folder.
Run `hledger-flow init` in an empty folder to start one.
```

He makes a folder for his finances. His IT years taught him to keep
anything he cares about in Git, so he starts a repository first. Then
every change hledger-flow makes shows up as a diff he can read before he
commits it.

```bash
mkdir finances
cd finances
git init -q
hledger-flow init
```

```text
Started a ledger in ~/finances
Created an example account (import/gawie/mybank/checking) and starter journals for manual entries and balances.
Created directives.journal
Created prices/2026/EUR-USD.prices
Created prices/2026/BTC-USD.prices
Created prices/2026/prices.journal

Next: Import 1 new or changed statement
      Run: hledger-flow import
```

```bash
git add -A
git status --short
```

```text
A  directives.journal
A  import/_manual_/2026/post-import.journal
A  import/_manual_/2026/pre-import.journal
A  import/gawie/_manual_/2026/post-import.journal
A  import/gawie/_manual_/2026/pre-import.journal
A  import/gawie/mybank/_manual_/2026/post-import.journal
A  import/gawie/mybank/_manual_/2026/pre-import.journal
A  import/gawie/mybank/checking/1-in/2026/hledger-flow-example.csv
A  import/gawie/mybank/checking/2026-closing.journal
A  import/gawie/mybank/checking/2026-opening.journal
A  import/gawie/mybank/checking/_manual_/2026/post-import.journal
A  import/gawie/mybank/checking/_manual_/2026/pre-import.journal
A  import/gawie/mybank/checking/mybank-checking.rules
A  prices/2026/BTC-USD.prices
A  prices/2026/EUR-USD.prices
A  prices/2026/prices.journal
```

`init` made a small but complete ledger:

- `import/gawie/mybank/checking` is an example account. Its folders are
  the owner, the bank and the account. `1-in/` holds a sample statement,
  filed under the current year, and `mybank-checking.rules` tells hledger
  how to read it. hledger-flow takes the owner name from your login.
- The `_manual_` folders, at every level, hold starter journals for
  transactions you type in yourself. `<year>-opening.journal` and
  `<year>-closing.journal` hold an account’s
  [opening and closing balances](/reference/balances/).
- `directives.journal` declares common currencies and account types for
  the whole ledger.
- `prices/` holds starter files for exchange rates.

The starters are only comments with an example in each, so they change
nothing until Gawie fills them in. He commits the new ledger:

```bash
git commit -q -m "Start a ledger"
```

## Import the example

`init` ended by naming the next step:

```bash
hledger-flow import
```

```text
Imported 1 statement for gawie/mybank/checking.
```

The CSV rules can read the sample statement as it is, so the import goes
straight from `1-in/` to `3-journal/`. [Part 5](/tutorial/part-5/) shows
the optional step in between, where a `preprocess` script adds missing
years to the dates and the resulting CSV goes in `2-preprocessed/`.

```bash
git add -A
git status --short
git diff --staged -- all-years.journal import/gawie/mybank/checking/3-journal
```

```text
A  all-years.journal
A  import/2026-include.journal
A  import/all-years.journal
A  import/gawie/2026-include.journal
A  import/gawie/all-years.journal
A  import/gawie/mybank/2026-include.journal
A  import/gawie/mybank/all-years.journal
A  import/gawie/mybank/checking/2026-include.journal
A  import/gawie/mybank/checking/3-journal/2026/hledger-flow-example.journal
A  import/gawie/mybank/checking/all-years.journal
diff --git a/all-years.journal b/all-years.journal
new file mode 100644
index 0000000..6d4dcbe
--- /dev/null
+++ b/all-years.journal
@@ -0,0 +1,4 @@
+### Generated by hledger-flow - DO NOT EDIT ###
+
+include directives.journal
+include import/all-years.journal
diff --git a/import/gawie/mybank/checking/3-journal/2026/hledger-flow-example.journal b/import/gawie/mybank/checking/3-journal/2026/hledger-flow-example.journal
new file mode 100644
index 0000000..9b0827d
--- /dev/null
+++ b/import/gawie/mybank/checking/3-journal/2026/hledger-flow-example.journal
@@ -0,0 +1,8 @@
+2026-01-02 Example purchase
+    assets:mybank:checking                        -5.00 USD
+    expenses:example                               5.00 USD
+
+2026-01-02 Example refund
+    assets:mybank:checking                         5.00 USD
+    expenses:example                              -5.00 USD
+
```

The sample statement became an hledger journal in `3-journal/`. The
`*-include.journal` and `all-years.journal` files tie every journal
together, so the top-level `all-years.journal` holds the whole ledger.
`hledger-flow` writes them on every import; don’t edit them.

```bash
git commit -q -m "Import the example statement"
```

## Report on the example

```bash
hledger-flow report
```

```text
Generated 7 reports in reports/. Skipped 2 empty reports.
Each report starts with the hledger command that made it. Copy that line to tailor the report.
```

```bash
git add -A
git status --short
cat reports/gawie/*/accounts.txt
```

```text
A  reports/all/2026/balance-sheet.txt
A  reports/all/2026/income-expenses.txt
A  reports/all/2026/monthly/income-expenses.txt
A  reports/gawie/2026/accounts.txt
A  reports/gawie/2026/balance-sheet.txt
A  reports/gawie/2026/income-expenses.txt
A  reports/gawie/2026/monthly/income-expenses.txt
hledger --file directives.journal --file import/gawie/all-years.journal --period 2026 accounts

assets:mybank:checking
expenses:example
```

`report` saves a set of hledger reports for each owner and year, plus
combined ones for all owners under `reports/all/`. The example’s purchase
and refund cancel out, so its balance sheet and income statement show
zeros. Gawie’s own statements will fill them in.

```bash
git commit -q -m "Report on the example"
```

## Ask status what’s next

```bash
hledger-flow status
git status --short
```

```text
Ledger: ~/finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/mybank/checking   1 statement (2026)   ✓ 1 journal

  Note: LEDGER_FILE isn't set, so hledger won't find this ledger by default.
        hledger-flow report uses all-years.journal.
        For hledger, run: export LEDGER_FILE="$HOME/finances/all-years.journal"
        Or try: hledger -f "$HOME/finances/all-years.journal" balance

Next: Adapt the example account to your ledger
      Rename the owner, bank and account folders in import/gawie/mybank/checking to match your ledger.
      Replace the sample CSV under 1-in/ with your statement in the folder for its year.
      Replace the sample rules with your own <bank>-<account>.rules in the account folder.
      Use your account names in any starter journals you fill in.
      Then run hledger-flow import and hledger-flow report.
      Docs: https://hledger-flow.github.io/go/input-files/

Environment:
  hledger-flow  0.21.0 (443ad42), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

`status` lists each account with its statements, says what to do next,
and ends with the versions and settings it found. It only reads, so `git
status` printed nothing. Gawie can run it at any point. He’ll deal with
the note about `LEDGER_FILE` in [part 2](/tutorial/part-2/).

## Make the account your own

Gawie starts with the easiest of his accounts: his Kudu Bank credit card.
Kudu’s online banking exports a tidy CSV, with a header row, one row per
transaction and dates hledger already understands:

```text
Date,Description,Amount
2016-03-02,PUNDA MARIA TRADING STORE,-842.50
2016-03-04,LETABA FUEL STOP,-910.00
```

He follows status’s advice. He keeps his owner folder, renames the bank and
account folders, and removes the sample statement and its rules:

```bash
mv import/gawie/mybank import/gawie/kudu
mv import/gawie/kudu/checking import/gawie/kudu/creditcard
rm import/gawie/kudu/creditcard/1-in/*/hledger-flow-example.csv
rm import/gawie/kudu/creditcard/mybank-checking.rules
```

`init` made the starter journals and prices for the current year, but
Gawie’s statements are from 2016. He removes the starters and the
example’s reports. He’ll add the files he needs for 2016 later in the
tutorial. If your own statements are from this year, keep the starters.

```bash
rm -r import/_manual_ import/gawie/_manual_ import/gawie/kudu/_manual_ import/gawie/kudu/creditcard/_manual_
rm import/gawie/kudu/creditcard/*-opening.journal import/gawie/kudu/creditcard/*-closing.journal
rm -r prices reports
```

Then he files his first statement in the folder for its year,
`import/<owner>/<bank>/<account>/1-in/<year>/`, and asks status again:

```bash
mkdir -p import/gawie/kudu/creditcard/1-in/2016
cp ../Downloads/Kudu/creditcard_2016-03-31.csv import/gawie/kudu/creditcard/1-in/2016/
hledger-flow status
```

```text
Ledger: ~/finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/kudu/creditcard   1 statement (2016)   ✗ no rules file

  Note: LEDGER_FILE isn't set, so hledger won't find this ledger by default.
        hledger-flow report uses all-years.journal.
        For hledger, run: export LEDGER_FILE="$HOME/finances/all-years.journal"
        Or try: hledger -f "$HOME/finances/all-years.journal" balance

Next: Create import/gawie/kudu/creditcard/kudu-creditcard.rules (rules for gawie/kudu/creditcard)
      Run: hledger-flow rules gawie/kudu/creditcard
      It creates that file from the statement's columns, for you to edit.
      Docs: https://hledger-flow.github.io/go/rules-files/

Environment:
  hledger-flow  0.21.0 (443ad42), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

```bash
git add -A
git status --short
```

```text
D  import/_manual_/2026/post-import.journal
D  import/_manual_/2026/pre-import.journal
D  import/gawie/_manual_/2026/post-import.journal
D  import/gawie/_manual_/2026/pre-import.journal
R  import/gawie/mybank/2026-include.journal -> import/gawie/kudu/2026-include.journal
R  import/gawie/mybank/all-years.journal -> import/gawie/kudu/all-years.journal
A  import/gawie/kudu/creditcard/1-in/2016/creditcard_2016-03-31.csv
R  import/gawie/mybank/checking/2026-include.journal -> import/gawie/kudu/creditcard/2026-include.journal
R  import/gawie/mybank/checking/3-journal/2026/hledger-flow-example.journal -> import/gawie/kudu/creditcard/3-journal/2026/hledger-flow-example.journal
R  import/gawie/mybank/checking/all-years.journal -> import/gawie/kudu/creditcard/all-years.journal
D  import/gawie/mybank/_manual_/2026/post-import.journal
D  import/gawie/mybank/_manual_/2026/pre-import.journal
D  import/gawie/mybank/checking/1-in/2026/hledger-flow-example.csv
D  import/gawie/mybank/checking/2026-closing.journal
D  import/gawie/mybank/checking/2026-opening.journal
D  import/gawie/mybank/checking/_manual_/2026/post-import.journal
D  import/gawie/mybank/checking/_manual_/2026/pre-import.journal
D  import/gawie/mybank/checking/mybank-checking.rules
D  prices/2026/BTC-USD.prices
D  prices/2026/EUR-USD.prices
D  prices/2026/prices.journal
D  reports/all/2026/balance-sheet.txt
D  reports/all/2026/income-expenses.txt
D  reports/all/2026/monthly/income-expenses.txt
D  reports/gawie/2026/accounts.txt
D  reports/gawie/2026/balance-sheet.txt
D  reports/gawie/2026/income-expenses.txt
D  reports/gawie/2026/monthly/income-expenses.txt
```

Git sees the renamed folders as renames (`R`) and the example’s files as
deletions (`D`). The example journal in `3-journal/` moved with its folder;
the next import deals with it.

```bash
git commit -q -m "Replace the example with my Kudu statement"
```

Status’s next step is a rules file for the new statement. That’s where
[part 2](/tutorial/part-2/) begins.
