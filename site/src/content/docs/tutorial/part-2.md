---
title: "Part 2: Categories and reports"
description: Import more statements, classify transactions with if rules, and generate reports.
---

This is the second part of a [five-part tutorial](/tutorial/). It continues
in `my-finances` from [part 1](/tutorial/part-1/), with `LEDGER_FILE` set.

## More months

Adding statements to an account that already has a rules file takes one
copy and one import:

```bash
cp ../Downloads/Kudu/creditcard_2016-0[45]*.csv import/gawie/kudu/creditcard/1-in/2016/
hledger-flow import
```

```text
Imported 3 statements for gawie/kudu/creditcard.
```

```bash
hledger balance
```

```text
            R2674.40  expenses:unknown
           R-2674.40  liabilities:kudu:creditcard
--------------------
                   0  
```

Three months of spending, and all of it is still unknown. Even the monthly
payments to the card count as negative expenses. Gawie wants to know where
the money goes.

## Categories

hledger rules can set fields for matching rows only. An `if` block matches
a pattern against the whole CSV row, ignoring case, and its indented lines
apply only to the rows that match.

Gawie’s other accounts will need the same categories, so he keeps them in a
file of their own, `import/gawie/categories.rules`:

```hledger title="import/gawie/categories.rules"
# Gawie's categories, shared by all his accounts.
# Each if block matches text anywhere in a CSV row, ignoring case.

if PUNDA MARIA TRADING
  account2 expenses:groceries

if FUEL STOP|TYRES
  account2 expenses:car

if MOPANE MASTERS
  account2 expenses:mopane-worms

if APPLIANCES|HARDWARE
  account2 expenses:household

if BUSHTEL
  account2 expenses:phone

if PAYMENT RECEIVED
  account2 assets:transfers:creditcard
```

A payment to the card isn’t an expense. It’s money moving from Gawie’s
cheque account to the card, so it goes to a transfer account. When the
cheque account arrives in [part 3](/tutorial/part-3/), its side of each
payment goes to the same account, and the two cancel out.

The card’s rules file includes the categories at the end. `include` paths
are relative to the file that includes them:

```hledger title="import/gawie/kudu/creditcard/kudu-creditcard.rules"
skip 1
fields date, description, amount

currency R
account1 liabilities:kudu:creditcard
account2 expenses:unknown

include ../../categories.rules
```

`account2 expenses:unknown` stays as the fallback for rows no `if` block
matches.

```bash
hledger-flow import
hledger balance
```

```text
Imported 3 statements for gawie/kudu/creditcard.
          R-43000.00  assets:transfers:creditcard
            R9055.00  expenses:car
            R6197.00  expenses:groceries
            R6839.00  expenses:household
           R19135.00  expenses:mopane-worms
             R750.00  expenses:phone
            R3698.40  expenses:unknown
           R-2674.40  liabilities:kudu:creditcard
--------------------
                   0  
```

R19135 on mopane worms in three months. Gawie is a dedicated chef, even at
home.

## Reports

`hledger-flow report` runs a set of hledger reports for every owner and
year, plus combined ones for all owners under `reports/all/`, and saves them
as text files:

```bash
hledger-flow report
tree reports
```

```text
Generated 9 reports in reports/.
Each report starts with the hledger command that made it. Copy that line to tailor the report.
reports
|-- all
|   `-- 2016
|       |-- balance-sheet.txt
|       |-- income-expenses.txt
|       |-- monthly
|       |   `-- income-expenses.txt
|       `-- transfer-balance.txt
`-- gawie
    `-- 2016
        |-- accounts.txt
        |-- balance-sheet.txt
        |-- income-expenses.txt
        |-- monthly
        |   `-- income-expenses.txt
        `-- unknown-transactions.txt

7 directories, 9 files
```

`unknown-transactions.txt` is Gawie’s to-do list:

```bash
cat reports/gawie/2016/unknown-transactions.txt
```

```hledger
hledger --file import/gawie/all-years.journal --period 2016 print unknown

2016-03-12 BAOBAB BOOKS ONLINE
    liabilities:kudu:creditcard                R-389.00
    expenses:unknown                            R389.00

2016-03-26 PAFURI PHARMACY
    liabilities:kudu:creditcard                R-214.90
    expenses:unknown                            R214.90

2016-05-13 BAOBAB BOOKS ONLINE
    liabilities:kudu:creditcard                R-455.00
    expenses:unknown                            R455.00

2016-05-21 TSHIPISE HOT SPRINGS
    liabilities:kudu:creditcard               R-2450.00
    expenses:unknown                           R2450.00

2016-05-27 PAFURI PHARMACY
    liabilities:kudu:creditcard                R-189.50
    expenses:unknown                            R189.50
```

Each month, the routine is the same: copy in the new statements, run
`hledger-flow import`, add `if` blocks for whatever turned up in
`unknown-transactions.txt`, and run `hledger-flow report`.

## Try it yourself

Classify the three transactions that are left: the pharmacy as
`expenses:health`, and the books and the weekend at the hot springs as
`expenses:fun`. Then run `hledger-flow import` and `hledger-flow report`.

<details>
<summary>Answer</summary>

Add these blocks to `import/gawie/categories.rules`. One `if` can match
several patterns, separated by `|`.

```hledger
if PHARMACY
  account2 expenses:health

if BOOKS|HOT SPRINGS
  account2 expenses:fun
```

`hledger-flow report` then says `Skipped 1 empty report.` and removes
`unknown-transactions.txt`: nothing is left to classify.

</details>

The story continues with [part 3](/tutorial/part-3/), where Gawie meets
Bogart Bank’s export.
