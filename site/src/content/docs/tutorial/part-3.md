---
title: "Part 3: Categories and reports"
description: Import more statements, classify transactions with if rules, and generate reports.
---

This is the third part of a [six-part tutorial](/tutorial/). It continues
in `my-finances` from [part 2](/tutorial/part-2/), with `LEDGER_FILE` set.

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
           R-2674.40  liabilities:kudu:creditcard
            R2674.40  expenses:unknown
--------------------
                   0  
```

Three months of spending, and all of it is still unknown. Even the monthly
payments to the card count as negative expenses. Gawie commits the new
months before he changes any rules, so the next diff shows only what the
rules change:

```bash
git add -A
git commit -q -m "Import April and May"
```

Gawie wants to know where the money goes.

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
cheque account arrives in [part 4](/tutorial/part-4/), its side of each
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
           R-2674.40  liabilities:kudu:creditcard
            R9055.00  expenses:car
            R6197.00  expenses:groceries
            R6839.00  expenses:household
           R19135.00  expenses:mopane-worms
             R750.00  expenses:phone
            R3698.40  expenses:unknown
--------------------
                   0  
```

R19135 on mopane worms in three months. Gawie is a dedicated chef, even at
home.

Git shows what the new rules did. Besides the two rules files, the import
rewrote all three journals:

```bash
git add -A
git diff --staged --stat
git diff --staged -- import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
```

```text
 import/gawie/categories.rules                      | 20 ++++++++++++++++++++
 .../3-journal/2016/creditcard_2016-03-31.journal   | 18 +++++++++---------
 .../3-journal/2016/creditcard_2016-04-30.journal   | 22 +++++++++++-----------
 .../3-journal/2016/creditcard_2016-05-31.journal   | 16 ++++++++--------
 import/gawie/kudu/creditcard/kudu-creditcard.rules |  2 ++
 5 files changed, 50 insertions(+), 28 deletions(-)
diff --git a/import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal b/import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
index 0ce930d..555224d 100644
--- a/import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
+++ b/import/gawie/kudu/creditcard/3-journal/2016/creditcard_2016-03-31.journal
@@ -1,18 +1,18 @@
 2016-03-02 PUNDA MARIA TRADING STORE
     liabilities:kudu:creditcard                R-842.50
-    expenses:unknown                            R842.50
+    expenses:groceries                          R842.50
 
 2016-03-04 LETABA FUEL STOP
     liabilities:kudu:creditcard                R-910.00
-    expenses:unknown                            R910.00
+    expenses:car                                R910.00
 
 2016-03-07 MOPANE MASTERS WHOLESALE
     liabilities:kudu:creditcard               R-3150.00
-    expenses:unknown                           R3150.00
+    expenses:mopane-worms                      R3150.00
 
 2016-03-10 PAYMENT RECEIVED - THANK YOU
     liabilities:kudu:creditcard               R14000.00
-    expenses:unknown                         R-14000.00
+    assets:transfers:creditcard              R-14000.00
 
 2016-03-12 BAOBAB BOOKS ONLINE
     liabilities:kudu:creditcard                R-389.00
@@ -20,19 +20,19 @@
 
 2016-03-15 BUSHTEL AIRTIME
     liabilities:kudu:creditcard                R-250.00
-    expenses:unknown                            R250.00
+    expenses:phone                              R250.00
 
 2016-03-18 PUNDA MARIA TRADING STORE
     liabilities:kudu:creditcard               R-1135.20
-    expenses:unknown                           R1135.20
+    expenses:groceries                         R1135.20
 
 2016-03-21 LETABA FUEL STOP
     liabilities:kudu:creditcard                R-875.00
-    expenses:unknown                            R875.00
+    expenses:car                                R875.00
 
 2016-03-23 THOHOYANDOU APPLIANCES
     liabilities:kudu:creditcard               R-4999.00
-    expenses:unknown                           R4999.00
+    expenses:household                         R4999.00
 
 2016-03-26 PAFURI PHARMACY
     liabilities:kudu:creditcard                R-214.90
@@ -40,5 +40,5 @@
 
 2016-03-29 MOPANE MASTERS WHOLESALE
     liabilities:kudu:creditcard               R-2980.00
-    expenses:unknown                           R2980.00
+    expenses:mopane-worms                      R2980.00
 
```

Every transaction a rule matched moved from `expenses:unknown` to its
category. The two that no rule matched, the books and the pharmacy, stayed
where they were. When a rules change goes wrong, this diff shows exactly
which transactions moved.

```bash
git commit -q -m "Sort spending into categories"
```

## Reports

Gawie ran `hledger-flow report` on the example in part 1. Now it has real
transactions to report on:

```bash
hledger-flow report
tree reports
```

```text
Reporting on ~/my-finances/all-years.journal (from LEDGER_FILE).
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

After a report, status also lists what to
[review](/reference/status/#review-unknown-transactions):

```bash
hledger-flow status
```

```text
Ledger: ~/my-finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

  To review:
    1 transfer account to check  reports/all/2016/transfer-balance.txt
    5 unknown transactions  reports/gawie/2016/unknown-transactions.txt

Next: Categorise 5 unknown transactions
      Review reports/gawie/2016/unknown-transactions.txt.
      Add if rules to the relevant account's rules file, then run: hledger-flow import
      Or start with transfers: reports/all/2016/transfer-balance.txt
      Docs: https://hledger-flow.github.io/go/review-unknown-transactions/

Environment:
  hledger-flow  0.20.1 (575e7b3), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

The transfer account holds the card payments, which wait for their other
side until the cheque account arrives in [part 4](/tutorial/part-4/).
`unknown-transactions.txt` is Gawie’s to-do list:

```bash
cat reports/gawie/2016/unknown-transactions.txt
```

```hledger
hledger --file directives.journal --file import/gawie/all-years.journal --period 2016 print unknown

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

Each month, Gawie files the new statements, runs `hledger-flow import` and
then `hledger-flow report`. He can leave unfamiliar purchases in
`expenses:unknown` until he recognises them. After adding a category rule,
he runs import again to rebuild the entries, then report to refresh his list.
He commits the reports too, so the next diff shows how they changed:

```bash
git add -A
git commit -q -m "Generate reports"
```

## Try it yourself

Classify the five purchases from the three remaining merchants: the pharmacy as
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

The story continues with [part 4](/tutorial/part-4/), where Gawie meets
Bogart Bank’s export.
