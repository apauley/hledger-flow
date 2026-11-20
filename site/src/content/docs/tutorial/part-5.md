---
title: "Part 5: When rules aren't enough"
description: Add a preprocess script that does the one thing rules can't, and keep everything else in rules.
---

This is the fifth part of a [six-part tutorial](/tutorial/). It continues
in `finances` from [part 4](/tutorial/part-4/).

## A savings account

In October 2016 Gawie opens a savings account at Bogart Bank. Its
statements are quarterly, and the first one runs from 28 October 2016 to
27 January 2017. It’s filed under 2017, the year the statement ends:

```bash
mkdir -p import/gawie/bogart/savings/1-in/2017
cp ../Downloads/Bogart/123456790_2017-01-27.csv import/gawie/bogart/savings/1-in/2017/
```

It has the same format as the cheque account, so the two accounts should
share their rules. Gawie moves everything except `account1` and the
categories from `bogart-cheque.rules` into a new file,
`import/gawie/bogart/bogart-statements.rules`. Each account’s rules file is
now three lines.

`import/gawie/bogart/cheque/bogart-cheque.rules`:

```rules title="import/gawie/bogart/cheque/bogart-cheque.rules"
include ../bogart-statements.rules
account1 assets:bogart:cheque
include ../../categories.rules
```

`import/gawie/bogart/savings/bogart-savings.rules`:

```rules title="import/gawie/bogart/savings/bogart-savings.rules"
include ../bogart-statements.rules
account1 assets:bogart:savings
include ../../categories.rules
```

And three more categories for `import/gawie/categories.rules`. They come
after `if Rent`, so `Rente` (interest) gets the last word:

```rules
if Oordrag Van
  account2 assets:transfers:savings

if Rente
  account2 income:interest

if Kontant Onttrekking
  account2 expenses:cash
```

```bash
hledger-flow import
hledger incomestatement
```

```text
Imported 7 statements for gawie/bogart/cheque, gawie/bogart/savings, gawie/kudu/creditcard.
hledger: Error: /home/gawie/finances/import/gawie/bogart/savings/3-journal/2017/123456790_2017-01-27.journal:2:61:
  | 2016-01-02 * Oordrag Van Tjekrekening
2 |     assets:bogart:savings                      5,000.00 ZAR = 13,579.50 ZAR
  |                                                             ^^^^^^^^^^^^^^^
  |     assets:transfers:savings                  -5,000.00 ZAR

Balance assertion failed in assets:bogart:savings
In commodity ZAR at this point, excluding subaccounts, ignoring costs,
the asserted balance is:        13,579.50 ZAR
but the calculated balance is:   5,000.00 ZAR
(difference: 8,579.50 ZAR)
To troubleshoot, check this account's running balance with assertions disabled, eg:
hledger reg -E --ignore-assertions '2016-01-03'assets:bogart:savings$ cur:ZAR -e
```

2016-01-02? That deposit was made on 2 January **2017**. The date table
gave every month the year 2016, so January jumped back a whole year, ahead
of the deposits it depends on. The balance assertion noticed.

## What rules can’t do

hledger reads a CSV one row at a time. A rule can use anything in the row
it’s converting, but nothing from the rows around it. The year is in the
statement period near the top, `'28 Oktober 2016','27 Januarie 2017'`, and
not in any transaction row. No rule can reach it.

This is what a [preprocess script](/reference/preprocess/) is for. If an
account directory has an executable called `preprocess`, `hledger-flow`
runs it on each statement and gives hledger the script’s output instead.
The script gets the statement’s path and the path to write to, plus the
bank, account and owner names.

The script should do only what rules can’t, and leave everything else to
the rules. Gawie’s script adds the year to each date and changes nothing
else. He saves it as `import/gawie/bogart/add-year`:

```sh title="import/gawie/bogart/add-year"
#!/bin/sh
# Bogart Bank leaves the year out of its dates: '01 Mrt'.
# Add the year the statement period starts in, and count
# one more each time the rows go from December to January.
awk -F, -v OFS=, '
  $1 == 3 && $3 ~ /[0-9][0-9][0-9][0-9].$/ {
    year = substr($3, length($3) - 4, 4)
    previous = substr($3, 5, 3)
  }
  $1 == 5 && $2 ~ /^[0-9]+$/ {
    month = substr($3, 5, 3)
    if (month == "Jan" && previous == "Des") year++
    previous = month
    sub(/.$/, " " year "&", $3)
  }
  { print }
' "$1" > "$2"
```

Both accounts need it, so each gets a `preprocess` link to the one script:

```bash
chmod +x import/gawie/bogart/add-year
ln -s ../add-year import/gawie/bogart/cheque/preprocess
ln -s ../add-year import/gawie/bogart/savings/preprocess
```

The dates now carry their year, `'01 Mrt 2016'`, so the date table in
`bogart-statements.rules` takes it from a second group instead of writing
2016. This is the whole shared file:

```rules title="import/gawie/bogart/bogart-statements.rules"
# Bogart Bank's old system adds a preamble, so keep only the rows
# that start with 5 and a transaction number.
if ! ^5,[0-9]
  skip

fields record, number, datum, desc1, desc2, desc3, amount, balance, fees

# Dates look like '01 Mrt 2016', with Afrikaans month names.
# The preprocess script added the year.
# Start with the raw date, then let the table below rewrite it.
date %datum

if,date
%datum '(..) Jan (....)',\2-01-\1
%datum '(..) Feb (....)',\2-02-\1
%datum '(..) Mrt (....)',\2-03-\1
%datum '(..) Apr (....)',\2-04-\1
%datum '(..) Mei (....)',\2-05-\1
%datum '(..) Jun (....)',\2-06-\1
%datum '(..) Jul (....)',\2-07-\1
%datum '(..) Aug (....)',\2-08-\1
%datum '(..) Sep (....)',\2-09-\1
%datum '(..) Okt (....)',\2-10-\1
%datum '(..) Nov (....)',\2-11-\1
%datum '(..) Des (....)',\2-12-\1

description %{replace(join(" ", %desc1, %desc2, %desc3), "^#", "")}
currency ZAR
status *
```

```bash
hledger-flow import
```

```text
Imported 7 statements for gawie/bogart/cheque, gawie/bogart/savings, gawie/kudu/creditcard.
```

```bash
head -n 9 import/gawie/bogart/savings/2-preprocessed/2017/123456790_2017-01-27.csv
```

```csv
2,123456790,'MNR GAWIE DE GROOT','BOGART SPAARREKENING'
3,,'Staat'
3,'Staatnommer','Vanaf Datum','Tot Datum'
3,1,'28 Oktober 2016','27 Januarie 2017'
4,,'Opsomming'
5,,'Transaksies'
5,'Nommer','Datum','Beskrywing1','Beskrywing2','Beskrywing3','Bedrag','Saldo','Opgeloopte Koste'
5,1,'01 Nov 2016',"Oordrag Van","Tjekrekening","",5000.00,5000.00,
5,2,'30 Nov 2016',"Rente","","",27.40,5027.40,
```

The preprocessed statements are in `2-preprocessed/`, next to `1-in/` and
`3-journal/`. When the rules misbehave, they show exactly what hledger read.

```bash
hledger register bogart:savings
```

```text
2016-11-01 Oordrag Van Tjek..  as:bogart:savings     5,000.00 ZAR   5,000.00 ZAR
2016-11-30 Rente               as:bogart:savings        27.40 ZAR   5,027.40 ZAR
2016-12-01 Oordrag Van Tjek..  as:bogart:savings     5,000.00 ZAR  10,027.40 ZAR
2016-12-23 Kontant Onttrekk..  as:bogart:savings    -1,500.00 ZAR   8,527.40 ZAR
2016-12-31 Rente               as:bogart:savings        52.10 ZAR   8,579.50 ZAR
2017-01-02 Oordrag Van Tjek..  as:bogart:savings     5,000.00 ZAR  13,579.50 ZAR
2017-01-27 Rente               as:bogart:savings        71.85 ZAR  13,651.35 ZAR
```

January is back in 2017, and every balance assertion holds. A savings
account that starts empty needs no opening balance.

## Transfers waiting for their other half

```bash
hledger-flow report
cat reports/all/2016/transfer-balance.txt
```

```text
Reporting on ~/finances/all-years.journal (from LEDGER_FILE).
Generated 16 reports in reports/. Skipped 2 empty reports.
Each report starts with the hledger command that made it. Copy that line to tailor the report.
hledger --file all-years.journal --period 2016 balance --pretty-tables --quarterly --flat --no-total transfer

Balance changes in 2016:

┌──────────────────────────╥────────┬────────┬────────┬────────────────┐
│                          ║ 2016Q1 │ 2016Q2 │ 2016Q3 │         2016Q4 │
╞══════════════════════════╬════════╪════════╪════════╪════════════════╡
│ assets:transfers:savings ║      0 │      0 │      0 │ -10,000.00 ZAR │
└──────────────────────────╨────────┴────────┴────────┴────────────────┘
```

The report shows the change in each transfer account during each quarter.
The card payments cancel within each quarter here. The -10,000.00 ZAR in the savings
row is the other side of deposits into savings. Gawie has yet to import the
cheque statements showing the withdrawals. When he does, he can check that
the corresponding postings reach `assets:transfers:savings`. If the two
sides fall in different quarters, both quarters can show a change even after
the transfer balances.

```bash
git add -A
git commit -q -m "Add the Bogart savings account"
```

The story ends with [part 6](/tutorial/part-6/), and a file that isn’t a
statement at all.
