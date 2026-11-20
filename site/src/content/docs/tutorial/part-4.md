---
title: "Part 4: A difficult statement"
description: Convert a bank CSV with a preamble, Afrikaans dates and no year, using hledger rules alone.
---

This is the fourth part of a [six-part tutorial](/tutorial/). It continues
in `finances` from [part 3](/tutorial/part-3/).

## Bogart Bank

Gawie’s salary is paid into his cheque account at Bogart Bank. Bogart’s
statement export hasn’t changed since the last century.

```bash
mkdir -p import/gawie/bogart/cheque/1-in/2016
cp ../Downloads/Bogart/123456789_2016-0*.csv import/gawie/bogart/cheque/1-in/2016/
hledger-flow status
```

```text
Ledger: ~/finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/bogart/cheque     3 statements (2016)   ✗ no rules file
  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

  To review:
    1 transfer account to check  reports/all/2016/transfer-balance.txt
    5 unknown transactions  reports/gawie/2016/unknown-transactions.txt

Next: Create import/gawie/bogart/cheque/bogart-cheque.rules (rules for gawie/bogart/cheque)
      Run: hledger-flow rules gawie/bogart/cheque
      It creates that file from the statement's columns, for you to edit.
      Docs: https://hledger-flow.github.io/go/rules-files/

Environment:
  hledger-flow  0.21.0 (443ad42), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

```bash
hledger-flow rules gawie/bogart/cheque
cat import/gawie/bogart/cheque/bogart-cheque.rules
```

```text
hledger-flow: import/gawie/bogart/cheque/1-in/2016/123456789_2016-05-28.csv: Statement rows have different numbers of columns. Write the fields line yourself: https://hledger-flow.github.io/go/rules-files/
cat: import/gawie/bogart/cheque/bogart-cheque.rules: No such file or directory
```

No draft this time. Gawie looks at the statement:

```bash
head -n 9 import/gawie/bogart/cheque/1-in/2016/123456789_2016-03-30.csv
```

```text
2,123456789,'MNR GAWIE DE GROOT','BOGART TJEKREKENING'
3,,'Staat'
3,'Staatnommer','Vanaf Datum','Tot Datum'
3,75,'28 Februarie 2016','28 Maart 2016'
4,,'Opsomming'
5,,'Transaksies'
5,'Nommer','Datum','Beskrywing1','Beskrywing2','Beskrywing3','Bedrag','Saldo','Opgeloopte Koste'
5,1,'01 Mrt',"#Monthly Bank Fee",,"",-500.00,40000.00,
5,2,'01 Mrt',"Gereeld Bet Na","Rent","",-5000.00,35000.00,
```

The first column says what kind of row it is, and only the `5` rows with a
transaction number are transactions. The dates have Afrikaans month names
and no year. The description is spread over three columns.

Gawie’s IT instincts say “write a script”. But each of these problems has
an answer in hledger’s rules, and the accounts can share those rules. He tries them first.

## Rules for a difficult CSV

**Skip everything but transactions.** `skip` inside an `if` block skips the
matching rows. With `!`, the block matches rows that *don’t* match the
pattern:

```hledger
if ! ^5,[0-9]
  skip
```

**Name the columns.** The `fields` line names every column. `amount` and
`balance` are hledger field names: `balance` turns Bogart’s running balance
into a [balance assertion](https://hledger.org/hledger.html#balance-assertions)
on every transaction, so hledger checks the arithmetic.

```hledger
fields record, number, datum, desc1, desc2, desc3, amount, balance, fees
```

**Rewrite the dates.** An [if table](https://hledger.org/hledger.html#if-table)
is a compact list of `if` blocks: each row has a pattern and the value to
assign. In a value, `\1` inserts what the pattern’s first parenthesised
group matched, the day of the month here. hledger insists on a top-level
`date` line, so Gawie starts with the raw date and lets the table replace it.

```hledger
date %datum

if,date
%datum '(..) Jan',2016-01-\1
%datum '(..) Feb',2016-02-\1
%datum '(..) Mrt',2016-03-\1
```

This isn’t ideal: the year 2016 is now written into the rules, so every
statement gets it, even next year’s. It works for these three statements,
and Gawie removes the hard-coded year in [part 5](/tutorial/part-5/).

**Build a description.** hledger’s
[rules functions](https://hledger.org/dev/hledger.html#functions) transform
values as they’re used. `join` combines the three description columns,
leaving out the empty ones, and `replace` removes Bogart’s stray `#`:

```hledger
description %{replace(join(" ", %desc1, %desc2, %desc3), "^#", "")}
```

Put together, with a currency, a cleared status and the account, this is
`import/gawie/bogart/cheque/bogart-cheque.rules`:

```hledger title="import/gawie/bogart/cheque/bogart-cheque.rules"
# Bogart Bank's old system adds a preamble, so keep only the rows
# that start with 5 and a transaction number.
if ! ^5,[0-9]
  skip

fields record, number, datum, desc1, desc2, desc3, amount, balance, fees

# Dates look like '01 Mrt': Afrikaans month names, no year.
# Start with the raw date, then let the table below rewrite it.
date %datum

if,date
%datum '(..) Jan',2016-01-\1
%datum '(..) Feb',2016-02-\1
%datum '(..) Mrt',2016-03-\1
%datum '(..) Apr',2016-04-\1
%datum '(..) Mei',2016-05-\1
%datum '(..) Jun',2016-06-\1
%datum '(..) Jul',2016-07-\1
%datum '(..) Aug',2016-08-\1
%datum '(..) Sep',2016-09-\1
%datum '(..) Okt',2016-10-\1
%datum '(..) Nov',2016-11-\1
%datum '(..) Des',2016-12-\1

description %{replace(join(" ", %desc1, %desc2, %desc3), "^#", "")}
currency ZAR
status *
account1 assets:bogart:cheque

include ../../categories.rules
```

The cheque account brings new categories, which Gawie adds to
`import/gawie/categories.rules`. If you skipped the exercise in part 3, add
its answer too.

```hledger
if Monthly Bank Fee
  account2 expenses:bank-fees

if Rent
  account2 expenses:rent

if Insurance
  account2 expenses:insurance

if Charity
  account2 expenses:charity

if Cash Withdrawal
  account2 expenses:cash

if MyBTCEx|Traditional Boring Investments
  account2 assets:investments

if Credit Card Account
  account2 assets:transfers:creditcard

if Salary
  account2 income:salary
```

The payments to the credit card go to `assets:transfers:creditcard`, the
account the card’s side of each payment already uses.

```bash
hledger-flow import
head -n 7 import/gawie/bogart/cheque/3-journal/2016/123456789_2016-03-30.journal
```

```text
Imported 6 statements for gawie/bogart/cheque, gawie/kudu/creditcard.
2016-03-01 * Monthly Bank Fee
    assets:bogart:cheque                        -500.00 ZAR = 40,000.00 ZAR
    expenses:bank-fees                           500.00 ZAR

2016-03-01 * Gereeld Bet Na Rent
    assets:bogart:cheque                      -5,000.00 ZAR = 35,000.00 ZAR
    expenses:rent                              5,000.00 ZAR
```

Gawie ran a full import from `finances`, so it also rebuilt the card’s
journals with the updated shared categories.

## An opening balance

```bash
hledger incomestatement
```

```text
hledger: Error: /home/gawie/finances/import/gawie/bogart/cheque/3-journal/2016/123456789_2016-03-30.journal:2:61:
  | 2016-03-01 * Monthly Bank Fee
2 |     assets:bogart:cheque                        -500.00 ZAR = 40,000.00 ZAR
  |                                                             ^^^^^^^^^^^^^^^
  |     expenses:bank-fees                           500.00 ZAR

Balance assertion failed in assets:bogart:cheque
In commodity ZAR at this point, excluding subaccounts, ignoring costs,
the asserted balance is:        40,000.00 ZAR
but the calculated balance is:    -500.00 ZAR
(difference: 40,500.00 ZAR)
To troubleshoot, check this account's running balance with assertions disabled, eg:
hledger reg -E --ignore-assertions '2016-03-02'assets:bogart:cheque$ cur:ZAR -e
```

The balance assertion caught something. Bogart says the account held
40,000.00 ZAR after the first transaction, but hledger only knows about the
500.00 ZAR fee. The account already had 40,500.00 ZAR in it before the
first statement.

`hledger-flow` includes a file named `<year>-opening.journal` in an account
directory before that year’s statements. It’s the file `init` made as a
starter for the example account in [part 1](/tutorial/part-1/). Gawie saves the
[opening balance](/reference/balances/) as
`import/gawie/bogart/cheque/2016-opening.journal`:

```hledger title="import/gawie/bogart/cheque/2016-opening.journal"
2016-03-01 Cheque account opening balance
    assets:bogart:cheque              40500.00 ZAR
    equity:opening-closing
```

```bash
hledger-flow import
hledger incomestatement --monthly --pretty
```

```text
Imported 6 statements for gawie/bogart/cheque, gawie/kudu/creditcard.
Monthly Income Statement 2016-03-01..2016-05-31

┌───────────────────────╥───────────────┬───────────────┬───────────────┐
│                       ║           Mar │           Apr │           May │
╞═══════════════════════╬═══════════════╪═══════════════╪═══════════════╡
│ Revenues              ║               │               │               │
├───────────────────────╫───────────────┼───────────────┼───────────────┤
│ income:salary         ║ 37,256.28 ZAR │ 37,256.28 ZAR │ 37,256.28 ZAR │
├───────────────────────╫───────────────┼───────────────┼───────────────┤
│                       ║ 37,256.28 ZAR │ 37,256.28 ZAR │ 37,256.28 ZAR │
╞═══════════════════════╬═══════════════╪═══════════════╪═══════════════╡
│ Expenses              ║               │               │               │
├───────────────────────╫───────────────┼───────────────┼───────────────┤
│ expenses:bank-fees    ║  1,050.23 ZAR │    599.06 ZAR │             0 │
│ expenses:car          ║  1,785.00 ZAR │  5,455.00 ZAR │  1,815.00 ZAR │
│ expenses:cash         ║  3,000.00 ZAR │  5,000.00 ZAR │  4,000.00 ZAR │
│ expenses:charity      ║  4,000.00 ZAR │  4,500.00 ZAR │  4,000.00 ZAR │
│ expenses:fun          ║    389.00 ZAR │             0 │  2,905.00 ZAR │
│ expenses:groceries    ║  1,977.70 ZAR │  2,056.05 ZAR │  2,163.25 ZAR │
│ expenses:health       ║    214.90 ZAR │             0 │    189.50 ZAR │
│ expenses:household    ║  4,999.00 ZAR │  1,840.00 ZAR │             0 │
│ expenses:insurance    ║  5,234.43 ZAR │  5,234.43 ZAR │  5,234.43 ZAR │
│ expenses:mopane-worms ║  6,130.00 ZAR │  6,495.00 ZAR │  6,510.00 ZAR │
│ expenses:phone        ║    250.00 ZAR │    250.00 ZAR │    250.00 ZAR │
│ expenses:rent         ║  5,000.00 ZAR │  5,000.00 ZAR │  5,000.00 ZAR │
├───────────────────────╫───────────────┼───────────────┼───────────────┤
│                       ║ 34,030.26 ZAR │ 36,429.54 ZAR │ 32,067.18 ZAR │
╞═══════════════════════╬═══════════════╪═══════════════╪═══════════════╡
│ Net:                  ║  3,226.02 ZAR │    826.74 ZAR │  5,189.10 ZAR │
└───────────────────────╨───────────────┴───────────────┴───────────────┘
```

Gawie earns more than he spends every month, even in April, when the car
needed new tyres. The card payments and investments don’t appear: they
move money between Gawie’s own accounts, so they’re neither income nor
expenses.

```bash
hledger balancesheet --pretty
```

```text
Balance Sheet 2016-05-30

┌─────────────────────────────╥───────────────┐
│                             ║    2016-05-30 │
╞═════════════════════════════╬═══════════════╡
│ Assets                      ║               │
├─────────────────────────────╫───────────────┤
│ assets:bogart:cheque        ║ 37,416.26 ZAR │
│ assets:investments          ║ 15,000.00 ZAR │
├─────────────────────────────╫───────────────┤
│                             ║ 52,416.26 ZAR │
╞═════════════════════════════╬═══════════════╡
│ Liabilities                 ║               │
├─────────────────────────────╫───────────────┤
│ liabilities:kudu:creditcard ║  2,674.40 ZAR │
├─────────────────────────────╫───────────────┤
│                             ║  2,674.40 ZAR │
╞═════════════════════════════╬═══════════════╡
│ Net:                        ║ 49,741.86 ZAR │
└─────────────────────────────╨───────────────┘
```

All from rules files, without a line of script. Gawie commits the new
account:

```bash
git add -A
git commit -q -m "Add the Bogart cheque account"
```

The year written into the date table is the one thing left to fix.
That’s where [part 5](/tutorial/part-5/) begins.

## Try it yourself

Kudu Bank’s descriptions are in capitals. Use the `capitalize` function to
turn `PUNDA MARIA TRADING STORE` into `Punda Maria Trading Store`.

<details>
<summary>Answer</summary>

Add this line to `import/gawie/kudu/creditcard/kudu-creditcard.rules`, then
run `hledger-flow import`:

```hledger
description %{capitalize(%description)}
```

`%description` is the CSV column the `fields` line named `description`. The
categories still match, because `if` blocks match the CSV row, not the
final description.

</details>
