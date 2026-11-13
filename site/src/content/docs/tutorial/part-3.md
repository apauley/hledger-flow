---
title: "Part 3: A difficult statement"
description: Convert a bank CSV with a preamble, Afrikaans dates and no year, using hledger rules alone.
---

This is the third part of a [five-part tutorial](/tutorial/). It continues
in `my-finances` from [part 2](/tutorial/part-2/).

## Bogart Bank

Gawie’s salary is paid into his cheque account at Bogart Bank. Bogart’s
statement export hasn’t changed since the last century.

```bash
mkdir -p import/gawie/bogart/cheque/1-in/2016
cp ../Downloads/Bogart/123456789_2016-0*.csv import/gawie/bogart/cheque/1-in/2016/
hledger-flow status
```

```text
Ledger: ~/my-finances   (hledger 1.99.6-gc3a608c2b-20261101 found)

  gawie/bogart/cheque     3 statements (2016)   ✗ no rules file
  gawie/kudu/creditcard   3 statements (2016)   ✓ 3 journals

Next: Create import/gawie/bogart/cheque/bogart-cheque.rules (rules for gawie/bogart/cheque)
      Run: hledger-flow status --rules-template gawie/bogart/cheque
      Save its output as that file, then edit it.
      Docs: https://hledger-flow.github.io/go/rules-files/

Environment:
  hledger-flow  0.19.0 (2a937e9), ghc 9.10, darwin aarch64
  hledger       ~/bin/hledger  (1.99.6-gc3a608c2b-20261101)   --hledger-path
  hledger.conf  none                                          --hledger-conf
  Cores         18, using 18
```

```bash
hledger-flow status --rules-template gawie/bogart/cheque
```

```text
hledger-flow: The first line of import/gawie/bogart/cheque/1-in/2016/123456789_2016-05-28.csv looks like data, not column names. Write the fields line yourself: https://hledger-flow.github.io/go/rules-files/
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
an answer in hledger’s rules, and rules are easier to read, change and share
than a script. He tries rules first.

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
currency R
status *
account1 assets:bogart:cheque

include ../../categories.rules
```

The cheque account brings new categories, which Gawie adds to
`import/gawie/categories.rules`. If you skipped the exercise in part 2, add
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

Next: Generate reports, or explore with hledger
      Run: hledger-flow report
      Or try: hledger balance
2016-03-01 * Monthly Bank Fee
    assets:bogart:cheque                       R-500.00 = R40000.00
    expenses:bank-fees                          R500.00

2016-03-01 * Gereeld Bet Na Rent
    assets:bogart:cheque                      R-5000.00 = R35000.00
    expenses:rent                              R5000.00
```

`hledger-flow import` regenerates every journal each time, so the card’s
statements were imported again too. A change to a rules file always
reaches every statement that uses it.

## An opening balance

```bash
hledger incomestatement
```

```text
hledger: Error: /home/gawie/my-finances/import/gawie/bogart/cheque/3-journal/2016/123456789_2016-03-30.journal:2:57:
  | 2016-03-01 * Monthly Bank Fee
2 |     assets:bogart:cheque                       R-500.00 = R40000.00
  |                                                         ^^^^^^^^^^^
  |     expenses:bank-fees                          R500.00

Balance assertion failed in assets:bogart:cheque
In commodity R at this point, excluding subaccounts, ignoring costs,
the asserted balance is:        R40000.00
but the calculated balance is:   R-500.00
(difference: R40500.00)
To troubleshoot, check this account's running balance with assertions disabled, eg:
hledger reg -E --ignore-assertions '2016-03-02'assets:bogart:cheque$ cur:R -e
```

The balance assertion caught something. Bogart says the account held
R40000 after the first transaction, but hledger only knows about the R500
fee. The account already had R40500 in it before the first statement.

`hledger-flow` includes a file named `<year>-opening.journal` in an account
directory before that year’s statements. Gawie saves the
[opening balance](/reference/balances/) as
`import/gawie/bogart/cheque/2016-opening.journal`:

```hledger title="import/gawie/bogart/cheque/2016-opening.journal"
2016-03-01 Cheque account opening balance
    assets:bogart:cheque              R40500
    equity:opening/closing balances
```

```bash
hledger-flow import
hledger incomestatement --monthly --pretty
```

```text
Imported 6 statements for gawie/bogart/cheque, gawie/kudu/creditcard.

Next: Generate reports, or explore with hledger
      Run: hledger-flow report
      Or try: hledger balance
Monthly Income Statement 2016-03-01..2016-05-31

┌───────────────────────╥───────────┬───────────┬───────────┐
│                       ║       Mar │       Apr │       May │
╞═══════════════════════╬═══════════╪═══════════╪═══════════╡
│ Revenues              ║           │           │           │
├───────────────────────╫───────────┼───────────┼───────────┤
│ income:salary         ║ R37256.28 │ R37256.28 │ R37256.28 │
├───────────────────────╫───────────┼───────────┼───────────┤
│                       ║ R37256.28 │ R37256.28 │ R37256.28 │
╞═══════════════════════╬═══════════╪═══════════╪═══════════╡
│ Expenses              ║           │           │           │
├───────────────────────╫───────────┼───────────┼───────────┤
│ expenses:bank-fees    ║  R1050.23 │   R599.06 │         0 │
│ expenses:car          ║  R1785.00 │  R5455.00 │  R1815.00 │
│ expenses:cash         ║  R3000.00 │  R5000.00 │  R4000.00 │
│ expenses:charity      ║  R4000.00 │  R4500.00 │  R4000.00 │
│ expenses:fun          ║   R389.00 │         0 │  R2905.00 │
│ expenses:groceries    ║  R1977.70 │  R2056.05 │  R2163.25 │
│ expenses:health       ║   R214.90 │         0 │   R189.50 │
│ expenses:household    ║  R4999.00 │  R1840.00 │         0 │
│ expenses:insurance    ║  R5234.43 │  R5234.43 │  R5234.43 │
│ expenses:mopane-worms ║  R6130.00 │  R6495.00 │  R6510.00 │
│ expenses:phone        ║   R250.00 │   R250.00 │   R250.00 │
│ expenses:rent         ║  R5000.00 │  R5000.00 │  R5000.00 │
├───────────────────────╫───────────┼───────────┼───────────┤
│                       ║ R34030.26 │ R36429.54 │ R32067.18 │
╞═══════════════════════╬═══════════╪═══════════╪═══════════╡
│ Net:                  ║  R3226.02 │   R826.74 │  R5189.10 │
└───────────────────────╨───────────┴───────────┴───────────┘
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

┌─────────────────────────────╥────────────┐
│                             ║ 2016-05-30 │
╞═════════════════════════════╬════════════╡
│ Assets                      ║            │
├─────────────────────────────╫────────────┤
│ assets:bogart:cheque        ║  R37416.26 │
│ assets:investments          ║  R15000.00 │
├─────────────────────────────╫────────────┤
│                             ║  R52416.26 │
╞═════════════════════════════╬════════════╡
│ Liabilities                 ║            │
├─────────────────────────────╫────────────┤
│ liabilities:kudu:creditcard ║   R2674.40 │
├─────────────────────────────╫────────────┤
│                             ║   R2674.40 │
╞═════════════════════════════╬════════════╡
│ Net:                        ║  R49741.86 │
└─────────────────────────────╨────────────┘
```

All from rules files, without a line of script. One thing still bothers
Gawie: the year 2016 is written into his rules. That’s where
[part 4](/tutorial/part-4/) begins.

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
