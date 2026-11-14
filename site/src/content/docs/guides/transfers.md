---
title: Transfers between your accounts
description: Balance transfers when you import statements for both accounts.
---

## The Problem

Suppose you move $200 between two accounts and import both bank statements.
If each statement creates a posting to each bank account, the combined ledger
records a $400 transfer. Here are the two entries that cause the problem.

In `bank1.journal`, imported from `bank1.csv`:

    2018/11/09 Transfer from primary account to secondary account
        assets:bank1:primary    $-200
        assets:bank2:secondary

In `bank2.journal`, imported from `bank2.csv`:

    2018/11/09 Transfer from primary account to secondary account
        assets:bank2:secondary  $200
        assets:bank1:primary

## The Solution

Use a transfer account as the other side of each bank’s posting. In the CSV
rules for each account, classify the matching rows with
`account2 assets:transfers:bank1bank2`. Run `hledger-flow import` from the
ledger root. The two statement entries now look like this.

In `bank1.journal`, imported from `bank1.csv`:

    2018/11/09 Transfer from primary account to secondary account
        assets:bank1:primary         $-200
        assets:transfers:bank1bank2

In `bank2.journal`, imported from `bank2.csv`:

    2018/11/09 Transfer from primary account to secondary account
        assets:bank2:secondary       $200
        assets:transfers:bank1bank2

The first statement puts $200 into the transfer account. The second takes
$200 out. Each bank’s balance changes once. Using a subaccount for each pair
of accounts helps narrow down a difference. Name it after the two accounts in
sorted order, so both sides agree on the name whichever way the money moves:
`bank1bank2`, never `bank2bank1`.

With `LEDGER_FILE` pointing to the combined journal, run
`hledger register assets:transfers:bank1bank2` to see both postings and the
running balance. They can fall on different dates. If a balance remains after
both statements have arrived, check the amounts and classifications.
A zero balance alone can hide offsetting mistakes.

`hledger-flow report` also runs a quarterly balance change report for accounts
whose names contain `transfer`. Look for `transfer-balance.txt` in the
aggregate reports for that year.

## External references

This approach is based on what is described in Full-fledged hledger:
<https://github.com/adept/full-fledged-hledger/wiki/Adding-more-accounts#lets-make-sure-that-transfers-are-not-double-counted>

The question was first asked in [issue \#51](https://github.com/apauley/hledger-flow/issues/51).
