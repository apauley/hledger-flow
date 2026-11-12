---
title: Transfers between your accounts
description: Balance transfers when you import statements for both accounts.
---

## The Problem

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

## The Solution

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

## External references

This approach is based on what is described in Full-fledged hledger:
<https://github.com/adept/full-fledged-hledger/wiki/Adding-more-accounts#lets-make-sure-that-transfers-are-not-double-counted>

The question was first asked in [issue \#51](https://github.com/apauley/hledger-flow/issues/51).
