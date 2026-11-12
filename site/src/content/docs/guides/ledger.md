---
title: Using Ledger
description: Use ledger-cli alongside hledger on the journals hledger-flow generates.
---

`hledger-flow` should work with any recent version of `hledger`.

`hledger-flow` uses `hledger` to produce journals, so this page about [hledger and Ledger](https://hledger.org/ledger.html) should be relevant for all `hledger-flow` users.

That said, here are some observations that are specifically relevant to `hledger-flow`:

When writing out the journal include files, `hledger-flow` sorts the
include statements by filename.

[Ledger](https://www.ledger-cli.org/) fails any balance assertions
when the transactions aren't included in chronological order.

An easy way around this is to name your input files so that March's
statement is listed before December's statement.

Another option is to add `--permissive` to any
[ledger](https://www.ledger-cli.org/) command.

So you should easily be able to use both `ledger` and `hledger` on these
journals if you take care to [avoid the few incompatibilities](https://hledger.org/faq.html#how-is-hledger-different-from-ledger-) which exists
(eg in your rules files or manual journals).
