# hledger-flow

## What is it?

hledger-flow organises bank statements by owner, bank, account and year.
It uses [hledger](https://hledger.org/) and its CSV rules to generate one
journal per statement, then includes those journals and your manual entries
in `all-years.journal`. You keep the original statements so you can rebuild
the imported entries when your rules change.

It grew out of scripts I wrote while using
[adept's Full-fledged Hledger](https://github.com/adept/full-fledged-hledger/wiki).

## Who should use this?

**Choose hledger-flow** if you keep non-overlapping statements and want to
apply better rules to several years of transactions. It is especially useful
when you manage several accounts or owners and want to review the changes
to each statement's journal before committing them.

**Choose plain `hledger import`** if you want to append new transactions
and then review, split or annotate them directly in your journal. Your edits
survive the next import. This also suits rolling downloads such as “the last
90 days” from the same account. hledger remembers how far it got for each
source, which works well when dates and the order of same-day transactions
are stable. hledger-flow processes each statement independently, so overlapping
files can count the same purchase twice. You can keep the original CSVs with
either workflow. See [hledger's import guide](https://hledger.org/dev/hledger.html#import).

## Overview of the Basic Workflow

1. Save each statement under its [account and year](https://hledger-flow.github.io/reference/input-files/).
2. Add an [hledger rules file](https://hledger-flow.github.io/reference/rules-files/) for the account or bank.
   For most bank CSVs, rules are all you need.
3. Run `hledger-flow import` from the base directory. Use the generated
   `all-years.journal` as your `LEDGER_FILE`.
4. Commit the statements, rules, manual journals and generated journals to
   version control so you can review the effect of later rule changes.

### Example: reclassify purchases across three years

Suppose your 2023–2025 statements are already imported and committed.
Purchases from City Rail currently appear under `expenses:misc`. You also
recorded a one-off cash fare by hand in
`import/john/_manual_/2024/post-import.journal`. Keep handwritten adjustments
in a [manual journal](https://hledger-flow.github.io/reference/manual-journals/), since each
full import rewrites the statement journals in `3-journal/`.

Add this rule to `import/mybank.rules`, the rules file used by those statements:

```rules
if %description City Rail
  account2 expenses:transport
```

From the base directory, rebuild all three years and review the changes:

```sh
hledger-flow import
git diff -- import/
hledger -f all-years.journal print expenses:transport
```

The diff should show the rule and the matching purchases moving from
`expenses:misc` to `expenses:transport` in each affected statement journal.
The cash fare stays in the manual journal. If you booked it to
`expenses:transport`, the last command shows it alongside the rebuilt entries.

## How do I install it?

Install with Homebrew on macOS or Linux:

```sh
brew install ferntrail/tap/hledger-flow
```

Or download a release for Windows, macOS or Linux, or build from source.
The [quick start](https://hledger-flow.github.io/quickstart/) covers each option, how to verify a
download, and your first import.

## Documentation

The full documentation is at **<https://hledger-flow.github.io/>**:

- [Quick start](https://hledger-flow.github.io/quickstart/)
- [Tutorial](https://hledger-flow.github.io/tutorial/)
- [Reference](https://hledger-flow.github.io/reference/status/)
- [Transfers between your accounts](https://hledger-flow.github.io/guides/transfers/)
- [Using Ledger](https://hledger-flow.github.io/guides/ledger/)

## Project Goals

My `hledger` files started to collect a bunch of supporting code that
weren't really specific to my financial situation.

I want to extract and share as much as possible of that supporting code.

[Adept's](https://github.com/adept/full-fledged-hledger/wiki) goals
also resonated with me:

- Tracking expenses should take as little time, effort and manual work
  as possible
- Eventual consistency should be achievable: even if I can't record
  something precisely right now, maybe I would be able to do it later,
  so I should be able to leave things half-done and pick them up later
- Ability to refactor is a must. I want to be able to go back and change
  the way I am doing things, with as little effort as possible and
  without fear of irrevocably breaking things.

# Support

hledger-flow is supported by [FernTrail B.V.](https://ferntrail.tech/)

# Contributing to hledger-flow

Have a look at the [contribution guidelines](CONTRIBUTING.md).
