# hledger-flow

## What is it?

Keep every statement. Rebuild your journals from them.

hledger-flow is a command-line workflow for [hledger](https://hledger.org/),
the plain-text accounting tool. hledger keeps your accounts in journals:
human-readable text files you can open in any editor, in a format that
hledger reads and that
[Ledger](https://hledger-flow.github.io/guides/ledger/) and other plain-text
accounting tools largely share.

hledger-flow is opinionated about where files live. You save each statement,
exactly as your bank issued it, in a folder for its owner, institution,
account and year. `hledger-flow import` converts each one with your hledger
rules, or a script for formats rules can't read, and links the journals
through `all-years.journal`. The statements stay as permanent input, so
every journal can be rebuilt from its source and every entry traced to the
statement it came from.

It grew out of scripts I wrote while using
[adept's Full-fledged Hledger](https://github.com/adept/full-fledged-hledger/wiki).

## Who should use this?

There are many ways to keep your finances with hledger, and
[hledger's workflows page](https://hledger.org/workflows.html) describes them.
Three of them handle bank statements in quite different ways:

- **`hledger import`.** Your journal is the primary record. You classify,
  split and annotate entries by editing the journal, and a better rule changes
  future imports only. Overlapping downloads are fine: hledger skips entries
  it has already imported, as long as their dates and same-day order stay
  stable. [How hledger import works](https://hledger.org/hledger.html#import)
- **Read the CSV directly.** The CSV files are the record and nothing is
  generated. You classify by editing the rules, and a better rule applies the
  next time hledger reads the file. Suits a few files you name yourself, as in
  `hledger -f checking.csv bal`. The hledger 1.99 previews can also `include`
  a CSV in a journal. [Reading CSV with hledger](https://hledger.org/hledger.html#csv)
- **hledger-flow.** The original statements are the primary record, and
  journals are rebuilt from them. You classify by editing rules files, and a
  better rule rebuilds every journal that uses it, in every year. A fixed
  folder for each owner, institution, account and year keeps years of
  statements maintainable. An account's statements must not overlap, or a
  purchase appears twice.

Choose `hledger import` if your journal is your primary record and you want
to edit it by hand. Choose hledger-flow if you want to generate your journals
from statements exactly as they came from the source, classify by editing
rules, and keep years of files in a structure that stays maintainable and
auditable.

Full-fledged hledger teaches this last approach with build scripts you can adapt.
hledger-flow packages the folder conventions, import and standard reports as CLI commands.
[Full-fledged hledger’s principles and practices](https://github.com/adept/full-fledged-hledger/wiki/Key-principles-and-practices)

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

Install with Homebrew on Linux or macOS:

```sh
brew install ferntrail/tap/hledger-flow
```

Or download a release for Linux, macOS, or Windows, or build from source.
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
