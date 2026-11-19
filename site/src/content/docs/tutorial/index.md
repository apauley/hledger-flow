---
title: Tutorial
sidebar:
  label: About the tutorial
description: Follow Gawie, a chef in the Kruger Park, from his first CSV statement to reports.
---

Follow Gawie de Groot, a mopane worm chef in the Kruger National Park, as he
takes charge of his finances. Part 1 starts his ledger from the example
that `hledger-flow init` creates. Each part after that brings a new kind of
statement, and each one needs a little more than the last:

1.  [Try the example](/tutorial/part-1/): start a ledger in Git, follow
    the example account through import, report and status, then make the
    account your own.
2.  [The first statement](/tutorial/part-2/): a tidy credit card CSV and the
    rules file `hledger-flow` drafts for it.
3.  [Categories and reports](/tutorial/part-3/): sort spending into accounts
    with `if` rules shared by all of Gawie’s accounts.
4.  [A difficult statement](/tutorial/part-4/): a bank CSV with a preamble,
    Afrikaans dates and no year, still converted by rules alone.
5.  [When rules aren’t enough](/tutorial/part-5/): a `preprocess` script
    that fixes the one thing rules can’t.
6.  [The last resort](/tutorial/part-6/): a `construct` script for an export
    that isn’t a table at all.

The order is the advice: let [hledger’s CSV rules](/reference/rules-files/)
do as much as they can. Add a [preprocess script](/reference/preprocess/)
only when the input needs something rules can’t do, and write your own
transactions with a [construct script](/reference/construct/) only when
nothing else fits.

## Follow along

Download Gawie’s statements into your home folder, keeping the folder names.
The tutorial runs its commands from your home folder and from
`my-finances`, which part 1 creates there.

| File | Used in |
| --- | --- |
| [Downloads/Kudu/creditcard_2016-03-31.csv](/tutorial/Downloads/Kudu/creditcard_2016-03-31.csv) | Part 1 |
| [Downloads/Kudu/creditcard_2016-04-30.csv](/tutorial/Downloads/Kudu/creditcard_2016-04-30.csv) | Part 3 |
| [Downloads/Kudu/creditcard_2016-05-31.csv](/tutorial/Downloads/Kudu/creditcard_2016-05-31.csv) | Part 3 |
| [Downloads/Bogart/123456789_2016-03-30.csv](/tutorial/Downloads/Bogart/123456789_2016-03-30.csv) | Part 4 |
| [Downloads/Bogart/123456789_2016-04-28.csv](/tutorial/Downloads/Bogart/123456789_2016-04-28.csv) | Part 4 |
| [Downloads/Bogart/123456789_2016-05-28.csv](/tutorial/Downloads/Bogart/123456789_2016-05-28.csv) | Part 4 |
| [Downloads/Bogart/123456790_2017-01-27.csv](/tutorial/Downloads/Bogart/123456790_2017-01-27.csv) | Part 5 |
| [Downloads/Goggatjie/tips_2016-05.json](/tutorial/Downloads/Goggatjie/tips_2016-05.json) | Part 6 |

You need `hledger-flow` and `hledger` on your `PATH`; see the
[quick start](/quickstart/). Gawie commits each step with Git, so you need it to
follow along exactly. Part 4 uses hledger’s rules
functions, which need hledger 1.99.6 or newer. Part 6 uses Python 3.

The commands are for a Unix shell (Linux, macOS or WSL). On Windows, see
[Windows import scripts](/reference/windows-hooks/) for the scripts in parts 5 and 6.

## About the output

The output below each command came from running it with these versions:

```text
$ hledger-flow --version
hledger-flow 0.20.1 darwin aarch64 ghc 9.10 0d4990656291b03a490188bde1bb96de951f9ee5
$ hledger --version
hledger 1.99.6-gc3a608c2b-20261101, mac-aarch64
```

hledger 1.99 is a preview of the upcoming hledger 2. Other hledger versions
word their errors and draw their tables slightly differently.

The commands ran in a fresh home folder, with both programs in `~/bin`,
no hledger config file and a Git config that only names the default
branch. hledger-flow shows paths in the home folder as `~`;
hledger’s own error messages show the full path, written here as
`/home/gawie`. Your own paths, and the core count that `hledger-flow status`
reports, will differ. Every
code block on these pages comes from that run or from the files it used;
the [contributing guide](https://github.com/apauley/hledger-flow/blob/master/CONTRIBUTING.md#update-the-tutorial)
explains how to regenerate them.
