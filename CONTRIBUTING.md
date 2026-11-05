# Hledger Flow Contributor Guidelines

Please read the [Contributor Agreement](#hledger-flow-contributor-agreement) before continuing below.
By submitting a contribution it means that you agree to the terms in the
contributor's agreement.

# List of Contributors

The complete list of contributors can be seen in the git logs or
[on GitHub](https://github.com/apauley/hledger-flow/graphs/contributors).

If you have made a contribution, please consider adding your name (or github
username) to the list below, and include it in a new or existing pull request.

Hledger Flow is brought to you by:

- [Andreas Pauley](https://github.com/apauley)

Miscellaneous contributors:

- [Le Stephane](https://github.com/apauley/hledger-flow/issues?q=author%3Alestephane) (various contributions to issues with suggested solutions and always with thoughtful feedback)
- [Jean-Charles Quillet](https://github.com/apauley/hledger-flow/issues?q=author%3Ajecaro) (improved command-line parameters)
- [Max Linke](https://github.com/apauley/hledger-flow/issues?q=author%3Akain88-de) (added a monthly version of the income statement in reports)
- [Kepi](https://github.com/apauley/hledger-flow/issues?q=author%3Akepi) (doc fixes)
- [Udit Desai](https://github.com/apauley/hledger-flow/issues?q=author%3Adesaiuditd) (Github Action for build, test and release)

# Contributing

To begin contributing, please follow these steps:

1.  [Get the Project](#get-the-project)
2.  [Build the Project](#build-the-project)
3.  [Find an Issue](#find-an-issue)
4.  [Create a Pull Request](#create-a-pull-request)
5.  [Get Your Pull Request Merged](#get-your-pull-request-merged)
6.  [Understand the Release Process](#releases)

## Getting Started

### Get The Project

If you don't already have one, sign up for a free
[Github Account](https://github.com/join).

After you [log into](https://github.com/login) Github using your
account, go to the [Hledger Flow Project Page](https://github.com/apauley/hledger-flow), and click on [Fork](https://github.com/apauley/hledger-flow/fork) to fork the
Hledger Flow repository into your own account.

Once you have forked the repository, you can now clone your forked
repository to your own machine, so you have a complete copy of the
project and can begin safely making your modifications.

To clone your forked repository, first make sure you have installed
[Git](https://git-scm.com/downloads), the version control system used
by Github. Then open a Terminal and type the following commands:

``` bash
mkdir hledger-flow
cd hledger-flow
git clone git@github.com:apauley/hledger-flow.git .
```

This repository has some submodules included, mostly related to the
examples in the documentation.

You need to initialise and update the submodules:

``` bash
git submodule init
git submodule update
```

### Build the Project

You can build and test from a clean checkout with either Stack or Cabal.
[Stack](https://docs.haskellstack.org/en/stable/README/) is the project default
and uses the LTS snapshot pinned in `stack.yaml`.

``` bash
stack build
stack test
```

To build and test with Cabal, run:

``` bash
cabal build all
cabal test all
```

To install the executable with Stack, run `stack install`. On macOS and Linux,
ensure that the installation directory (usually `${HOME}/.local/bin`) is on
your `PATH`. For Bash, add this line to `~/.bashrc`:

``` bash
export PATH="${HOME}/.local/bin:${PATH}"
```

On Windows, run the Stack commands directly from PowerShell. Stack installs
the required GHC and MSYS2 toolchain automatically. Use
`stack path --local-bin` to find the directory containing `hledger-flow.exe`.
Running imports and reports also requires `hledger` on `PATH` (or the
`--hledger-path` option). The [Windows hook reference](docs/README.md#windows-import-hooks)
describes PowerShell scripts, native executables, and the order of discovery.

### Run the command-line scenarios

The shelltest suite exercises imports and reports with real `hledger` and
`hledger-flow` executables. Each scenario creates its own temporary finance
directory. From the repository root on macOS or Linux, with `hledger` on
your `PATH`, run:

``` bash
stack build hledger-flow shelltestrunner
stack exec -- shelltest test/shell
```

On Windows, install the [latest upstream hledger release](https://hledger.org/get-hledger-installed.html) and put `hledger.exe`
on your `PATH`. From Git Bash at the repository root, build `hledger-flow.exe`
from this checkout and run the same scenarios as Windows CI:

``` bash
stack install hledger-flow shelltestrunner --local-bin-path dist
export PATH="$PWD/dist:$PATH"
shelltest --shell=sh --exclude=posix-preprocess.test test/shell test/windows
```

The Linux and macOS jobs exercise the `#!/bin/sh` hook. The Windows case
starts on code page 850, runs both PowerShell hooks with spaces and
apostrophes in their arguments, generates reports, and checks that a failed
construct preserves the previous journal.

For an end-to-end comparison with real statements, import the same annual
CSVs into a rules-only base and a separate base with both hooks. In a UTF-8
terminal, compare `hledger -f all-years.journal print -O csv` and
`hledger -f all-years.journal balance -O csv` between bases. Repeat for
each annual journal, and run `hledger -f all-years.journal check` in both
bases. The Windows ING test covered three years, 290 transactions, and 580
postings. Every posting and each annual and combined balance matched.

### Find an Issue

You may have your own idea about what contributions to make to Hledger
Flow, which is great! If you want to make sure the Hledger Flow
contributors are open to your idea, you can
[open an issue](https://github.com/apauley/hledger-flow/issues/new)
first on the Hledger Flow project site.

Otherwise, if you have no ideas about what to contribute, you can find a
list of issues on the project's [issue tracker](https://github.com/apauley/hledger-flow/issues).

Issues are tagged with various labels, such as `good first issue` or
`help wanted`, which can help you find issues that are a fit for you.

If some issue is confusing or you think you might need help, then just
post a comment on the issue asking for help.

Once you've decided on an issue and understand what is necessary to
complete the issue, then it's a good idea to post a comment on the issue
saying that you intend to work on it. Otherwise, someone else might work
on it too!

### Create a Pull Request

To create a pull request, first push all your changes to your fork of
the project repository:

``` bash
git push
```

Next, [open a new pull request](https://github.com/apauley/hledger-flow/compare) on Github, and select *Compare Across Forks*.
On the right hand side, choose your own fork of the Hledger Flow repository,
in which you've been making your contribution.

Provide a description for the pull request, which details the issue it
is fixing, and has other information that may be helpful to developers
reviewing the pull request.

Finally, click *Create Pull Request*!

### Get Your Pull Request Merged

Once you have a pull request open, it's still your job to get it merged!
To get it merged, a core contributor has to approve the code.

Code reviews can sometimes take a few days, because open source projects
are largely done outside of work, in people's leisure time. Be patient,
but don't wait forever. If you haven't gotten a review within a few
days, then consider gently reminding people that you need a review.

Once you receive a review, you will probably have to go back and make
minor changes that improve your contribution and make it follow existing
conventions in the code base. This is normal, even for experienced
contributors, and the rigorous reviews help ensure that the quality of
the code stays high.

After you make changes, you may need to remind reviewers to check out
the code again. If they give a final approval, it means your code is
ready for merge!

If you don't get a merge in a day after your review is successful, then
please gently remind folks that your code is ready to be merged.

# Releases

## For Contributors

Contributors are not expected to publish releases, push release tags, or upload to Hackage.

What is helpful in contributor pull requests:

- keep tests passing, update the docs and follow the [ChangeLog writing guidelines](ChangeLog.md?plain=1)
- see if it is possible to update the stack dependencies: update `resolver` in `stack.yaml` with a newer version from [Stackage](https://www.stackage.org/)
- for a version bump, follow the [release procedure](release-procedure.md)
- if you want to help validate Hackage packaging, also verify that a source tarball can be produced locally:

``` bash
stack build
stack test
cabal check
cabal sdist
```

Edit package metadata in `package.yaml`. Run `stack test` to regenerate
`hledger-flow.cabal` and commit both files together so Cabal users get the same bounds.

## For Maintainers

See [release-procedure.md](release-procedure.md) for the full release procedure.

# Hledger Flow Contributor Agreement

Thank you for your interest in contributing to the Hledger Flow open
source project.

This is the official contributor agreement for the Hledger Flow project.

The purpose of this agreement is to ensure:

1.  that there is a clear legal status and audit trail for the project
2.  that you get proper credit for your work
3.  that we are able to remain license-compatible with related software by
    updating to newer versions of our license when appropriate (eg maintaining
    compatibility with [hledger](https://hledger.org/))

By submitting a contribution you declare that all of your contributions to
hledger-flow:

- are free of patent violations or copyright violations, to the best of your knowledge
- are released under the hledger-flow project's license
- are granted legal ownership to both yourself and the project leaders of hledger-flow
- may be relicensed in future at the discretion of the project leader

This contributor agreement describes the terms and conditions under which you
may submit a contribution to us. By submitting a contribution to us, you accept
the terms and conditions in the agreement. If you do not accept the terms and
conditions in the agreement, you must not submit any contribution to us.

Although it is not required, we encourage you to add your name to the
[list of contributors](#list-of-contributors) if you have made a contribution to the project.
