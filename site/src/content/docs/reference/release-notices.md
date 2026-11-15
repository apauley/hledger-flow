---
title: Release notices
description: How hledger-flow tells you about new releases, and how to turn the check off.
---

Interactive `import` and `report` share the latest stable release they have
seen on GitHub. They save it in `.hledger-flow/cache/release-check.json` at
the ledger root, including when you run an ordinary import. The cache uses
the same Git ignore file as [incremental import](/reference/incremental-import/).

A successful check lasts 24 hours. After that, the next interactive import
or report refreshes it in the background. If GitHub is unavailable, the
command can still show a previously discovered upgrade and waits 15 minutes
before retrying. On the first check, a command allows up to a quarter of a
second at exit for a result to arrive. Once a release is cached, commands
can show it immediately.

Interactive `status` (or just `hledger-flow` inside the ledger) reads the
saved release. It never contacts GitHub or creates a cache. Once you upgrade,
the notice disappears as soon as the installed version matches or exceeds
the saved version.

Notices go to standard error when both output streams are terminals.
Homebrew installs get the upgrade command when the tap has been confirmed
to offer that release. Other notices link to the
[latest release](https://github.com/apauley/hledger-flow/releases/latest).
`status --json` and `status --rules-template` skip notices.

Set `HLEDGER_FLOW_NO_UPDATE_CHECK=1` to disable release checks and notices.
