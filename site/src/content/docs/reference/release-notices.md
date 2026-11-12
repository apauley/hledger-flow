---
title: Release notices
description: How hledger-flow tells you about new releases, and how to turn the check off.
---

When you run hledger-flow in a terminal, it checks GitHub for a newer release.
`status` tells you whether you're up to date. `import` and `report` mention a
new release only when one is available, and never wait for the check.
Homebrew installs get the upgrade command; others get a link to the
[latest release](https://github.com/apauley/hledger-flow/releases/latest).

Set `HLEDGER_FLOW_NO_UPDATE_CHECK=1` to turn the check off.
