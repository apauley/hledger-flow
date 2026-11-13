---
title: Release notices
description: How hledger-flow tells you about new releases, and how to turn the check off.
---

Interactive `import` and `report` commands check GitHub for a newer release
in the background. If a new release is available by the time the command
finishes, it prints an upgrade notice. `status` runs entirely locally.
Homebrew installs get the upgrade command; others get a link to the
[latest release](https://github.com/apauley/hledger-flow/releases/latest).

Set `HLEDGER_FLOW_NO_UPDATE_CHECK=1` to turn the check off.
