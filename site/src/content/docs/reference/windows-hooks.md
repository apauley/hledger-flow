---
title: Windows import hooks
description: PowerShell and executable preprocess and construct hooks on native Windows.
---

In the account directory, Windows checks each hook name in this order:
the extensionless file first, then `.ps1`, then `.exe`. For example,
`construct.ps1` takes precedence over `construct.exe`. Move an old
extensionless launcher out of the account directory to use the script directly.
These are the supported filenames. To run a batch file or another scripting
language, invoke its interpreter from a PowerShell hook.

hledger-flow runs `.ps1` files with Windows PowerShell, using the current
execution policy. From **Command Prompt** (`cmd.exe`), this example imports
a base under Documents, permits local scripts for the child PowerShell
process, and checks the combined journal:

```bat
cd /d "%USERPROFILE%\Documents\Finance\My Ledger"
powershell.exe -NoProfile -ExecutionPolicy RemoteSigned -Command "hledger-flow import; exit $LASTEXITCODE"
chcp 65001 >nul
hledger -f all-years.journal check
```

In **PowerShell**, the same folder is `"$HOME\Documents\Finance\My Ledger"`.
You can use `Set-ExecutionPolicy -Scope Process RemoteSigned` there and then
run `hledger-flow import` directly.

hledger-flow uses UTF-8 on Windows while running imports and reports, then
restores the console’s previous code page. Names such as `Morgan’s Current`
are preserved in hook arguments and journal includes. When running `hledger`
separately, set the console to UTF-8 with `chcp 65001` in that terminal.
Use the same setting before starting overlapping imports that share a Windows
console, since those processes share its code page. For CSVs saved in a
different encoding, set the CSV rules file’s `encoding` directive. Save
journal and rules files that contain non-ASCII text in UTF-8.

Both scripts receive the same five positional arguments as the extensionless
hooks. A PowerShell hook can start with
`param($Source, $Destination, $Bank, $Account, $Owner)`.
For `preprocess.ps1`, write the CSV to `$Destination`. For `construct.ps1`,
`$Destination` is the literal `-`: write the journal to standard output.
Use `throw` or `exit` with a nonzero code to signal failure. If your hook ends
by running a native command, use `exit $LASTEXITCODE` to forward its result.
