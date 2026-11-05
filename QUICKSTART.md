# Start using hledger-flow

hledger-flow turns bank statements into journals and reports. Install
[hledger](https://hledger.org/install.html) separately and put its executable
on your PATH. The release binaries run without a Haskell toolchain.

## Choose and verify your download

Get the archive for your computer from the [official releases](https://github.com/apauley/hledger-flow/releases).

| System | Archive |
| --- | --- |
| Linux on x86-64 | `hledger-flow-linux-x64.tar.gz` |
| Linux on ARM64 | `hledger-flow-linux-arm64.tar.gz` |
| Mac with Apple silicon | `hledger-flow-macos-arm64.tar.gz` |
| Intel Mac | `hledger-flow-macos-x64.tar.gz` |
| Windows x64 | `hledger-flow-windows-x64.zip` |

To verify a download with the [GitHub CLI](https://cli.github.com/), run both
commands in the download folder. Substitute your release tag and archive name.
The first checks the file against the immutable release. The second checks
its build provenance against this GitHub repository.

```text
gh release verify-asset v0.17.0 hledger-flow-linux-x64.tar.gz -R apauley/hledger-flow
gh attestation verify hledger-flow-linux-x64.tar.gz -R apauley/hledger-flow
```

## Install on Linux

Extract the archive for your processor. On x86-64, run:

```sh
tar -xzf hledger-flow-linux-x64.tar.gz
./hledger-flow --version
```

On ARM64, use `hledger-flow-linux-arm64.tar.gz`. Copy the extracted
`hledger-flow` into a directory on your PATH.

## Install on macOS

On a Mac with Apple silicon, run:

```sh
tar -xzf hledger-flow-macos-arm64.tar.gz
./hledger-flow --version
```

On an Intel Mac, use `hledger-flow-macos-x64.tar.gz`. Copy the extracted
`hledger-flow` into a directory on your PATH. The macOS release binary is
signed with a Developer ID and submitted to Apple for notarization.

## Install on Windows

Extract the ZIP using **Extract All**. Open PowerShell in the extracted folder
and run `.\hledger-flow.exe --version`. Use the terminal for this CLI.

The Windows executable is unsigned. If SmartScreen shows **Windows protected
your PC**, verify the ZIP first. If you trust the download, **More info → Run
anyway** starts it when your device policy permits. File Explorer’s
**Properties → Details** shows the version on builds that include version metadata.

Copy `hledger-flow.exe` to a folder on your user PATH, or add its folder under
**Edit environment variables for your account → Path** and open a new terminal.

## Import your first statement

Check that both commands are available in a new terminal:

```text
hledger --version
hledger-flow --version
```

Follow the [step-by-step tutorial](https://github.com/apauley/hledger-flow/blob/master/step-by-step/README.md)
to set up a statement and rules file, or use an existing hledger-flow accounting
directory. In your terminal, change to the directory containing `import/` and run:

```text
hledger-flow import
hledger-flow report
```

Import creates `all-years.journal`. Report writes files under `reports/`.
To query the generated journal directly, run `hledger -f all-years.journal bal`.
If you use `LEDGER_FILE`, point it at this journal. On Linux or macOS, use
`export LEDGER_FILE="$PWD/all-years.journal"` in a POSIX shell. On Windows,
use `$env:LEDGER_FILE = (Resolve-Path .\all-years.journal).Path` in PowerShell.

Rules-based imports work on all three platforms. If your workflow uses
extensionless Unix `preprocess` or `construct` scripts, run it in a POSIX
environment such as Linux, macOS, or WSL with Linux binaries. Native Windows
launches hooks directly and Unix shell scripts rely on an interpreter.
