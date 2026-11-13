# Start using hledger-flow

hledger-flow turns bank statements into journals and reports using
[hledger](https://hledger.org/). Homebrew installs both. If you download a
release instead, [install hledger](https://hledger.org/install.html) separately
and put its executable on your PATH. The release binaries run without a Haskell toolchain.

## Install with Homebrew

On a [supported Mac or Linux system](https://github.com/FernTrail/homebrew-tap#supported-platforms),
check that `brew --version` works in your terminal. If you’re new to Homebrew,
[install Homebrew](https://brew.sh/) and follow the installer’s instructions to add `brew` to your `PATH`.

Install hledger-flow and its `hledger` dependency:

```sh
brew install ferntrail/tap/hledger-flow
```

Homebrew adds FernTrail’s package repository (its *tap*) automatically and
trusts only the named formula. See [Homebrew’s tap trust guide](https://docs.brew.sh/Tap-Trust).
To inspect the package before installing it (Homebrew 6 or newer):

```sh
brew trust --formula ferntrail/tap/hledger-flow
brew info ferntrail/tap/hledger-flow
```

If you don’t want Homebrew to install `hledger`, add `--without-hledger` to the install command.
For later releases, run `brew update` and `brew upgrade ferntrail/tap/hledger-flow`.

Skip ahead to [import your first statement](#import-your-first-statement).

## Download a release

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

Extract and run the download from your terminal, not Finder. `hledger-flow`
is a command-line program, not an app bundle. If you open it from Finder,
macOS refuses it with "Apple could not verify", even though it is signed with
a Developer ID and notarized.

On a Mac with Apple silicon, run this in the folder you downloaded to:

```sh
tar -xzf hledger-flow-macos-arm64.tar.gz
./hledger-flow --version
```

On an Intel Mac, use `hledger-flow-macos-x64.tar.gz`. If macOS asks whether
to open a program downloaded from the Internet, click **Open**. After that it
runs normally. Copy the extracted `hledger-flow` into a directory on your PATH.

## Install on Windows

Extract the ZIP using **Extract All**. Open PowerShell in the extracted folder
and run `.\hledger-flow.exe --version`. Use the terminal for this CLI.

The Windows executable is unsigned. If SmartScreen shows **Windows protected
your PC**, verify the ZIP first. If you trust the download, **More info → Run
anyway** starts it when your device policy permits. File Explorer’s
**Properties → Details** shows the version on builds that include version metadata.

Copy `hledger-flow.exe` to a folder on your user PATH, or add its folder under
**Edit environment variables for your account → Path** and open a new terminal.

## Build from source

From a checkout of the [repository](https://github.com/apauley/hledger-flow), run `stack install`.
Add the directory reported by `stack path --local-bin` to your `PATH` and [install hledger](https://hledger.org/install.html).
See the [build instructions](https://github.com/apauley/hledger-flow/blob/master/CONTRIBUTING.md#build-the-project) for prerequisites and tests.

## Import your first statement

Check that both commands are available in a new terminal:

```text
hledger --version
hledger-flow --version
```

To use a specific copy of hledger, pass its path before the command, for
example `hledger-flow --hledger-path /path/to/hledger import`.

Start a ledger in an empty folder, then let `hledger-flow status` guide you.
Each `status` ends with a `Next:` line saying exactly what to do:

```text
hledger-flow init ~/finances
cd ~/finances
hledger-flow status
```

1. Put a statement where `status` says, for example
   `import/me/mybank/checking/1-in/2026/statement.csv`.
2. Create the rules file `status` names. For a CSV with a header row,
   `hledger-flow status --rules-template me/mybank/checking` prints a starter
   to save as that file and edit.
3. Run `hledger-flow import`. It creates `all-years.journal`.
4. Point `LEDGER_FILE` at that journal with the line `status` prints, then run
   `hledger-flow report`. Reports go under `reports/`.

Running `hledger-flow` with no command does the same as `status` inside a
ledger. Scripts and AI agents can use `hledger-flow status --json`; see
[status for scripts and agents](https://hledger-flow.github.io/reference/status/#status-for-scripts-and-agents).
The [step-by-step tutorial](https://hledger-flow.github.io/tutorial/)
explains each part in depth.

On Windows, run `chcp 65001` before querying the journal with standalone
hledger. This sets the current terminal session to UTF-8 so currency
symbols and accented letters display correctly. Then run
`hledger -f all-years.journal bal`.
In PowerShell, set `LEDGER_FILE` with
`$env:LEDGER_FILE = (Resolve-Path .\all-years.journal).Path`.

Rules-based imports work on all three platforms. On native Windows you can
put `preprocess.ps1` and `construct.ps1`, or their `.exe` equivalents, in
the account directory. See [Windows import hooks](https://hledger-flow.github.io/reference/windows-hooks/)
for the arguments and commands to use from Command Prompt or PowerShell.
For extensionless Unix shell scripts, use Linux, macOS, or WSL with Linux binaries.
