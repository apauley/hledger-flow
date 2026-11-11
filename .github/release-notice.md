> [!IMPORTANT]
> **On macOS, extract and run the download from your terminal, not Finder.**
> `hledger-flow` is a command-line program, not an app bundle. If you open it
> from Finder, macOS refuses it with "Apple could not verify", even though it is
> signed and notarized. Instead, in the folder you downloaded to, run
> `tar -xzf hledger-flow-macos-arm64.tar.gz` (`hledger-flow-macos-x64.tar.gz` on an Intel Mac),
> then `./hledger-flow --version`. If macOS asks whether to open a program
> downloaded from the Internet, click **Open**. After that it runs normally.
> [Homebrew](https://github.com/apauley/hledger-flow/blob/master/README.md#install-with-homebrew) installs it without this step.
