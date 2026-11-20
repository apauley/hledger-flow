In an empty folder, run the command for your system to download hledger-flow, extract it, and check its version. On macOS, use Terminal to avoid Finder’s “Apple could not verify” message.

**Linux · Intel or AMD (x86-64)**

```sh
curl -fLo hledger-flow.tar.gz https://github.com/apauley/hledger-flow/releases/download/v@VERSION@/hledger-flow-linux-x64.tar.gz && tar -xzf hledger-flow.tar.gz && ./hledger-flow --version
```

**Linux · ARM64**

```sh
curl -fLo hledger-flow.tar.gz https://github.com/apauley/hledger-flow/releases/download/v@VERSION@/hledger-flow-linux-arm64.tar.gz && tar -xzf hledger-flow.tar.gz && ./hledger-flow --version
```

**macOS · Apple silicon**

```sh
curl -fLo hledger-flow.tar.gz https://github.com/apauley/hledger-flow/releases/download/v@VERSION@/hledger-flow-macos-arm64.tar.gz && tar -xzf hledger-flow.tar.gz && ./hledger-flow --version
```

**macOS · Intel**

```sh
curl -fLo hledger-flow.tar.gz https://github.com/apauley/hledger-flow/releases/download/v@VERSION@/hledger-flow-macos-x64.tar.gz && tar -xzf hledger-flow.tar.gz && ./hledger-flow --version
```

**Windows · x64** — Download the [Windows ZIP](https://github.com/apauley/hledger-flow/releases/download/v@VERSION@/hledger-flow-windows-x64.zip), choose **Extract All**, then run `.\hledger-flow.exe --version` in PowerShell from the extracted folder.
