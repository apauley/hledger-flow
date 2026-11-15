#!/usr/bin/env bash
set -euo pipefail

version=$(tr -d '[:space:]' < site/scripts/tutorial/hledger-version)
case "$RUNNER_OS-$RUNNER_ARCH" in
  Linux-X64)    asset=hledger-linux-x64 ;;
  macOS-X64)    asset=hledger-mac-x64 ;;
  macOS-ARM64)  asset=hledger-mac-arm64 ;;
  Windows-X64)  asset=hledger-windows-x64 ;;
  *) echo "No hledger release configured for $RUNNER_OS-$RUNNER_ARCH" >&2; exit 1 ;;
esac

dir="$RUNNER_TEMP/hledger-$version"
mkdir -p "$dir"
cd "$dir"
gh release download "$version" -R hledgerorg/hledger --pattern "$asset.*"
if [[ -f "$asset.tar.gz" ]]; then
  tar -xzf "$asset.tar.gz"
else
  unzip -q "$asset.zip"
fi

exe=hledger
if [[ $RUNNER_OS == Windows ]]; then exe=hledger.exe; fi
binary=$(find "$PWD" -type f -name "$exe" -print -quit)
[[ -f "$binary" ]] || { echo "Missing $exe in the $asset archive" >&2; exit 1; }
chmod +x "$binary"
installed=$("$binary" --version)
echo "$installed"
case "$installed" in
  "hledger $version,"* | "hledger $version-"*) ;;
  *) echo "Expected hledger $version in $asset" >&2; exit 1 ;;
esac
bin_dir=$(dirname "$binary")
if [[ $RUNNER_OS == Windows ]]; then bin_dir=$(cygpath -w "$bin_dir"); fi
echo "$bin_dir" >> "$GITHUB_PATH"
