#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
installer=$PWD/snap/local/install-hledger-flow
package_version=$(awk '/^version:/ {print $2}' package.yaml)
grep -Fxq "version: '$package_version'" snap/snapcraft.yaml || {
  echo "Set the Snap version to $package_version in snap/snapcraft.yaml" >&2
  exit 1
}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"
archive=hledger-flow-linux-x64.tar.gz
printf '%s\n' '#!/bin/sh' 'echo "hledger-flow 1.2.3 linux x86_64"' > hledger-flow
chmod +x hledger-flow
tar -czf "$archive" hledger-flow
digest=$(shasum -a 256 "$archive" | cut -d' ' -f1)
metadata() {
  jq -n --arg tag "v$1" --arg name "$archive" --arg hash "sha256:$digest" \
    '{tag_name: $tag, immutable: true, assets: [{name: $name, digest: $hash}]}' > release.json
}
reject() {
  if bash "$installer" "$1" release.json rejected > log 2>&1; then
    echo "Unexpectedly accepted $2" >&2
    exit 1
  fi
  test ! -e rejected/bin/hledger-flow
}
metadata 1.2.3
bash "$installer" 1.2.3 release.json accepted
test "$(accepted/bin/hledger-flow --version)" = "hledger-flow 1.2.3 linux x86_64"
reject 1.2.4 "metadata from a different release"
metadata 1.2.4
reject 1.2.4 "an older executable with a matching checksum"
metadata 1.2.3
printf corrupt >> "$archive"
reject 1.2.3 "a corrupt archive"
echo "Snap release checks passed"
