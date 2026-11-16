#!/usr/bin/env bash
# Exercise a real version bump and tutorial regeneration without touching the release checkout.
set -euo pipefail
cd "$(dirname "$0")/../.."
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
git ls-files -z | tar -cf - --null -T - | tar -xf - -C "$work"
cd "$work"
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
git init -q -b master
git config user.name "Release test"
git config user.email test@example.invalid
git add .
git commit -qm 'Previous release'
git tag "v$(awk '/^version:/ {print $2}' package.yaml)"
printf '## Unreleased\n\n- Exercise the release bump.\n\n' > ChangeLog.md
git commit -qam 'Prepare release notes'
before=$(git rev-parse HEAD)

if HLEDGER="$work/missing-hledger" bin/release bump patch; then
  echo "Bump accepted an unavailable tutorial hledger" >&2
  exit 1
fi
test "$(git rev-parse HEAD)" = "$before"
test -z "$(git status --porcelain)"

# An inherited override must not substitute an installed binary for the new build.
HLEDGER_FLOW="$work/missing-hledger-flow" bin/release bump patch
test "$(git rev-parse HEAD^)" = "$before"
test -z "$(git status --porcelain)"
version=$(awk '/^version:/ {print $2}' package.yaml)
git show HEAD:site/src/content/docs/tutorial/index.md | grep -F "hledger-flow $version "
HLEDGER_FLOW='' site/scripts/tutorial/update.sh --check
