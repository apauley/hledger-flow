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
# A stale package year must advance even after the real repository is up to date.
perl -pi -e 's/^copyright: "[0-9]{4} /copyright: "2000 /' package.yaml
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
year=$(date +%Y)
grep -Fx "copyright: \"$year Andreas Pauley\"" package.yaml
grep -Ex "copyright:[[:space:]]+$year Andreas Pauley" hledger-flow.cabal
version=$(awk '/^version:/ {print $2}' package.yaml)
git show HEAD:site/src/content/docs/tutorial/index.md | grep -F "hledger-flow $version "
HLEDGER_FLOW='' site/scripts/tutorial/update.sh --check
