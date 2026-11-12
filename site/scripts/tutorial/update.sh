#!/usr/bin/env bash
# Regenerates the command output in the tutorial from a real run.
#
#   site/scripts/tutorial/update.sh           update the pages
#   site/scripts/tutorial/update.sh --check   fail if they're out of date
#
# HLEDGER_FLOW defaults to this checkout's `stack build`. HLEDGER defaults to
# the hledger on PATH, and must be the version in hledger-version.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd -P)
repo=$(cd "$here/../../.." && pwd -P)

hledger_flow=${HLEDGER_FLOW:-$(cd "$repo" && stack path --local-install-root)/bin/hledger-flow}
hledger=${HLEDGER:-$(command -v hledger || echo hledger)}
want=$(tr -d '[:space:]' < "$here/hledger-version")

[ -x "$hledger_flow" ] || { echo "error: no hledger-flow at $hledger_flow. Run stack build, or set HLEDGER_FLOW." >&2; exit 1; }
got=$("$hledger" --version 2>/dev/null || true)
case "$got" in
  "hledger $want,"* | "hledger $want-"*) ;;
  *) echo "error: the tutorial uses hledger $want, but $hledger is ${got:-missing}." >&2
     echo "Download it from https://github.com/hledgerorg/hledger/releases/tag/$want and set HLEDGER to its path." >&2
     exit 1 ;;
esac
command -v tree >/dev/null || { echo "error: the tutorial runs tree. Install it first." >&2; exit 1; }

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
work=$(cd "$work" && pwd -P)
bash "$here/run.sh" "$work/home" "$hledger_flow" "$hledger" > "$work/run.txt"
python3 -I "$here/sync.py" "$work/home" "$work/run.txt" "$@"
