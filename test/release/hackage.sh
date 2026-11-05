#!/usr/bin/env bash
# Exercise the irreversible boundary offline, without access to real credentials.
set -euo pipefail
cd "$(dirname "$0")/../.."
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/mock"
cp bin/release "$work/bin/"
printf 'version: 1.2.3\n' > "$work/package.yaml"
cd "$work"
export PATH="$work/mock:$PATH" HACKAGE_MODE=local
unset HACKAGE_TOKEN
printf 'reviewed bytes' > candidate
digest=$(shasum -a 256 candidate | cut -d' ' -f1)

cat > mock/git <<'MOCK'
#!/usr/bin/env bash
case "$1" in rev-parse) echo same-commit ;; status) ;; *) exit 1 ;; esac
MOCK
cat > mock/gh <<'MOCK'
#!/usr/bin/env bash
case "$1 $2" in
  'release view')
    printf 'hledger-flow-%s\n' linux-x64.tar.gz linux-arm64.tar.gz macos-x64.tar.gz macos-arm64.tar.gz windows-x64.zip ;;
  'secret list') printf '%s\n' "${CONFIGURED_SECRET:-}" ;;
  'api -X') printf '%s\n' "$@" > dispatch; echo 42 ;;
  'run watch') test "$3" = 42 ;;
  *) exit 1 ;;
esac
MOCK
cat > mock/curl <<'MOCK'
#!/usr/bin/env bash
if [[ "$*" == *" -o "* ]]; then cp candidate "${!#}"; else cat candidate; fi
MOCK
cat > mock/cabal <<'MOCK'
#!/usr/bin/env bash
case "$1" in
  user-config) printf '%s\n' "${CABAL_FIELDS:-}" ;;
  check) ;;
  sdist) mkdir -p dist-newstyle/sdist; cp candidate dist-newstyle/sdist/hledger-flow-1.2.3.tar.gz ;;
  upload) cp "${!#}" uploaded; printf '%s\n' "$@" > upload-args ;;
  *) exit 1 ;;
esac
MOCK
chmod +x mock/*
fails() { if "$@" > result 2>&1; then echo "unexpected success: $*" >&2; exit 1; fi; }
fails bin/release hackage
grep -q 'configure a token' result
export CABAL_FIELDS='+ token: dummy'
bin/release hackage > result
grep -q "$digest" result
if grep -qx -- --publish upload-args; then exit 1; fi
rm uploaded upload-args
fails bin/release hackage-publish
fails bin/release hackage-publish "${digest%?}x"
printf changed > candidate
fails bin/release hackage-publish "$digest"
grep -q 'differs from the reviewed' result
test ! -e uploaded
printf 'reviewed bytes' > candidate
bin/release hackage-publish "$digest"
cmp candidate uploaded
grep -qx -- --publish upload-args

export HACKAGE_MODE=ci
fails bin/release hackage
grep -q HACKAGE_TOKEN result
test ! -e dispatch
export CONFIGURED_SECRET=HACKAGE_TOKEN
bin/release hackage > result
grep -Fxq 'inputs[stage]=candidate' dispatch
bin/release hackage-publish "$digest" > result
grep -Fxq 'inputs[stage]=publish' dispatch
grep -Fxq "inputs[sha256]=$digest" dispatch
grep -Fxq 'ref=v1.2.3' dispatch
echo "Hackage release checks passed"
