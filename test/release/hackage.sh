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
printf 'reviewed bytes' > source
printf true > draft

cat > mock/git <<'MOCK'
#!/usr/bin/env bash
case "$1" in rev-parse) echo same-commit ;; status) ;; *) exit 1 ;; esac
MOCK
cat > mock/gh <<'MOCK'
#!/usr/bin/env bash
set -eu
case "$1 $2" in
  'release view')
    case "$*" in
      *isDraft*) cat draft ;;
      *--jq*|*" -q "*) printf 'hledger-flow-%s\n' linux-x64.tar.gz linux-arm64.tar.gz macos-x64.tar.gz macos-arm64.tar.gz windows-x64.zip ;;
      *) if [ -e asset ]; then
           checksum=$(shasum -a 256 asset | cut -d' ' -f1)
           printf '{"assets":[{"name":"hledger-flow-1.2.3.tar.gz","digest":"sha256:%s"}]}' "$checksum"
         else echo '{"assets":[]}'; fi ;;
    esac ;;
  'release upload') test ! -e asset; cp "$4" asset ;;
  'release download') cp asset "${!#}/hledger-flow-1.2.3.tar.gz" ;;
  'release edit') test -e public && test ! -e block-edit && test "$(cat draft)" = true && echo false > draft ;;
  'release verify') test "$(cat draft)" = false ;;
  'secret list') printf '%s\n' "${CONFIGURED_SECRET:-}" ;;
  'api -X') printf '%s\n' "$@" > dispatch; echo 42 ;;
  'run watch') test "$3" = 42; if grep -Fqx 'inputs[stage]=publish' dispatch; then cp asset public; fi ;;
  *) exit 1 ;;
esac
MOCK
cat > mock/curl <<'MOCK'
#!/usr/bin/env bash
set -eu
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o) out=$2; shift ;;
    -w) status=true; shift ;;
    *://*/candidate/*) file=candidate ;;
    *://*) file=public ;;
  esac
  shift
done
if [ -e "$file" ]; then cp "$file" "$out"; code=200; else code=404; fi
if [[ ${status:-} ]]; then echo "$code"; else test "$code" = 200; fi
MOCK
cat > mock/cabal <<'MOCK'
#!/usr/bin/env bash
case "$1" in
  user-config) printf '%s\n' "${CABAL_FIELDS:-}" ;;
  check) ;;
  sdist) mkdir -p dist-newstyle/sdist; cp source dist-newstyle/sdist/hledger-flow-1.2.3.tar.gz ;;
  upload)
    if [[ "$*" == *--publish* ]]; then cp "${!#}" public; echo publish >> uploads
    else cp "${!#}" candidate; echo candidate >> uploads; fi ;;
  *) exit 1 ;;
esac
MOCK
chmod +x mock/*
fails() { if "$@" > result 2>&1; then echo "unexpected success: $*" >&2; exit 1; fi; }
fails bin/release hackage
grep -q 'configure a token' result
export CABAL_FIELDS='+ token: dummy'
bin/release hackage > result
cmp source asset
cmp asset candidate
bin/release hackage > result
test "$(cat uploads)" = candidate
printf changed > candidate
fails bin/release publish
grep -q 'differs from the source archive' result
test ! -e public
test "$(cat draft)" = true
cp asset candidate
touch block-edit
fails bin/release publish
cmp asset public
rm block-edit
bin/release publish > result
bin/release publish > result
test "$(cat draft)" = false
test "$(cat uploads)" = "$(printf 'candidate\npublish')"

# Even a completed release must fail verification if the public bytes differ.
printf changed > public
fails bin/release publish
grep -q 'differs from the source archive' result
rm public
export HACKAGE_MODE=ci
fails bin/release hackage
grep -q HACKAGE_TOKEN result
test ! -e dispatch
export CONFIGURED_SECRET=HACKAGE_TOKEN
bin/release hackage > result
grep -Fxq 'inputs[stage]=candidate' dispatch
bin/release hackage-publish > result
grep -Fxq 'inputs[stage]=publish' dispatch
grep -Fxq 'ref=v1.2.3' dispatch
rm asset
fails bin/release hackage-publish
grep -q 'missing source archive' result
echo "Hackage release checks passed"
