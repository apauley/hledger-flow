#!/usr/bin/env bash
# Exercise the irreversible boundary offline, without access to real credentials.
set -euo pipefail
cd "$(dirname "$0")/../.."
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/mock"
cp bin/release bin/update-homebrew-formula.py "$work/bin/"
printf 'version: 1.2.3\n' > "$work/package.yaml"
cd "$work"
export REAL_GIT
REAL_GIT=$(command -v git)
export PATH="$work/mock:$PATH" HACKAGE_MODE=local
unset HACKAGE_TOKEN
printf 'reviewed bytes' > source
printf true > draft

# Push to a real, isolated Git remote. No production tap or credentials are used.
export work GIT_CONFIG_GLOBAL="$work/gitconfig" GIT_CONFIG_NOSYSTEM=1
git config --global user.name "Release test"
git config --global user.email test@example.invalid
git init -q --bare -b main tap.git
git init -q -b main seed
mkdir -p seed/Formula
{
  echo '  tag = "v1.2.2"'
  for platform in linux-x64 linux-arm64 macos-x64 macos-arm64; do
    printf '    url "#{release}/hledger-flow-%s.tar.gz"\n    sha256 "%064d"\n' "$platform" 0
  done
  printf '    url "https://raw.githubusercontent.com/apauley/hledger-flow/#{tag}/LICENSE"\n    sha256 "%064d"\n' 0
  echo '  depends_on "hledger" => :recommended'
} > seed/Formula/hledger-flow.rb
echo '**hledger-flow 1.2.2** (https://github.com/apauley/hledger-flow/releases/tag/v1.2.2)' > seed/README.md
git -C seed add . && git -C seed commit -qm seed && git -C seed push -q "$work/tap.git" main

cat > mock/git <<'MOCK'
#!/usr/bin/env bash
if [ "$PWD" = "$work" ]; then
  case "$1" in rev-parse) echo same-commit; exit ;; status) exit ;; esac
fi
exec "$REAL_GIT" "$@"
MOCK
cat > mock/gh <<'MOCK'
#!/usr/bin/env bash
set -eu
case "$1 $2" in
  'release view')
    case "$*" in
      *isDraft*) cat draft ;;
      *--jq*|*" -q "*) printf 'hledger-flow-%s\n' linux-x64.tar.gz linux-arm64.tar.gz macos-x64.tar.gz macos-arm64.tar.gz windows-x64.zip ;;
      *) {
           for platform in macos-arm64 macos-x64 linux-arm64 linux-x64; do
             [ "$platform" != "${OMIT_PLATFORM:-}" ] || continue
             printf 'hledger-flow-%s.tar.gz %s\n' "$platform" "$(printf %s "$platform" | shasum -a 256 | cut -d' ' -f1)"
           done
           if [ -e asset ]; then
             printf 'hledger-flow-1.2.3.tar.gz %s\n' "$(shasum -a 256 asset | cut -d' ' -f1)"
           fi
         } | jq -Rn '{assets: [inputs | split(" ") | {name: .[0], digest: ("sha256:" + .[1])}]}' ;;
    esac ;;
  'release upload') test ! -e asset; cp "$4" asset ;;
  'release download') cp asset "${!#}/hledger-flow-1.2.3.tar.gz" ;;
  'release edit') test -e public && test ! -e block-edit && test "$(cat draft)" = true && echo false > draft ;;
  'release verify') test "$(cat draft)" = false ;;
  'secret list') printf '%s\n' "${CONFIGURED_SECRET:-}" ;;
  'api repos/FernTrail/homebrew-tap') echo true ;;
  'repo clone')
    test "$3" = FernTrail/homebrew-tap
    test "$(cat draft)" = false
    test -e public
    git clone -q "$work/tap.git" "$4" ;;
  'run list')
    test "$*" = "run list -R FernTrail/homebrew-tap --workflow test.yml --commit $(git --git-dir="$work/tap.git" rev-parse main) --branch main --json databaseId -q .[0].databaseId" && echo 43 ;;
  'api -X') printf '%s\n' "$@" > dispatch; echo 42 ;;
  'run watch')
    if [ "$3" = 43 ]; then test ! -e "$work/block-tap-ci"; exit; fi
    test "$3" = 42; if grep -Fqx 'inputs[stage]=publish' dispatch; then cp asset public; fi ;;
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
    *://raw.githubusercontent.com/*/v1.2.3/LICENSE) file=LICENSE ;;
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
echo license > LICENSE
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
# A missing platform digest leaves the tap untouched even after publication.
export OMIT_PLATFORM=macos-arm64
fails bin/release publish
grep -q 'SHA-256 for hledger-flow-macos-arm64' result
test "$(git --git-dir=tap.git rev-list --count main)" = 1
unset OMIT_PLATFORM
touch block-tap-ci
fails bin/release publish
grep -q 'inspect tap CI' result
tap_commit=$(git --git-dir=tap.git rev-parse main)
rm block-tap-ci
bin/release publish > result
test "$(git --git-dir=tap.git rev-parse main)" = "$tap_commit"
git --git-dir=tap.git show main:Formula/hledger-flow.rb > formula
grep -Fq 'tag = "v1.2.3"' formula
grep -Fq 'depends_on "hledger" => :recommended' formula
for platform in macos-arm64 macos-x64 linux-arm64 linux-x64; do
  digest=$(printf %s "$platform" | shasum -a 256 | cut -d' ' -f1)
  grep -A1 "hledger-flow-${platform}.tar.gz" formula | grep -Fq "sha256 \"$digest\""
done
grep -Fq "$(shasum -a 256 LICENSE | cut -d' ' -f1)" formula
git --git-dir=tap.git show main:README.md | grep -Fq '**hledger-flow 1.2.3**'
# A late retry of an old release cannot roll back a newer tap.
git -C seed fetch -q "$work/tap.git" main
git -C seed merge -q --ff-only FETCH_HEAD
sed -i.bak 's/v1.2.3/v2.0.0/' seed/Formula/hledger-flow.rb
git -C seed commit -qm newer -- Formula/hledger-flow.rb && git -C seed push -q "$work/tap.git" main
fails bin/release publish
grep -q 'newer release' result
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
echo "Hackage and Homebrew release checks passed"
