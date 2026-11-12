#!/usr/bin/env bash
# Exercise the irreversible boundary offline, without access to real credentials.
set -euo pipefail
cd "$(dirname "$0")/../.."
work=$(mktemp -d)
trap 'status=$?; if [ "$status" -ne 0 ] && [ -f "$work/result" ]; then cat "$work/result" >&2; fi; rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/mock"
cp bin/release bin/hackage-artifacts.py bin/update-homebrew-formula.py "$work/bin/"
printf 'version: 1.2.3\n' > "$work/package.yaml"
cd "$work"
export REAL_GIT
REAL_GIT=$(command -v git)
export PATH="$work/mock:$PATH" HACKAGE_MODE=local
export REAL_PYTHON
REAL_PYTHON=$(command -v python3)
unset HACKAGE_TOKEN
mkdir hledger-flow-1.2.3 assets
printf 'reviewed bytes' > hledger-flow-1.2.3/marker
tar -czf source hledger-flow-1.2.3
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
           for file in assets/*; do
             [ -f "$file" ] || continue
             printf '%s %s\n' "${file##*/}" "$(shasum -a 256 "$file" | cut -d' ' -f1)"
           done
           if [ -e asset ]; then
             printf 'hledger-flow-1.2.3.tar.gz %s\n' "$(shasum -a 256 asset | cut -d' ' -f1)"
           fi
         } | jq -Rn '{assets: [inputs | split(" ") | {name: .[0], digest: ("sha256:" + .[1])}]}' ;;
    esac ;;
  'release upload')
    name=$(basename "$4")
    dest="assets/$name"; [ "$name" != hledger-flow-1.2.3.tar.gz ] || dest=asset
    test ! -e "$dest"; cp "$4" "$dest" ;;
  'release download')
    name=$7
    src="assets/$name"; [ "$name" != hledger-flow-1.2.3.tar.gz ] || src=asset
    cp "$src" "${!#}/$name" ;;
  'release edit') test -e public && test ! -e block-edit && test "$(cat draft)" = true && echo false > draft ;;
  'release verify') test "$(cat draft)" = false ;;
  'secret list') printf '%s\n' "${CONFIGURED_SECRET:-}" ;;
  'api repos/FernTrail/homebrew-tap') echo true ;;
  'api repos/FernTrail/homebrew-tap/commits/main') git --git-dir="$work/tap.git" log -1 --format='%an <%ae>' main ;;
  'config get') echo https ;;
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
cat > mock/python3 <<'MOCK'
#!/usr/bin/env bash
if [[ -n ${HACKAGE_TOKEN:-} && "$*" == *"$HACKAGE_TOKEN"* ]]; then echo "token in python arguments" >&2; exit 1; fi
exec "$REAL_PYTHON" "$@"
MOCK
cat > mock/curl <<'MOCK'
#!/usr/bin/env bash
set -eu
if [[ -n ${HACKAGE_TOKEN:-} && "$*" == *"$HACKAGE_TOKEN"* ]]; then echo "token in curl arguments" >&2; exit 1; fi
# The Python helper returns response bodies followed by a newline and HTTP code.
if [[ "$*" == *--write-out* ]]; then
  url=${!#}; url=${url%%\?*}; code=200
  docs=public-docs
  [[ "$url" != */candidate/* ]] || docs=candidate-docs
  if [[ "$*" == *--config* ]]; then
    auth=$(cat)
    test "$auth" = "header = \"Authorization: X-ApiKey ${HACKAGE_TOKEN:-dummy}\""
  fi
  case "$url" in
    https://hackage.haskell.org/packages/candidates/|https://hackage.haskell.org/packages/)
      test -n "$HACKAGE_TOKEN"
      test -n "$auth"
      [[ "$*" == *"Accept: text/plain"* ]]
      while [ "$1" != --form ]; do shift; done
      [[ "$2" == package=@* ]]
      if [[ "$url" == */packages/ ]]; then cp "${2#package=@}" public; echo publish >> uploads
      else cp "${2#package=@}" candidate; echo candidate >> uploads; fi
      code=200 ;;
    */docs.tar)
      if [ -e "$docs" ]; then cat "$docs"; else code=404; fi ;;
    */docs)
      while [ "$1" != --upload-file ]; do shift; done
      gzip -dc "$2" > "$docs"
      echo "$docs" >> writes
      [ ! -e "interrupt-$docs" ] || exit 52
      code=204 ;;
    */docs/doc-index.html) test -s "$docs"; echo '<title>hledger-flow (Index)</title>' ;;
    */reports/.txt)
      if [ -e report ]; then echo '[(BuildReportId 1,BuildReport {...})]'; else echo '[]'; fi ;;
    */reports/1.txt) cat report ;;
    */reports/)
      while [ "$1" != --data-binary ]; do shift; done
      test ! -e report
      cp "${2#@}" report
      echo report >> writes
      [ ! -e interrupt-report ] || exit 52
      code=303 ;;
    */hledger-flow-1.2.3)
      test -f public-docs
      echo '<div id="badges">'
      echo 'label=Documentation&message=Available&color=success'
      if [ -e report ] && [ ! -e red-badge ]; then echo 'label=Build&message=InstallOk&color=success'; fi
      echo '</div>' ;;
    *) exit 1 ;;
  esac
  printf '\n%s' "$code"; exit
fi
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
set -eu
if [[ -n ${HACKAGE_TOKEN:-} && "$*" == *"$HACKAGE_TOKEN"* ]]; then echo "token in cabal arguments" >&2; exit 1; fi
case "$1" in
  haddock)
    test "$(cat marker)" = 'reviewed bytes'
    mkdir -p dist-newstyle
    printf docs | gzip > dist-newstyle/hledger-flow-1.2.3-docs.tar.gz ;;
  build)
    test "$(cat marker)" = 'reviewed bytes'
    test ! -e "$work/block-build"
    pkg=hledger-flow-1.2.3
    dir=${3#--build-summary=}
    cat > "${dir%/*}/$pkg.summary" <<REPORT
package: $pkg
install-outcome: InstallOk

package: $pkg
compiler: ghc-9.10.3
dependencies:
    $pkg
install-outcome: InstallOk
docs-outcome: NotTried
tests-outcome: NotTried
REPORT
    ;;
  list-bin) echo /usr/bin/true ;;
  user-config) printf '%s\n' "${CABAL_FIELDS:-}" ;;
  check) ;;
  sdist) mkdir -p dist-newstyle/sdist; cp source dist-newstyle/sdist/hledger-flow-1.2.3.tar.gz ;;
  *) exit 1 ;;
esac
MOCK
echo license > LICENSE
chmod +x mock/*
fails() { if "$@" > result 2>&1; then echo "unexpected success: $*" >&2; exit 1; fi; }
fails bin/release hackage
grep -q 'configure a token' result
export CABAL_FIELDS='+ token: dummy'
export HACKAGE_TOKEN=test-only-source-upload-token
touch block-build
fails bin/release hackage
test -e assets/hledger-flow-1.2.3-docs.tar.gz
test ! -e assets/hledger-flow-1.2.3.report
test ! -e public
rm block-build
touch interrupt-candidate-docs
fails bin/release hackage
rm interrupt-candidate-docs
bin/release hackage > result
cmp source asset
cmp asset candidate
bin/release hackage > result
test "$(cat uploads)" = candidate
printf changed > candidate
fails bin/release publish
grep -q 'differs from the asset' result
test ! -e public
test "$(cat draft)" = true
cp asset candidate
touch interrupt-public-docs
fails bin/release publish
cmp asset public
unset HACKAGE_TOKEN
test ! -e report
rm interrupt-public-docs
touch interrupt-report
fails bin/release publish
cmp assets/hledger-flow-1.2.3.report report
grep -Eq '^time: ' report
test "$(cat draft)" = true
rm interrupt-report
touch red-badge
fails bin/release hackage-publish
grep -q 'green Build badge' result
rm red-badge
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
# Tap commits use the identity of the tap checkout named by release.tapDir.
git clone -q tap.git tap-checkout
git -C tap-checkout remote set-url origin git@github.com:FernTrail/homebrew-tap.git
git -C tap-checkout config user.name 'Tap owner'
git -C tap-checkout config user.email tap@example.invalid
git config --global release.tapDir "$work/seed"
fails bin/release publish
grep -q 'not a checkout of FernTrail/homebrew-tap' result
git config --global release.tapDir "$work/tap-checkout"
touch block-tap-ci
fails bin/release publish
grep -q 'inspect tap CI' result
grep -q 'warning: the latest FernTrail/homebrew-tap commit is by Release test' result
tap_commit=$(git --git-dir=tap.git rev-parse main)
test "$(git --git-dir=tap.git log -1 --format='%an <%ae> %cn <%ce>' main)" = 'Tap owner <tap@example.invalid> Tap owner <tap@example.invalid>'
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

test "$(cat writes)" = "$(printf 'candidate-docs\npublic-docs\nreport')"

# Even a completed release must fail verification if the public bytes differ.
printf changed > public
fails bin/release publish
grep -q 'differs from the asset' result
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
# A previous run can have published the source before completing the uploads.
rm dispatch
bin/release hackage-publish > result
grep -Fxq 'inputs[stage]=publish' dispatch
rm asset
fails bin/release hackage-publish
grep -q 'missing source archive' result
echo "Hackage and Homebrew release checks passed"
