#!/usr/bin/env bash
# Runs the tutorial's commands in a scratch home and prints each command
# with its output. update.sh calls this; run it directly to debug a part.
# Usage: run.sh HOME_DIR HLEDGER_FLOW HLEDGER
set -u
here=$(cd "$(dirname "$0")" && pwd -P)
repo=$(cd "$here/../../.." && pwd -P)
home=$1

mkdir -p "$home/bin"
cp "$2" "$home/bin/hledger-flow"
cp "$3" "$home/bin/hledger"
ln -s "$(command -v tree)" "$home/bin/tree"
cp -R "$repo/site/public/tutorial/Downloads" "$home/"

# The same environment on every machine, so the output only depends on the
# two programs and the tutorial's files. HOME is the scratch home, so no
# personal hledger config (such as ~/.hledger.conf) changes the output.
export HOME=$home PATH=$home/bin:/usr/bin:/bin:/usr/sbin:/sbin
export LANG=C.UTF-8 TZ=UTC COLUMNS=80 TREE_CHARSET=ASCII NO_COLOR=1
export HLEDGER_FLOW_NO_UPDATE_CHECK=1
export GIT_AUTHOR_NAME=Gawie GIT_AUTHOR_EMAIL=gawie@example.com
export GIT_COMMITTER_NAME=Gawie GIT_COMMITTER_EMAIL=gawie@example.com
unset LC_ALL LEDGER_FILE XDG_CONFIG_HOME
cd "$home"

# c COMMAND: run a command the tutorial shows, and record its output.
# Quote a command that has a glob, so it's recorded as the reader types it.
c() { echo; echo "===== \$ $*"; eval "$@" 2>&1; echo "[rc=$?]"; }
# q COMMAND: run a step the tutorial only describes, such as a git commit.
q() { eval "$@" >/dev/null 2>&1 || echo "FAILED: $*"; }
# put PATH [FILES]: save the file the reader writes, from files/<part>/PATH.
put() { mkdir -p "$(dirname "$1")"; cp "$here/files/${2:-$part}/$1" "$1"; }
commit() { q git add -A; q git commit -q -m "\"$1\""; }

c hledger-flow --version
c hledger --version
for part in part1 part2 part3 part4 part5; do
  echo; echo "######## $part"
  . "$here/$part.sh"
done
