# Sourced from each shelltest case, with the repository root as its working directory.
finance=$(mktemp -d "${TMPDIR:-/tmp}/hledger-flow.XXXXXXXX")
trap 'cd "$finance/.." && rm -rf "$finance"' EXIT
cd "$finance"
unset LEDGER_FILE
export XDG_CONFIG_HOME="$finance/config"

account=import/gawie/bogart/cheque
inbox=$account/1-in/2016
journal=$account/3-journal/2016/march.journal
mkdir -p "$inbox"
printf '%s\n' '2016-03-01,Monthly bank fee,-5' > "$inbox/march.csv"
cat > import/bogart.rules <<'RULES'
fields date, description, amount
account1 assets:bank:cheque
account2 expenses:bank-fees
RULES

# Successful imports and reports log timings and absolute paths.
flow() {
    hledger-flow "$@" > "$finance/flow.log" 2>&1 || {
        status=$?
        cat "$finance/flow.log" >&2
        return "$status"
    }
}
