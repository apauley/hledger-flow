#!/usr/bin/env python3
"""Generate synthetic statements for hledger-flow import benchmarks."""

import argparse
import csv
import os
from pathlib import Path
import sys
from datetime import date, timedelta

# Each group supplies both the raw bank category and merchant names to match.
MERCHANTS = {
    "groceries": ("North Market", "Corner Foods", "North Market refund"),
    "transport": ("City Rail", "Town Bus"),
}
EXAMPLES = [(name, category) for category, names in MERCHANTS.items() for name in names]
EXAMPLES.append(("River Kiosk", "unknown"))
DATES = [(date(2024, 1, 1) + timedelta(days=i)).isoformat() for i in range(366)]
RAW_FIELDS = ("date", "reference", "description", "amount", "category")


def write_statements(account_dir, first_ordinal, statements, transactions, seed):
    inbox = account_dir / "1-in" / "2024"
    inbox.mkdir(parents=True)
    ordinal = first_ordinal
    for number in range(1, statements + 1):
        with (inbox / f"statement-{number:04}.csv").open("w", encoding="utf-8", newline="") as out:
            writer = csv.writer(out, lineterminator="\n")
            writer.writerow(RAW_FIELDS)
            for _ in range(transactions):
                description, category = EXAMPLES[ordinal % len(EXAMPLES)]
                cents = 100 + (seed + ordinal * 7919) % 25000
                sign = "" if description.endswith(" refund") else "-"
                amount = f"{sign}{cents // 100}.{cents % 100:02}"
                day = DATES[(seed + ordinal) % len(DATES)]
                writer.writerow((day, f"T{ordinal + 1:06}", description, amount, category))
                ordinal += 1


HOOK_BODY = '''
import csv
from decimal import Decimal
import sys

mode, source, destination, bank, account, owner = sys.argv[1:]
with open(source, encoding="utf-8", newline="") as statement:
    rows = csv.DictReader(statement)
    if mode == "preprocess":
        with open(destination, "w", encoding="utf-8", newline="") as output:
            writer = csv.writer(output, lineterminator="\\n")
            writer.writerow(("date", "reference", "description", "amount", "counteraccount"))
            for row in rows:
                counter = "expenses:" + CATEGORIES.get(row["description"], "unknown")
                writer.writerow((row["date"], row["reference"], row["description"], row["amount"], counter))
    else:
        for row in rows:
            if mode == "combined":
                counter = row["counteraccount"]
            else:
                counter = "expenses:" + CATEGORIES.get(row["description"], "unknown")
            amount = Decimal(row["amount"])
            print(f"{row['date']} ({row['reference']}) {row['description']}")
            print(f"    assets:{bank}:{account}  {amount} USD")
            print(f"    {counter}  {-amount} USD")
            print()
'''


def write_launcher(account_dir, name, mode):
    if os.name == "nt":
        launcher = account_dir / (name + ".ps1")
        launcher.write_text(
            "$ErrorActionPreference = 'Stop'\n"
            "$helper = Join-Path $PSScriptRoot '../../../../workload-hook.py'\n"
            "if (Get-Command py -ErrorAction SilentlyContinue) {\n"
            f"    & py -3 $helper {mode} @args\n"
            "} else {\n"
            f"    & python $helper {mode} @args\n"
            "}\nexit $LASTEXITCODE\n", encoding="utf-8")
    else:
        launcher = account_dir / name
        launcher.write_text(
            '#!/bin/sh\n'
            f'exec python3 "$(dirname "$0")/../../../../workload-hook.py" {mode} "$@"\n',
            encoding="utf-8")
        launcher.chmod(0o755)


def write_conversion(base, account_dir, kind):
    if kind in ("preprocess", "construct", "combined"):
        helper = base / "workload-hook.py"
        if not helper.exists():
            categories = {name: cat for cat, names in MERCHANTS.items() for name in names}
            helper.write_text(f"CATEGORIES = {categories!r}\n" + HOOK_BODY, encoding="utf-8")
        if kind in ("preprocess", "combined"):
            write_launcher(account_dir, "preprocess", "preprocess")
        if kind in ("construct", "combined"):
            write_launcher(account_dir, "construct", kind)
            return

    last_field = "counteraccount" if kind == "preprocess" else "category"
    rules = ("skip 1\n"
             f"fields date, code, description, amount, {last_field}\n"
             "currency USD\n"
             f"account1 assets:examplebank:{account_dir.name}\n")
    if kind == "advanced":
        shared = account_dir.parent.parent / "merchants.rules"
        if not shared.exists():
            blocks = [f"if %description ^({'|'.join(names)})$\n  account2 expenses:{category}\n"
                      for category, names in MERCHANTS.items()]
            shared.write_text("\n".join(blocks), encoding="utf-8")
        rules += "account2 expenses:unknown\ninclude ../../merchants.rules\n"
    elif kind == "preprocess":
        rules += "account2 %counteraccount\n"
    else:
        rules += "account2 expenses:%category\n"
    (account_dir / f"examplebank-{account_dir.name}.rules").write_text(rules, encoding="utf-8")


KINDS = ("plain", "advanced", "preprocess", "construct", "combined")


def generate_base(base, counts, statements, transactions, seed):
    index = 0
    for kind, count in counts.items():
        for _ in range(count):
            account = base / "import/sample/examplebank" / f"account-{index + 1:04}"
            write_statements(account, index * statements * transactions, statements, transactions, seed)
            write_conversion(base, account, kind)
            index += 1
    totals = ((index, "account"), (index * statements, "statement"),
              (index * statements * transactions, "transaction"))
    summary = ", ".join(f"{n} {label}{'s' if n != 1 else ''}" for n, label in totals)
    print(f"{base}: {summary}")


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    modes = parser.add_subparsers(dest="mode", required=True)
    separate = modes.add_parser("separate", help="create one base directory per account type")
    mixed = modes.add_parser("mixed", help="create one base directory with a mix of account types")
    for command in (separate, mixed):
        command.add_argument("destination", type=Path, help="new output directory")
        command.add_argument("--statements", type=int, default=2, help="statements per account (default 2)")
        command.add_argument("--transactions", type=int, default=10, help="rows per statement (default 10)")
        command.add_argument("--seed", type=int, default=1, help="reproducible data seed (default 1)")
    separate.add_argument("--accounts", type=int, default=1, help="accounts per base (default 1)")
    for kind in KINDS:
        mixed.add_argument("--" + kind, type=int, help=f"number of {kind} accounts")
    args = parser.parse_args(argv)
    if args.statements < 1 or args.transactions < 1:
        parser.error("--statements and --transactions must be positive")
    dest = args.destination
    if args.mode == "separate":
        if args.accounts < 1:
            parser.error("--accounts must be positive")
        layouts = [(dest / kind, {kind: args.accounts}) for kind in KINDS]
    else:
        counts = {kind: getattr(args, kind) for kind in KINDS}
        default = int(all(n is None for n in counts.values()))
        counts = {kind: default if n is None else n for kind, n in counts.items()}
        if any(n < 0 for n in counts.values()) or sum(counts.values()) == 0:
            parser.error("specify nonnegative account counts with at least one account")
        layouts = [(dest, counts)]

    try:
        dest.mkdir(parents=True)
    except OSError as error:
        print(f"Could not create {dest}: {error}", file=sys.stderr)
        return 1
    try:
        for base, counts in layouts:
            generate_base(base, counts, args.statements, args.transactions, args.seed)
    except OSError as error:
        print(f"Partial workload at {dest}: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
