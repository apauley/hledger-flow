#!/usr/bin/env python3
"""Generate synthetic statements for hledger-flow import benchmarks."""

import csv
import os
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
