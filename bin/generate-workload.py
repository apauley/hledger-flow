#!/usr/bin/env python3
"""Generate synthetic statements for hledger-flow import benchmarks."""

import csv
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
