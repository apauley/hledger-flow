#!/usr/bin/env python3
"""Generate reproducible sample statements, rules and hooks for hledger-flow.

The same command and seed always produce the same files, so a command line is
enough to share a sample ledger, for example in a bug report.
"""

import argparse
from copy import copy
import csv
from datetime import date, timedelta
from decimal import Decimal
import os
from pathlib import Path
import re
import shlex
import sys

DOCS = "https://hledger-flow.github.io/go/sample-ledger/"
EXAMPLES_TEXT = f"""\
examples:
  python3 generate-sample-ledger.py mixed ~/tmp/hlflow/sample --all-features
  python3 generate-sample-ledger.py mixed ~/tmp/hlflow/household --plain 4 --construct 2
  python3 generate-sample-ledger.py separate ~/tmp/hlflow/comparison --accounts 10

Run "python3 generate-sample-ledger.py mixed --help" for every option.
Documentation: {DOCS}
"""

FIRST_YEAR = 2024
# Counter accounts and the merchant descriptions that rules and hooks map to them.
MERCHANTS = {
    "expenses:groceries": ("North Market", "Corner Foods", "North Market refund"),
    "expenses:transport": ("City Rail", "Town Bus"),
    "income:salary": ("Acme Payroll",),
}
# Statement rows cycle through the expenses. The raw bank category is the last name part.
EXAMPLES = [(name, account.split(":")[1]) for account, names in MERCHANTS.items()
            if account.startswith("expenses:") for name in names]
EXAMPLES.append(("River Kiosk", "unknown"))
RAW_FIELDS = ("date", "reference", "description", "amount", "category")
# Symbol, rough value in USD and display decimals. directives.journal declares all of them.
COMMODITIES = {
    "USD": ("$", "1", 2),
    "EUR": ("€", "1.10", 2),
    "GBP": ("£", "1.27", 2),
    "CAD": ("C$", "0.73", 2),
    "BTC": ("₿", "60000", 8),
}


def commodity_info(name):
    return COMMODITIES.get(name, ("", "1", 2))


def money(cents, currency):
    """Convert a USD-sized amount in cents to the currency's own scale."""
    _, value, places = commodity_info(currency)
    return (Decimal(cents) / 100 / Decimal(value)).quantize(Decimal(1).scaleb(-places))


def year_dates(year):
    first = date(year, 1, 1)
    return [(first + timedelta(days=i)).isoformat() for i in range((date(year + 1, 1, 1) - first).days)]


def no_extras(year, number, dates):
    return []


def write_statements(account_dir, first_ordinal, statements, transactions, seed,
                     years=1, currency="USD", extras=no_extras):
    """Write the raw CSVs. Return the row count and the total paid out."""
    ordinal = first_ordinal
    count, outflow = 0, Decimal(0)
    for year in range(FIRST_YEAR, FIRST_YEAR + years):
        inbox = account_dir / "1-in" / str(year)
        inbox.mkdir(parents=True)
        dates = year_dates(year)
        for number in range(1, statements + 1):
            rows = []
            for _ in range(transactions):
                description, category = EXAMPLES[ordinal % len(EXAMPLES)]
                cents = 100 + (seed + ordinal * 7919) % 25000
                if not description.endswith(" refund"):
                    cents = -cents
                rows.append((dates[(seed + ordinal) % len(dates)], f"T{ordinal + 1:06}",
                             description, cents, category))
                ordinal += 1
            rows += extras(year, number, dates)
            with (inbox / f"statement-{number:04}.csv").open("w", encoding="utf-8", newline="") as out:
                writer = csv.writer(out, lineterminator="\n")
                writer.writerow(RAW_FIELDS)
                for day, reference, description, cents, category in rows:
                    amount = money(cents, currency)
                    outflow -= min(amount, 0)
                    writer.writerow((day, reference, description, format(amount, "f"), category))
            count += len(rows)
    return count, outflow


def statement_extras(account, transfers, options):
    """Salary and transfer rows appended to each of the account's statements."""
    def extras(year, number, dates):
        rows = []
        if options.income and not account.card:
            salt = account.number * 7919 + number * 104729 + year
            rows.append((dates[(options.seed + salt) % len(dates)], f"S{account.number:04}-{year}-{number:04}",
                         "Acme Payroll", 150000 + (options.seed + salt) % 100000, "salary"))
        for payer, payee in transfers:
            if account in (payer, payee):
                salt = payer.number * 7919 + payee.number * 31 + number * 104729 + year
                cents = 10000 + (options.seed + salt) % 40000
                out = account is payer
                other = payee if out else payer
                rows.append((dates[(options.seed + salt) % len(dates)],
                             f"X{payer.number:04}-{payee.number:04}-{year}-{number:04}-{'out' if out else 'in'}",
                             f"Transfer {'to' if out else 'from'} {other.name}",
                             -cents if out else cents, "transfer"))
        return rows
    return extras


HOOK_BODY = '''
import csv
from decimal import Decimal
import sys

mode, currency, source, destination, bank, account, owner = sys.argv[1:]
kind = "liabilities" if account.startswith("card-") else "assets"


def counter_account(description):
    if description.startswith(("Transfer to ", "Transfer from ")):
        return "assets:transfers:" + "-".join(sorted((account, description.split()[-1])))
    return ACCOUNTS.get(description, "expenses:unknown")


with open(source, encoding="utf-8", newline="") as statement:
    rows = csv.DictReader(statement)
    if mode == "preprocess":
        with open(destination, "w", encoding="utf-8", newline="") as output:
            writer = csv.writer(output, lineterminator="\\n")
            writer.writerow(("date", "reference", "description", "amount", "counteraccount"))
            for row in rows:
                counter = counter_account(row["description"])
                writer.writerow((row["date"], row["reference"], row["description"], row["amount"], counter))
    else:
        for row in rows:
            if mode == "combined":
                counter = row["counteraccount"]
            else:
                counter = counter_account(row["description"])
            amount = Decimal(row["amount"])
            print(f"{row['date']} ({row['reference']}) {row['description']}")
            print(f"    {kind}:{bank}:{account}  {amount} {currency}")
            print(f"    {counter}  {-amount} {currency}")
            print()
'''


def write_launcher(account_dir, name, mode, currency):
    if os.name == "nt":
        launcher = account_dir / (name + ".ps1")
        launcher.write_text(
            "$ErrorActionPreference = 'Stop'\n"
            "$helper = Join-Path $PSScriptRoot '../../../../sample-hook.py'\n"
            "if (Get-Command py -ErrorAction SilentlyContinue) {\n"
            f"    & py -3 $helper {mode} {currency} @args\n"
            "} else {\n"
            f"    & python $helper {mode} {currency} @args\n"
            "}\nexit $LASTEXITCODE\n", encoding="utf-8")
    else:
        launcher = account_dir / name
        launcher.write_text(
            '#!/bin/sh\n'
            f'exec python3 "$(dirname "$0")/../../../../sample-hook.py" {mode} {currency} "$@"\n',
            encoding="utf-8")
        launcher.chmod(0o755)


def transfer_account(name, other):
    return "assets:transfers:" + "-".join(sorted((name, other)))


def write_conversion(base, account_dir, kind, currency="USD", partners=()):
    if kind in ("preprocess", "construct", "combined"):
        helper = base / "sample-hook.py"
        if not helper.exists():
            accounts = {name: account for account, names in MERCHANTS.items() for name in names}
            helper.write_text(f"ACCOUNTS = {accounts!r}\n" + HOOK_BODY, encoding="utf-8")
        if kind in ("preprocess", "combined"):
            write_launcher(account_dir, "preprocess", "preprocess", currency)
        if kind in ("construct", "combined"):
            write_launcher(account_dir, "construct", kind, currency)
            return

    name = account_dir.name
    last_field = "counteraccount" if kind == "preprocess" else "category"
    account1 = ("liabilities" if name.startswith("card-") else "assets") + ":examplebank:" + name
    rules = ("skip 1\n"
             f"fields date, code, description, amount, {last_field}\n"
             f"currency {currency}\n"
             f"account1 {account1}\n")
    if kind == "preprocess":
        rules += "account2 %counteraccount\n"
    else:
        if kind == "advanced":
            shared = account_dir.parent.parent / "merchants.rules"
            if not shared.exists():
                blocks = [f"if %description ^({'|'.join(names)})$\n  account2 {account}\n"
                          for account, names in MERCHANTS.items()]
                shared.write_text("\n".join(blocks), encoding="utf-8")
            rules += "account2 expenses:unknown\ninclude ../../merchants.rules\n"
        else:
            rules += "account2 expenses:%category\n\nif %category ^salary$\n  account2 income:salary\n"
        for other in partners:
            rules += (f"\nif %description ^Transfer (to|from) {other}$\n"
                      f"  account2 {transfer_account(name, other)}\n")
    (account_dir / f"examplebank-{name}.rules").write_text(rules, encoding="utf-8")


KINDS = ("plain", "advanced", "preprocess", "construct", "combined")


def price(base, other, seed, salt):
    """The other commodity's price in the base commodity, to 6 significant digits."""
    jitter = Decimal(950 + (seed + salt * 7919 + sum(map(ord, other))) % 100) / 1000
    rate = Decimal(commodity_info(other)[1]) / Decimal(commodity_info(base)[1]) * jitter
    return rate.quantize(Decimal(1).scaleb(rate.adjusted() - 5))


def write_prices(base, options):
    for year in range(FIRST_YEAR, FIRST_YEAR + options.years):
        folder = base / "prices" / str(year)
        folder.mkdir(parents=True)
        includes = []
        for other in options.other_currencies:
            name = f"{options.base_currency}-{other}.journal"
            lines = [f"P {day} {other} {format(price(options.base_currency, other, options.seed, year * 366 + i), 'f')}"
                     f" {options.base_currency}\n" for i, day in enumerate(year_dates(year))]
            (folder / name).write_text("".join(lines), encoding="utf-8")
            includes.append(f"include {name}\n")
        (folder / "prices.journal").write_text("".join(includes), encoding="utf-8")


def write_directives(base, options):
    used = [options.base_currency, *options.other_currencies]
    lines = []
    for name in [*COMMODITIES, *(c for c in used if c not in COMMODITIES)]:
        symbol, _, places = commodity_info(name)
        line = f"commodity 1,000.{'0' * places} {name}"
        lines.append(line + (f"  ; alias: {symbol}\n" if symbol else "\n"))
    (base / "directives.journal").write_text("".join(lines), encoding="utf-8")


def write_opening(account_dir, currency, amount):
    (account_dir / f"{FIRST_YEAR}-opening.journal").write_text(
        f"{FIRST_YEAR}-01-01 Opening balance\n"
        f"    assets:examplebank:{account_dir.name}  {format(amount, 'f')} {currency}\n"
        "    equity:opening/closing balances\n", encoding="utf-8")


class Account:
    def __init__(self, number, kind, card, currency):
        self.number, self.kind, self.card, self.currency = number, kind, card, currency
        self.name = f"{'card' if card else 'account'}-{number:04}"


def plan_accounts(counts, options):
    currencies = [options.base_currency, *options.other_currencies]
    accounts = []
    for kind, count in counts.items():
        for _ in range(count):
            number = len(accounts) + 1
            card = options.card_every > 0 and number % options.card_every == 0
            accounts.append(Account(number, kind, card, currencies[(number - 1) % len(currencies)]))
    transfers = []
    if options.transfers:
        for account in accounts:
            payer = next((a for a in accounts[:account.number - 1]
                          if not a.card and a.currency == account.currency), None)
            if payer:
                transfers.append((payer, account))
    return accounts, transfers


def generate_base(base, counts, options):
    accounts, transfers = plan_accounts(counts, options)
    per_account = options.statements * options.transactions * options.years
    rows = 0
    for account in accounts:
        directory = base / "import/sample/examplebank" / account.name
        partners = [(b if a is account else a).name for a, b in transfers if account in (a, b)]
        count, outflow = write_statements(
            directory, (account.number - 1) * per_account, options.statements, options.transactions,
            options.seed, options.years, account.currency, statement_extras(account, transfers, options))
        write_conversion(base, directory, account.kind, account.currency, partners)
        # Everything that ever goes out, plus up to 50% more, keeps the balance positive.
        if options.opening_balances and not account.card:
            extra = (options.seed + account.number * 7919) % 51
            write_opening(directory, account.currency,
                          (outflow * (100 + extra) / 100).quantize(Decimal(1).scaleb(-commodity_info(account.currency)[2])))
        rows += count
    if options.prices:
        write_prices(base, options)
    if options.directives:
        write_directives(base, options)
    totals = ((len(accounts), "account"), (len(accounts) * options.statements * options.years, "statement"),
              (rows, "transaction"))
    summary = ", ".join(f"{n} {label}{'s' if n != 1 else ''}" for n, label in totals)
    print(f"{format_path(base)}: {summary}")


def commodity(text):
    if not re.fullmatch(r"[A-Za-z]+", text):
        raise argparse.ArgumentTypeError(f"invalid commodity {text!r}: use letters only, such as EUR")
    return text


def commodity_list(text):
    return [commodity(name) for name in text.split(",")] if text else []


FLAGS = ("opening-balances", "income", "transfers", "prices", "directives")
# The smallest base that uses every feature: each account type, three currencies,
# a credit card, two years and a few rows per statement.
ALL_FEATURES = dict(plain=1, advanced=1, preprocess=1, construct=1, combined=1,
                    years=2, statements=1, transactions=4, other_currencies=["EUR", "BTC"],
                    card_every=4, **{flag.replace("-", "_"): True for flag in FLAGS})


def format_path(path, *, for_shell=False):
    """Abbreviate the home folder and optionally quote the path for the shell."""
    text, home = str(path), os.path.expanduser("~")
    if text == home and os.path.isabs(home):
        return "~"
    if text.startswith(home + os.sep):
        relative = text[len(home) + 1:]
        return "~/" + (shlex.quote(relative) if for_shell else Path(relative).as_posix())
    return shlex.quote(text) if for_shell else text


def format_os_error(error):
    if error.filename is not None:
        error = copy(error)
        error.filename = format_path(error.filename)
    return str(error)


def full_command(args, counts):
    words = [args.mode]
    if args.mode == "separate":
        words += ["--accounts", str(args.accounts)]
    else:
        for kind in KINDS:
            words += ["--" + kind, str(counts[kind])]
    words += ["--years", str(args.years), "--statements", str(args.statements),
              "--transactions", str(args.transactions), "--base-currency", args.base_currency]
    if args.other_currencies:
        words += ["--other-currencies", ",".join(args.other_currencies)]
    words += ["--card-every", str(args.card_every)]
    words += ["--" + flag for flag in FLAGS if getattr(args, flag.replace("-", "_"))]
    words += ["--seed", str(args.seed)]
    quoted = [shlex.quote(word) for word in ["python3", Path(sys.argv[0]).name, *words]]
    quoted.insert(3, format_path(args.destination, for_shell=True))
    return " ".join(quoted)


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__, epilog=EXAMPLES_TEXT,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    modes = parser.add_subparsers(dest="mode", required=True)
    separate = modes.add_parser("separate", help="create one base directory per account type",
                                epilog=f"Documentation: {DOCS}")
    mixed = modes.add_parser("mixed", help="create one base directory with a mix of account types",
                             epilog=f"Documentation: {DOCS}")
    for command in (separate, mixed):
        command.add_argument("destination", type=Path, help="new output directory")
        command.add_argument("--years", type=int, default=1, help=f"years of statements from {FIRST_YEAR} (default 1)")
        command.add_argument("--statements", type=int, default=2, help="statements per account per year (default 2)")
        command.add_argument("--transactions", type=int, default=10,
                             help="expense rows per statement, before salary and transfers (default 10)")
        command.add_argument("--base-currency", type=commodity, default="USD", metavar="COMMODITY",
                             help="currency of the first account and of prices (default USD)")
        command.add_argument("--other-currencies", type=commodity_list, default="", metavar="LIST",
                             help="comma-separated currencies that later accounts cycle through, such as EUR,BTC")
        command.add_argument("--card-every", type=int, default=0, metavar="N",
                             help="make every Nth account a credit card liability (default 0: none)")
        command.add_argument("--opening-balances", action="store_true",
                             help=f"give each asset account a {FIRST_YEAR} opening balance that covers its spending")
        command.add_argument("--income", action="store_true",
                             help="add a salary deposit to each statement of each asset account")
        command.add_argument("--transfers", action="store_true",
                             help="transfer money in each statement from the first earlier asset account "
                                  "in the same currency, through assets:transfers")
        command.add_argument("--prices", action="store_true",
                             help="write daily prices of the other currencies in the base currency")
        command.add_argument("--directives", action="store_true",
                             help="write commodity formats and symbol aliases to directives.journal")
        command.add_argument("--seed", type=int, default=42, help="reproducible data seed (default 42)")
    separate.add_argument("--accounts", type=int, default=1, help="accounts per base (default 1)")
    for kind in KINDS:
        mixed.add_argument("--" + kind, type=int, help=f"number of {kind} accounts")
    mixed.add_argument("--all-features", action="store_true",
                       help="start from the smallest base that uses every feature; other options override it")
    if not argv:
        parser.print_help()
        return 0
    args = parser.parse_args(argv)
    if getattr(args, "all_features", False):
        mixed.set_defaults(**ALL_FEATURES)
        args = parser.parse_args(argv)
    if args.years < 1 or args.statements < 1 or args.transactions < 1:
        parser.error("--years, --statements and --transactions must be positive")
    if args.card_every < 0:
        parser.error("--card-every must not be negative")
    currencies = [args.base_currency, *args.other_currencies]
    if len(set(currencies)) != len(currencies):
        parser.error("list each currency once, and leave the base currency out of --other-currencies")
    dest = args.destination
    if args.mode == "separate":
        if args.accounts < 1:
            parser.error("--accounts must be positive")
        counts = {}
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
        print(f"Could not create {format_path(dest)}: {format_os_error(error)}", file=sys.stderr)
        return 1
    print(f"Full command: {full_command(args, counts)}")
    try:
        for base, base_counts in layouts:
            generate_base(base, base_counts, args)
    except OSError as error:
        print(f"Partial sample ledger at {format_path(dest)}: {format_os_error(error)}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
