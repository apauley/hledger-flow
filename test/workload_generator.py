import csv
from decimal import Decimal
import io
import os
from pathlib import Path
import runpy
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "generate-workload.py"
KINDS = ("plain", "advanced", "preprocess", "construct", "combined")


class Workspace(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="workload test ")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        environment = patch.dict(os.environ, XDG_CONFIG_HOME=str(self.root / "config"))
        environment.start()
        self.addCleanup(environment.stop)
        os.environ.pop("LEDGER_FILE", None)
        if os.name == "nt":
            os.environ["PSExecutionPolicyPreference"] = "RemoteSigned"


class RawStatementTests(Workspace):
    def test_repartition_and_reproduce(self):
        write = runpy.run_path(str(SCRIPT))["write_statements"]

        def generate(name, statements, transactions, seed):
            accounts = []
            for index in range(2):
                account = self.root / name / f"account-{index + 1:04}"
                write(account, index * 12, statements, transactions, seed)
                files = sorted(account.rglob("*.csv"))
                self.assertEqual(files, [account / "1-in" / "2024" / f"statement-{n+1:04}.csv"
                                         for n in range(statements)])
                accounts.append([file.read_bytes() for file in files])
            return accounts

        original = generate("original", 2, 6, 1)
        self.assertEqual(original, generate("copy", 2, 6, 1))
        self.assertNotEqual(original, generate("different", 2, 6, 2))
        self.assertTrue(original[0][0].startswith(
            b"date,reference,description,amount,category\n"
            b"2024-01-02,T000001,North Market,-1.01,groceries\n"))

        def read_accounts(accounts):
            return [[row for file in files for row in csv.DictReader(
                io.StringIO(file.decode("utf-8")))] for files in accounts]

        rows = read_accounts(original)
        self.assertEqual(rows, read_accounts(generate("split", 3, 4, 1)))
        self.assertEqual(24, len({row["reference"] for account in rows for row in account}))
        for files in original:
            for file in files:
                self.assertNotIn(b"\r", file)
        for account in rows:
            for row in account:
                self.assertRegex(row["amount"], r"^-?\d+\.\d{2}$")
                self.assertTrue(row["date"].startswith("2024-"))


def run(*args, **kwargs):
    result = subprocess.run(args, capture_output=True, text=True, encoding="utf-8", **kwargs)
    if result.returncode:
        raise AssertionError(f"{args} exited {result.returncode}\n{result.stdout}{result.stderr}")
    return result.stdout


class ConversionTests(Workspace):
    def test_category_sources_and_hook_files(self):
        write = runpy.run_path(str(SCRIPT))["write_conversion"]
        bank_dir = self.root / "import/sample/examplebank"
        raw = self.root / "input.csv"
        raw.write_text("date,reference,description,amount,category\n"
                       "2024-01-03,T1,City Rail,-1.23,groceries\n"
                       "2024-01-04,T2,River Kiosk,0.98,transport\n", encoding="utf-8")
        normalized = self.root / "clean.csv"
        helper = self.root / "workload-hook.py"
        suffix = ".ps1" if os.name == "nt" else ""
        for kind in KINDS:
            with self.subTest(kind=kind):
                account = bank_dir / kind
                hook_args = ("examplebank", account.name, "sample")
                account.mkdir(parents=True)
                write(self.root, account, kind)
                rules = account / f"examplebank-{account.name}.rules"
                preprocess = kind in ("preprocess", "combined")
                construct = kind in ("construct", "combined")
                files = set()
                if preprocess:
                    files.add("preprocess" + suffix)
                if construct:
                    files.add("construct" + suffix)
                else:
                    files.add(rules.name)
                self.assertEqual({p.name for p in account.iterdir()}, files)
                source = raw
                if preprocess:
                    run(sys.executable, helper, "preprocess", "USD", raw, normalized, *hook_args)
                    with normalized.open(encoding="utf-8", newline="") as stream:
                        reader = csv.DictReader(stream)
                        self.assertEqual(reader.fieldnames[-1], "counteraccount")
                        self.assertEqual([r["counteraccount"] for r in reader],
                                         ["expenses:transport", "expenses:unknown"])
                    source = normalized
                if kind == "combined":
                    normalized.write_text(normalized.read_text().replace(
                        "expenses:transport", "expenses:custom"), encoding="utf-8")
                if construct:
                    journal = run(sys.executable, helper, kind, "USD", source, "-", *hook_args)
                    output = run("hledger", "--no-conf", "print", "-f", "-", "-O", "csv", input=journal)
                else:
                    output = run("hledger", "--no-conf", "print", "--explicit", "-f", source,
                                 "--rules-file", rules, "-O", "csv")
                postings = list(csv.DictReader(io.StringIO(output)))
                expected = {
                    "plain": ["expenses:groceries", "expenses:transport"],
                    "combined": ["expenses:custom", "expenses:unknown"],
                }.get(kind, ["expenses:transport", "expenses:unknown"])
                self.assertEqual([r["account"] for r in postings if r["account"].startswith("expenses:")],
                                 expected)

class GeneratorCliTests(Workspace):
    def setUp(self):
        super().setUp()
        self.script = self.root / "download.py"
        shutil.copyfile(SCRIPT, self.script)

    def generate(self, mode, name, *args):
        base = self.root / name
        return base, run(sys.executable, self.script, mode, base, *args)

    def test_layout_counts_and_source_bytes(self):
        def inputs(base):
            self.assertFalse(any(base.rglob("2-preprocessed")))
            self.assertFalse(any(base.rglob("*.journal")))
            self.assertFalse(any(base.rglob("3-journal")))
            self.assertFalse((base / "reports").exists())
            return {p.relative_to(base): p.read_bytes() for p in base.rglob("*.csv")}

        suite, output = self.generate("separate", "suite")
        self.assertEqual({p.name for p in suite.iterdir()}, set(KINDS))
        for kind in KINDS:
            self.assertIn(f"{suite / kind}: 1 account, 2 statements, 20 transactions", output)
            self.assertEqual(len(inputs(suite / kind)), 2)
        mixed, output = self.generate("mixed", "mixed")
        self.assertIn("5 accounts, 10 statements, 100 transactions", output)
        larger, _ = self.generate("separate", "five", "--accounts", "5")
        for kind in KINDS:
            self.assertEqual(inputs(mixed), inputs(larger / kind))
        copy, _ = self.generate("mixed", "copy")
        self.assertEqual(inputs(mixed), inputs(copy))
        args = ("--plain", "4", "--advanced", "2", "--preprocess", "2",
                "--construct", "1", "--combined", "1", "--statements", "12",
                "--transactions", "100", "--seed", "42")
        varied, output = self.generate("mixed", "varied", *args)
        self.assertIn("10 accounts, 120 statements, 12000 transactions", output)
        raw = inputs(varied)
        self.assertEqual(len(raw), 120)
        self.assertTrue(all(len(data.splitlines()) == 101 for data in raw.values()))
        only, output = self.generate("mixed", "only", "--construct", "2")
        self.assertIn("2 accounts, 4 statements, 40 transactions", output)
        self.assertEqual(len(list(only.rglob("construct*"))), 2)
        self.assertFalse(list(only.rglob("*.rules")))

    def test_reject_invalid_and_existing_destinations(self):
        dest = self.root / "out"

        def refuse(mode, *args):
            result = subprocess.run([sys.executable, self.script, mode, dest, *args],
                                    capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertTrue(result.stderr)

        for args in (("mixed", "--plain", "0"), ("mixed", "--construct", "-1"),
                     ("mixed", "--accounts", "2"), ("separate", "--plain", "1"),
                     ("separate", "--accounts", "0"), ("mixed", "--statements", "-1"),
                     ("separate", "--transactions", "0")):
            refuse(*args)
            self.assertFalse(dest.exists())
        dest.write_text("keep")
        refuse("mixed")
        self.assertEqual(dest.read_text(), "keep")
        dest.unlink()
        dest.mkdir()
        (dest / "sentinel").touch()
        refuse("separate")
        self.assertEqual(list(dest.iterdir()), [dest / "sentinel"])

    def test_exclusive_creation_and_partial_failure(self):
        dest = self.root / "race"
        command = [sys.executable, self.script, "mixed", dest, "--plain", "1"]
        processes = [subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                     for _ in range(2)]
        for process in processes:
            process.communicate()
        self.assertEqual(sorted(p.returncode == 0 for p in processes), [False, True])
        self.assertEqual(len(list(dest.rglob("*.csv"))), 2)
        main = runpy.run_path(str(self.script))["main"]
        partial = self.root / "partial"
        with patch.dict(main.__globals__, write_statements=unittest.mock.Mock(
                side_effect=OSError("disk full"))), patch("sys.stderr", new_callable=io.StringIO) as error:
            self.assertNotEqual(main(["mixed", str(partial)]), 0)
            self.assertIn(str(partial), error.getvalue())
            self.assertIn("disk full", error.getvalue())
        self.assertTrue(partial.is_dir())
        retry = subprocess.run([sys.executable, self.script, "mixed", partial], capture_output=True)
        self.assertNotEqual(retry.returncode, 0)

class ImportTests(Workspace):
    def check_postings(self, expected, rows):
        transactions = {}
        for row in rows:
            transactions.setdefault(row["txnidx"], []).append(row)
        codes = set()
        for postings in transactions.values():
            code = postings[0]["code"]
            self.assertNotIn(code, codes, f"duplicate transaction code {code}")
            codes.add(code)
            self.assertIn(code, expected)
            self.assertEqual({row["code"] for row in postings}, {code})
            actual = [(r["date"], r["description"], r["account"], Decimal(r["amount"]), r["commodity"])
                      for r in postings]
            self.assertCountEqual(actual, expected[code], msg=code)
        self.assertEqual(codes, set(expected))

    def test_postings_comparison_detects_corruption(self):
        expected = {"T1": [("2024-01-01", "City Rail", account, Decimal(amount), "USD")
                           for account, amount in (("assets:bank", "-1"), ("expenses:transport", "1"))]}
        good = [dict(txnidx="1", code="T1", date=day, description=memo, account=acct,
                     amount=str(amount), commodity=currency)
                for day, memo, acct, amount, currency in expected["T1"]]
        self.check_postings(expected, good)
        for corrupt in ([], good + [dict(r, txnidx="2") for r in good],
                        [dict(r, amount=str(-Decimal(r["amount"]))) for r in good]):
            with self.assertRaises(AssertionError):
                self.check_postings(expected, corrupt)

    def test_real_imports_and_report(self):
        script = self.root / "download.py"
        shutil.copyfile(SCRIPT, script)
        for mode, extra in (("mixed", ("--eur-every", "2", "--prices", "--directives")),
                            ("separate", ())):
            run(sys.executable, script, mode, self.root / mode,
                "--statements", "1", "--transactions", "6", *extra)
        script.unlink()
        config = self.root / "hledger.conf"
        config.touch()
        moved = self.root / "moved workloads"
        moved.mkdir()
        for mode in ("mixed", "separate"):
            (self.root / mode).rename(moved / mode)
        for kind in (*KINDS, "mixed"):
            with self.subTest(kind=kind):
                base = moved / "mixed" if kind == "mixed" else moved / "separate" / kind
                kinds = KINDS if kind == "mixed" else [kind]
                expected = {}
                journals = set()
                normalized = set()
                for i, account_kind in enumerate(kinds, 1):
                    name = f"account-{i:04}"
                    currency = "EUR" if kind == "mixed" and i % 2 == 0 else "USD"
                    account = base / "import/sample/examplebank" / name
                    source = account / "1-in/2024/statement-0001.csv"
                    with source.open(encoding="utf-8", newline="") as stream:
                        for row in csv.DictReader(stream):
                            amount = Decimal(row["amount"])
                            code = row["reference"]
                            self.assertNotIn(code, expected)
                            expected[code] = [
                                (row["date"], row["description"], f"assets:examplebank:{name}", amount, currency),
                                (row["date"], row["description"], "expenses:" + row["category"], -amount, currency),
                            ]
                    journals.add(account / "3-journal/2024/statement-0001.journal")
                    if account_kind in ("preprocess", "combined"):
                        normalized.add(account / "2-preprocessed/2024/statement-0001.csv")
                self.assertEqual(len(expected), 6 * len(kinds))
                run("hledger-flow", "--hledger-conf", config, "import", cwd=base)
                self.assertEqual(set(base.glob("import/*/*/*/3-journal/*/*")), journals)
                self.assertEqual(set(base.glob("import/*/*/*/2-preprocessed/*/*")), normalized)
                run("hledger", "--no-conf", "-f", "all-years.journal", "check", cwd=base)
                printed = run("hledger", "--no-conf", "-f", "all-years.journal", "print", "-O", "csv", cwd=base)
                self.check_postings(expected, csv.DictReader(io.StringIO(printed)))
                if kind == "mixed":
                    self.assertIn("include ../prices/2024/prices.journal",
                                  (base / "import/2024-include.journal").read_text())
                    self.assertIn("include directives.journal", (base / "all-years.journal").read_text())
                    for flags, commodities in (((), {"EUR", "USD"}), (("-X", "USD"), {"USD"})):
                        balances = run("hledger", "--no-conf", "-f", "import/2024-include.journal",
                                       "balance", "assets", "-O", "csv", "--layout", "tidy", *flags, cwd=base)
                        self.assertEqual({r["commodity"] for r in csv.DictReader(io.StringIO(balances))},
                                         commodities)
                    run("hledger-flow", "--hledger-conf", config, "report", cwd=base)
                    self.assertGreater((base / "reports/all/2024/income-expenses.txt").stat().st_size, 0)

if __name__ == "__main__":
    unittest.main()
