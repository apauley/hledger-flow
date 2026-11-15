import csv
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
                    run(sys.executable, helper, "preprocess", raw, normalized, *hook_args)
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
                    journal = run(sys.executable, helper, kind, source, "-", *hook_args)
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

if __name__ == "__main__":
    unittest.main()
