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

SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "generate-workload.py"
KINDS = ("plain", "advanced", "preprocess", "construct", "combined")


class Workspace(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="workload test ")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)


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


if __name__ == "__main__":
    unittest.main()
