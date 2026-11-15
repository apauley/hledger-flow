"""Acceptance tests for incremental imports, run with the hledger on PATH."""
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import textwrap
import time
import unittest


CHECKED = "--incremental=checked"
TRUSTED = "--incremental=trust-scripts"
NOTICE = "Incremental reuse unavailable"
LINK = "https://hledger-flow.github.io/go/incremental-import-fallback/"
FIELDS = "fields date, description, amount\naccount1 assets:bank:checking\n"
RULES = FIELDS + "include nested/categories.rules\n"
ROW = "2025-03-01,Coffee,-12.34\n"
CONSTRUCT = (
    "2025-03-01 Coffee\n"
    "    assets:bank:checking                        -12.34 EUR\n"
    "    expenses:coffee                              12.34 EUR\n"
)


def write(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(value.encode("utf-8"))


def old_timestamp(path):
    os.utime(path, (946684800, 946684800))


class Ledger(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        version = subprocess.check_output(["hledger", "--no-conf", "--version"], text=True)
        release = re.search(r"^hledger (1\.52\.4|1\.99\.6)(?:[-,\s]|$)", version)
        if not release:
            raise RuntimeError(f"Incremental CLI tests require hledger 1.52.4 or 1.99.6, found {version.strip()}")
        cls.release = release[1]

    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="flow ")
        self.addCleanup(tmp.cleanup)
        self.tmp = Path(tmp.name).resolve()
        self.base = self.tmp / "ledger"
        self.home = self.tmp / "home"
        self.home.mkdir()
        self.env = dict(os.environ, HOME=str(self.home), USERPROFILE=str(self.home),
                        XDG_CONFIG_HOME=str(self.tmp / "xdg"), NO_COLOR="1",
                        PSExecutionPolicyPreference="RemoteSigned")
        self.env.pop("LEDGER_FILE", None)
        self.account = self.base / "import/alice/bank/checking"
        self.csv = self.account / "1-in/2025/march.csv"
        self.journal = self.account / "3-journal/2025/march.journal"
        self.saved = self.account / "2-preprocessed/2025/march.csv"
        self.rules = self.base / "import/bank.rules"
        self.category = self.base / "import/nested/categories.rules"
        self.cache = self.base / ".hledger-flow/cache"
        self.snapshots = self.cache / "snapshots"
        write(self.base / "hledger.conf", "")
        write(self.csv, ROW)
        write(self.rules, RULES)
        write(self.category, "account2 expenses:coffee\n")

    def command(self, *args, cwd=None, **kwargs):
        return subprocess.run(args, cwd=cwd or self.base, env=self.env,
                              capture_output=True, text=True, encoding="utf-8", **kwargs)

    def flow(self, *flags, cwd=None, verbose=True, ok=True, options=()):
        result = self.command("hledger-flow", *(["--verbose"] if verbose else []),
                              *options, "import", *flags, cwd=cwd)
        if ok:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def imports(self, count, *flags, fallback=False, **kwargs):
        result = self.flow(*flags, **kwargs)
        output = result.stdout + result.stderr
        self.assertEqual(output.count("hledger finished in "),
                         count, output)
        self.assertEqual(result.stderr.count(NOTICE), int(fallback), output)
        if fallback:
            self.assertIn(LINK, result.stderr)
        return result

    def journals(self):
        return {p.relative_to(self.base): p.read_bytes() for p in self.base.rglob("*.journal")
                if ".hledger-flow" not in p.parts and ".trash" not in p.parts}

    def full_parity(self, *flags, ok=True):
        incremental = self.flow(*flags, ok=ok)
        journals = self.journals()
        full = self.flow(ok=ok)
        self.assertEqual(incremental.returncode, full.returncode)
        self.assertEqual(self.journals(), journals)
        return incremental, full

    def hook(self, name, body):
        """Both native hook forms run the same Python fixture."""
        helper = self.tmp / (name + ".py")
        write(helper, "from pathlib import Path\nimport os, sys, time\n"
              "source = Path(sys.argv[1])\n" + textwrap.dedent(body))
        hook = self.account / name
        if os.name == "nt":
            hook = hook.with_suffix(".ps1")
            def literal(arg):
                return "'" + str(arg).replace("'", "''") + "'"
            write(hook, f"& {literal(sys.executable)} {literal(helper)} @args\nexit $LASTEXITCODE\n")
        else:
            write(hook, f"#!/bin/sh\nexec {shlex.quote(sys.executable)} {shlex.quote(str(helper))} \"$@\"\n")
            hook.chmod(0o755)
        return hook

    def records(self):
        return sorted(p for p in (self.cache / "import").rglob("*") if p.is_file())

    def assert_warm(self, mode=CHECKED):
        records = self.records()
        self.assertTrue(records)
        for record in records:
            old_timestamp(record)
        # A clean warm run never creates even the snapshot registration directory.
        if self.snapshots.exists():
            shutil.rmtree(self.snapshots)
        self.imports(0, mode)
        self.assertFalse(self.snapshots.exists())
        for record in records:
            self.assertEqual(record.stat().st_mtime, 946684800)


class Modes(Ledger):
    def test_flags_and_environment(self):
        for flags in ([CHECKED, TRUSTED], [TRUSTED, CHECKED], ["--incremental"],
                      ["--incremental=quick"], ["--new-files-only", CHECKED],
                      [TRUSTED, "--new-files-only"]):
            self.flow(*flags, ok=False)
        for flags, display in (([], "regenerate all"), (["--new-files-only"], "only new or changed"),
                               ([CHECKED], "incremental (checked)"), ([TRUSTED], "incremental (trust-scripts)")):
            result = self.flow(*flags, options=["--show-options"])
            self.assertRegex(result.stdout + result.stderr, r"Files[^\n]*" + re.escape(display))
            if flags in ([], ["--new-files-only"]):
                self.assertFalse(self.cache.exists())


class Rules(Ledger):
    def test_conversion_counts_and_selected_scope(self):
        prior = self.account / "1-in/2024/january.csv"
        second = self.account.with_name("savings")
        write(prior, ROW)
        write(second / "1-in/2025/january.csv", ROW)
        self.imports(3, CHECKED)
        self.assert_warm()
        for source in (self.csv, self.category):
            old_timestamp(source)
        self.imports(0, CHECKED)
        write(self.csv, ROW.replace("Coffee", "Coffee corrected"))
        old_timestamp(self.csv)
        self.imports(1, CHECKED)
        write(self.journal, "changed on disk\n")
        self.imports(1, CHECKED)
        self.journal.unlink()
        self.imports(1, CHECKED)
        additional = self.csv.with_name("april.csv")
        write(additional, ROW)
        self.imports(1, CHECKED)

        write(self.category, "account2 expenses:food\n")
        self.imports(2, CHECKED, "--start-year=2025", cwd=self.account)
        self.assertIn(b"expenses:food", self.journal.read_bytes())
        self.imports(2, CHECKED)
        override = self.account / "bank-checking.rules"
        write(override, FIELDS + "account2 expenses:travel\n")
        old_timestamp(override)
        self.imports(3, CHECKED)
        override.unlink()
        self.imports(3, CHECKED)

        leaf = self.category.with_name("leaf.rules")
        write(leaf, "")
        write(self.category, "include leaf.rules\n")
        self.imports(4, CHECKED)
        write(leaf, "account2 expenses:lunch\n")
        self.imports(4, CHECKED)
        write(self.category.with_name("other.rules"), "account2 expenses:other\n")
        write(self.category, "include other.rules\n")
        self.imports(4, CHECKED)
        self.full_parity(CHECKED)

        additional.unlink()
        self.imports(0, CHECKED)
        self.assertFalse(self.journal.with_name("april.journal").exists())
        write(additional, ROW)
        self.imports(1, CHECKED)
        self.assert_warm()

    def test_directives_and_fallback_scope(self):
        directives = self.base / "directives.journal"
        write(directives, "")
        self.imports(1, CHECKED)
        for line in ("account assets:bank:checking  ; type: C", "commodity 1,000.00000000 BTC  ; lots:",
                     "commodity €1,000.00", "P 2000-01-01 EUR €1"):
            write(directives, directives.read_text(encoding="utf-8") + line + "\n")
            self.imports(1, CHECKED)
            self.imports(0, CHECKED)
        write(self.base / "prices-extra.journal", "")
        write(directives, "include prices-*.journal\n")
        for mode in (CHECKED, TRUSTED):
            self.imports(1, mode, fallback=True)
            result = self.flow(mode, verbose=False)
            self.assertIn("all selected statements", result.stderr)
            self.assertEqual(result.stderr.count(LINK), 1)
        directives.unlink()
        safe = self.account.with_name("savings")
        write(safe / "1-in/2025/january.csv", ROW)
        write(safe / "bank-savings.rules", FIELDS + "account2 expenses:test\n")
        outside = self.tmp / "external.rules"
        write(outside, "account2 expenses:external\n")
        self.imports(2, CHECKED)
        for include in (outside.as_posix(), "../../external.rules"):
            write(self.rules, FIELDS + "include " + include + "\n")
            result = self.imports(1, CHECKED, fallback=True)
            self.assertIn("1 of 2 selected statements", result.stderr)
            self.full_parity(CHECKED)


    def test_include_whitespace_and_missing_file(self):
        write(self.rules, FIELDS + "include nested/categories.rules   \n")
        # Windows may normalize trailing spaces. Follow the native full import.
        full = self.command("hledger-flow", "import")
        self.full_parity(CHECKED, ok=full.returncode == 0)
        if self.release == "1.99.6":
            self.assertEqual(full.returncode, 0, full.stderr)
            self.assert_warm()
        write(self.rules, FIELDS + "include missing.rules\n")
        incremental, full = self.full_parity(CHECKED, ok=False)
        for result in (incremental, full):
            self.assertIn("missing.rules", result.stderr)

    def test_nested_crlf_and_definite_include_cycle(self):
        leaf = self.category.with_name("leaf.rules")
        write(leaf, "account2 expenses:normal\n")
        if os.name != "nt":
            # On POSIX a trailing CR is legal in the filename. The 1.52.4
            # native text reader and the newer reader can select different files.
            write(leaf.with_name("leaf.rules\r"), "account2 expenses:return\n")
        self.category.write_bytes(b"include leaf.rules\r\n")
        self.full_parity(CHECKED)
        if self.release == "1.52.4":
            self.imports(1, CHECKED, fallback=True)
            self.assertEqual(self.records(), [])
        else:
            self.assertIn("expenses:normal", self.journal.read_text())
            self.assert_warm()

        write(self.category, "include leaf.rules\n")
        write(leaf, "include categories.rules\n")
        for mode in (CHECKED, TRUSTED):
            failure = self.command("hledger-flow", "import", mode, timeout=10)
            self.assertNotEqual(failure.returncode, 0)
            self.assertRegex(failure.stderr.lower(), "cycl(?:e|ic)")
            self.assertIn("categories.rules", failure.stderr)
        write(leaf, "account2 expenses:repaired\n")
        self.imports(1, CHECKED)
        self.assert_warm()

    def test_fallback_aggregates_different_inputs_once(self):
        self.csv.unlink()
        write(self.csv.with_suffix(".journal"), CONSTRUCT)
        second = self.account.with_name("savings")
        write(second / "1-in/2025/may.csv", ROW)
        outside = self.tmp / "external.rules"
        write(outside, "account2 expenses:external\n")
        write(second / "bank-savings.rules", FIELDS + "include " + outside.as_posix() + "\n")
        for mode in (CHECKED, TRUSTED):
            result = self.flow(mode, verbose=False)
            self.assertEqual(result.stderr.count(LINK), 1, result.stderr)
            self.assertIn("all selected statements", result.stderr)
            self.assertIn("reader", result.stderr)
            self.assertIn("include", result.stderr)
            self.assertFalse(self.snapshots.exists())
            self.assertEqual(self.records(), [])
            self.full_parity(mode)


class Cache(Ledger):
    def test_failures_retries_and_git_ignore(self):
        self.command("git", "init", "-q", check=True)
        self.imports(1, CHECKED)
        record, = self.records()
        ignore = self.cache / ".gitignore"
        self.assertEqual(ignore.read_bytes(), b"*\n")
        for file in (record, ignore):
            self.command("git", "check-ignore", "-q", str(file), check=True)
        sibling = self.cache.parent / "keep"
        write(sibling, "")
        self.assertIn("keep", self.command("git", "status", "--porcelain", "-uall").stdout)
        write(ignore, "custom rule\n")
        write(record, "partial")
        self.imports(1, TRUSTED)
        self.assertEqual(ignore.read_bytes(), b"custom rule\n")
        before = record.read_bytes(), self.journal.read_bytes()
        write(self.csv, "invalid date,New statement,-7\n")
        self.flow(CHECKED, ok=False)
        self.assertEqual((record.read_bytes(), self.journal.read_bytes()), before)
        write(self.csv, ROW.replace("-12.34", "-15.67"))
        self.imports(1, CHECKED)
        record.unlink()
        record.mkdir()  # Force a record publication failure even as root.
        result = self.imports(1, CHECKED)
        self.assertIn("record", result.stderr.lower())
        self.assertIn("15.67", self.journal.read_text())
        record.rmdir()
        self.imports(1, CHECKED)
        self.assert_warm()


class Config(Ledger):
    def config_parity(self, mode):
        (self.base / "hledger.conf").unlink()
        absence, _ = self.full_parity(mode)
        self.assertNotIn(NOTICE, absence.stderr)
        default = self.journal.read_bytes()
        self.assert_warm(mode)
        # On Windows getHomeDirectory uses the native user profile, independent
        # of HOME. Exercise the per-user XDG location without altering it.
        home = (Path(self.env["XDG_CONFIG_HOME"]) / "hledger/hledger.conf"
                if os.name == "nt" else self.home / ".hledger.conf")
        inactive = "[balance]\n--period=thismonth\n"
        layout = "[print]\n--layout=hledger1\n"
        write(home, inactive)
        self.imports(1, mode)
        self.full_parity(mode)
        self.assert_warm(mode)
        modern = self.release == "1.99.6"
        write(home, inactive + layout)
        incremental, full = self.full_parity(mode, ok=modern)
        if modern:
            self.assertNotIn(NOTICE, incremental.stderr)
            self.assertNotEqual(self.journal.read_bytes(), default)
            self.assert_warm(mode)
        else:
            for result in (incremental, full):
                self.assertIn("--layout", result.stderr)
        nearer = self.base / "hledger.conf"
        write(nearer, inactive)
        self.imports(1, mode)
        self.assertEqual(self.journal.read_bytes(), default)
        self.full_parity(mode)
        self.assert_warm(mode)
        nearer.unlink()
        self.full_parity(mode, ok=modern)

    def test_rules_config_discovery(self):
        self.config_parity(CHECKED)

    def test_construct_config_discovery(self):
        self.hook("construct", f"print({CONSTRUCT!r})")
        self.config_parity(TRUSTED)

    def test_rebuilds_and_fallbacks(self):
        config = self.base / "hledger.conf"
        for mode in (CHECKED, TRUSTED):
            write(config, "[balance]\n--period=thismonth\n")
            self.imports(1, mode)
            self.imports(0, mode)
            write(config, "[balance]\n--period=thisyear\n")
            self.imports(1, mode)
            self.imports(0, mode)
            default = self.journal.read_bytes()
            for lines, number in (("[balance]\n--period=thisyear\n[print]\n--period=thismonth\n", 4),
                                  ("--period=thismonth\n", 1), ("[general]\n--period=thismonth\n", 2),
                                  ("[txns]\n--invert\n", 1)):
                write(config, lines)
                for _ in range(2):
                    result = self.imports(1, mode, fallback=True)
                    self.assertIn(f"line {number}", result.stdout + result.stderr)
                    self.assertIn(str(config), result.stdout + result.stderr)
            self.assertEqual(self.journal.read_bytes(), default)
            write(config, "[print]\n--invert\n")
            self.imports(1, mode, fallback=True)
            self.assertNotEqual(self.journal.read_bytes(), default)
            self.full_parity(mode)
            write(config, "[print]\n--invalid-incremental-test-option\n")
            self.full_parity(mode, ok=False)
        if self.release == "1.99.6":
            write(config, "[alias]\nmyaccounts = accounts\n")
            self.imports(1, CHECKED, fallback=True)
            self.full_parity(CHECKED)


class Diagnostics(Ledger):
    def test_nested_rules_and_pasteable_error(self):
        leaf = self.category.parent / "bad categories.rules"
        write(self.category, "include " + leaf.name + "\n")
        write(leaf, "# one comment\nbad banana\n")
        ordinary = self.command("hledger", "print", "--explicit", "--file", str(self.csv),
                                "--rules-file", str(self.rules))
        self.assertNotEqual(ordinary.returncode, 0)
        location = re.search(r"Error: (.*?\.rules:\d+:\d+:)", ordinary.stderr)
        self.assertIsNotNone(location, ordinary.stderr)
        # Hledger can print a Windows short-name ancestor or a canonical path.
        # Match its reported absolute path rather than constructing one.
        expected = location[1]
        for verbose in (False, True):
            result = self.flow(CHECKED, verbose=verbose, ok=False)
            self.assertEqual(result.returncode, ordinary.returncode)
            output = result.stdout + result.stderr
            self.assertIn(expected, output)
            self.assertNotIn("snapshots" + os.sep, output)
            self.assertNotIn(NOTICE, output)
            self.assertEqual(list(self.snapshots.glob("r????????")), [])
            pattern = r"^\s*\$ (.*--rules-file[^\n]*)$" if verbose else r"Command:\s*\n\s*(.*--rules-file[^\n]*)"
            shown = re.search(pattern, output, re.MULTILINE)
            self.assertIsNotNone(shown, output)
            command = shown[1]
            if os.name == "nt":
                replay = self.command("powershell.exe", "-NoProfile", "-NonInteractive",
                                      "-Command", command + "; exit $LASTEXITCODE")
            else:
                replay = self.command("sh", "-c", command)
            self.assertEqual(replay.returncode, ordinary.returncode, replay.stderr)
            self.assertIn(Path(expected).name, replay.stderr)
            self.assertIn("bad banana", replay.stderr)
        marker = ".hledger-flow/cache/snapshots/r00000001/t00000002/import/nested/x.rules"
        write(self.csv, ROW.replace("Coffee", marker))
        write(leaf, "account2 expenses:food\n")
        self.imports(1, CHECKED)
        self.assertIn(marker, self.journal.read_text())
        self.full_parity(CHECKED)

    def test_existing_output_permissions(self):
        self.imports(1)
        self.journal.chmod(0o444)
        self.addCleanup(self.journal.chmod, 0o644)
        write(self.csv, ROW.replace("-12.34", "-7"))
        full = self.command("hledger-flow", "import")
        expected = self.journal.read_bytes()
        inc = self.command("hledger-flow", "import", CHECKED)
        self.assertEqual(inc.returncode, full.returncode, inc.stderr)
        self.assertEqual(self.journal.read_bytes(), expected)
        if os.name != "nt":
            self.assertEqual(self.journal.stat().st_mode & 0o777, 0o444)


class Hooks(Ledger):
    def test_preprocess_and_mode_switches(self):
        pre = self.hook("preprocess", """
            with Path("pre-runs").open("a") as counter:
                counter.write("run\\n")
            Path(sys.argv[2]).write_bytes(source.read_bytes().split(b"\\n", 1)[1])
        """)
        def count():
            return len((self.base / "pre-runs").read_text().splitlines())
        write(self.csv, "download one\n" + ROW)
        self.imports(1, CHECKED)
        self.imports(0, TRUSTED)
        self.assertEqual(count(), 1)
        for file in (self.saved, self.journal):
            old_timestamp(file)
        write(self.csv, "download two\n" + ROW)
        self.imports(0, CHECKED)
        self.assertEqual(count(), 2)
        for file in (self.saved, self.journal):
            self.assertEqual(file.stat().st_mtime, 946684800)
        self.imports(1, "--new-files-only")
        self.imports(0, "--new-files-only")
        self.assertEqual(count(), 3)
        write(self.category, "account2 expenses:travel\n")
        self.imports(1, TRUSTED)
        self.assertEqual(count(), 3)
        write(self.saved, "altered\n")
        self.imports(0, TRUSTED)
        self.saved.unlink()
        self.imports(0, TRUSTED)
        self.assertEqual(count(), 5)
        self.imports(0, TRUSTED, cwd=self.account)
        self.assertTrue((self.account / "pre-runs").is_file())
        old_timestamp(self.csv)
        self.imports(0, TRUSTED, cwd=self.account)
        self.assertEqual((self.account / "pre-runs").read_text().splitlines(), ["run"])
        pre.unlink()
        self.imports(1, TRUSTED)
        write(self.csv, ROW)
        old_timestamp(self.saved)
        failure = self.flow(TRUSTED, ok=False)
        self.assertIn("Restore the preprocess hook", failure.stderr)
        self.saved.unlink()
        self.imports(1, TRUSTED)

    def test_construct_hidden_include_and_cwd(self):
        extra = self.base / "extra.journal"
        write(extra, CONSTRUCT)
        self.hook("construct", """
            with Path("construct-runs").open("a") as counter:
                counter.write("run\\n")
            print("include extra.journal")
        """)
        self.imports(1, CHECKED)
        self.imports(0, TRUSTED)
        before = self.journal.read_bytes()
        write(extra, CONSTRUCT.replace("Coffee", "Lunch"))
        self.imports(0, TRUSTED)
        self.assertEqual(self.journal.read_bytes(), before)
        self.imports(1, CHECKED)
        self.assertIn("Lunch", self.journal.read_text())
        self.imports(0, TRUSTED)
        write(self.account / "extra.journal", CONSTRUCT.replace("Coffee", "Account directory"))
        self.imports(1, TRUSTED, cwd=self.account)
        self.assertIn("Account directory", self.journal.read_text())
        self.imports(0, TRUSTED, cwd=self.account)
        self.assertEqual((self.account / "construct-runs").read_text().splitlines(), ["run"])
        self.full_parity(TRUSTED)


    @unittest.skipIf(os.name == "nt", "POSIX executable bit")
    def test_execute_permission_required_on_every_trusted_hit(self):
        for name, body in (("preprocess", "Path(sys.argv[2]).write_bytes(source.read_bytes())"),
                           ("construct", f"print({CONSTRUCT!r})")):
            hook = self.hook(name, body)
            self.imports(1, TRUSTED)
            self.imports(0, TRUSTED)
            before = self.journal.read_bytes()
            hook.chmod(0o644)
            self.full_parity(TRUSTED, ok=False)
            self.assertEqual(self.journal.read_bytes(), before)
            hook.chmod(0o755)
            self.imports(0, TRUSTED)

    def test_hook_edits_rule_and_changes_known_input(self):
        self.hook("preprocess", """
            Path(sys.argv[2]).write_bytes(source.read_bytes())
            leaf = Path("import/nested/categories.rules")
            leaf.write_text("account2 expenses:" + Path("category").read_text() + "\\n")
            if Path("touch-source").exists():
                Path("touch-source").unlink()
                os.utime(source, (946684800, 946684800))
        """)
        write(self.base / "category", "lunch")
        self.imports(1, CHECKED)
        write(self.base / "category", "travel")
        self.imports(1, CHECKED)
        self.assertIn("expenses:travel", self.journal.read_text())
        write(self.base / "touch-source", "")
        write(self.csv, ROW.replace("Coffee", "Tea"))
        self.imports(1, TRUSTED, fallback=True)
        result = self.imports(0, TRUSTED)
        self.assertIn("to preprocess ", result.stderr)
        result = self.imports(0, TRUSTED)
        self.assertNotIn("to preprocess ", result.stderr)

        self.hook("construct", """
            if Path("touch-construct-input").exists():
                source.write_bytes(source.read_bytes())
                os.utime(source, (946684800, 946684800))
                Path("touch-construct-input").unlink()
            print("2025-03-01 Coffee\\n    expenses:lunch  4\\n    assets:cash")
        """)
        write(self.base / "touch-construct-input", "")
        self.imports(1, TRUSTED, fallback=True)
        self.imports(1, TRUSTED)
        self.imports(0, TRUSTED)


@unittest.skipUnless(os.name == "nt", "native Windows snapshot path limit")
class WindowsPaths(Ledger):
    def test_long_snapshot_runs_hook_once_and_matches_full_import(self):
        self.csv.unlink()
        self.hook("preprocess", """
            with Path("hook-runs").open("a") as count:
                count.write("x")
            Path(sys.argv[2]).write_bytes(source.read_bytes())
        """)
        for units in (259, 260):
            # The effective source is the preprocessed CSV, mirrored under l/.
            parent = self.snapshots / "r00000000/t00000000/l/import/alice/bank/checking/2-preprocessed/2025"
            suffix = "😀.csv"
            pad = units - len(str(parent / suffix).encode("utf-16-le")) // 2
            self.assertGreaterEqual(pad, 0)
            source = self.csv.with_name("x" * pad + suffix)
            write(source, ROW)
            result = self.imports(1, CHECKED, fallback=units == 260)
            self.assertEqual((self.base / "hook-runs").read_text(), "x")
            if units == 259:
                self.imports(0, TRUSTED)
            else:
                self.assertIn("overlong Windows snapshot path", result.stderr)
                self.imports(1, TRUSTED, fallback=True)
            self.assertEqual((self.base / "hook-runs").read_text(), "x")
            self.full_parity(CHECKED)
            source.unlink()
            (self.base / "hook-runs").unlink()


class Concurrent(Ledger):
    def test_abandoned_run_and_overlapping_writers(self):
        # Real construct hooks keep each CLI alive inside its private snapshot.
        # Marker files synchronize the processes at this exact point.
        write(self.base / "hledger.conf", "# private config for this ledger\n")
        self.hook("construct", f"""
            gate = os.environ.get("STATEMENT_TEST_GATE")
            if gate:
                Path(gate + ".ready").touch()
                deadline = time.monotonic() + 45
                while not Path(gate + ".release").exists():
                    if time.monotonic() > deadline:
                        raise RuntimeError("test did not release construct")
                    time.sleep(.02)
            print({CONSTRUCT!r})
        """)

        def start(gate):
            marker = str(self.tmp / gate)
            process = subprocess.Popen(["hledger-flow", "import", TRUSTED],
                                       cwd=self.base, env=dict(self.env, STATEMENT_TEST_GATE=marker),
                                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            def finish():
                Path(marker + ".release").touch()
                if process.poll() is None:
                    process.kill()
                process.communicate(timeout=20)
            self.addCleanup(finish)
            deadline = time.monotonic() + 30
            while not Path(marker + ".ready").exists():
                if process.poll() is not None:
                    self.fail(process.communicate()[0].decode(errors="replace"))
                if time.monotonic() > deadline:
                    self.fail("construct hook never reached the snapshot")
                time.sleep(.02)
            return process

        abandoned = start("first")
        first, = self.snapshots.glob("r????????")
        live = start("second")
        live_dir, = set(self.snapshots.glob("r????????")) - {first}
        self.assertTrue(list(first.rglob("hledger.conf")))
        abandoned.kill()
        abandoned.wait(timeout=10)
        self.assertEqual(self.records(), [])
        # Its still-running hook and converter must not retain its lifetime lock.
        self.imports(1, TRUSTED)
        self.assertFalse(first.exists())
        (self.tmp / "first.release").touch()
        abandoned.communicate(timeout=20)
        self.assertTrue(live_dir.is_dir())
        expected = self.journal.read_bytes()
        (self.tmp / "second.release").touch()
        output, _ = live.communicate(timeout=30)
        self.assertEqual(live.returncode, 0, output)
        self.assertFalse(live_dir.exists())
        self.assertEqual(self.journal.read_bytes(), expected)
        self.assert_warm(TRUSTED)
        self.full_parity(TRUSTED)


if __name__ == "__main__":
    unittest.main()
