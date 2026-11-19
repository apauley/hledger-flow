"""Acceptance tests for incremental imports, run with the hledger on PATH."""
import json
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


RUN_SCRIPTS = "--incremental=run-scripts"
SKIP_UNCHANGED = "--incremental=skip-unchanged-scripts"
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
        release = re.search(r"^hledger (\d+(?:\.\d+)*)(?:[-,\s]|$)", version)
        if not release or tuple(map(int, release[1].split("."))) < (1, 52, 4):
            raise RuntimeError(f"Incremental CLI tests require hledger >= 1.52.4, found {version.strip()}")
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
                if ".hledger-flow" not in p.parts}

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

    def assert_warm(self, mode=RUN_SCRIPTS):
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
        for flags in ([RUN_SCRIPTS, SKIP_UNCHANGED], [SKIP_UNCHANGED, RUN_SCRIPTS], ["--incremental", "--incremental"],
                      ["--incremental=quick"], ["--incremental=checked"], ["--incremental=rerun-scripts"],
                      ["--incremental=trust-scripts"], ["--incremental="], ["--incremental", "--new-files-only"], ["--new-files-only", RUN_SCRIPTS],
                      [SKIP_UNCHANGED, "--new-files-only"]):
            self.flow(*flags, ok=False)
        for flags, display in (([], "regenerate all"), (["--new-files-only"], "only new or changed"),
                               (["--incremental"], "incremental (run-scripts)"),
                               ([RUN_SCRIPTS], "incremental (run-scripts)"), ([SKIP_UNCHANGED], "incremental (skip-unchanged-scripts)")):
            result = self.flow(*flags, options=["--show-options"])
            self.assertRegex(result.stdout + result.stderr, r"Files[^\n]*" + re.escape(display))
            if flags in ([], ["--new-files-only"]):
                self.assertFalse(self.cache.exists())

    def test_bare_option_preserves_directory_scope(self):
        other = self.account.with_name("skip-unchanged-scripts")
        write(other / "1-in/2025/other.csv", ROW)
        for flags in (("--incremental", str(self.account)),
                      (str(self.account), "--incremental"),
                      ("--incremental", "--start-year", "2025", str(self.account)),
                      (SKIP_UNCHANGED, str(self.account))):
            result = self.flow(*flags)
            self.assertIn("checking", result.stdout)
            self.assertFalse((other / "3-journal/2025/other.journal").exists())
        result = self.flow("--incremental", other.name, cwd=other.parent, options=["--show-options"])
        self.assertIn("incremental (run-scripts)", result.stdout + result.stderr)
        self.assertTrue((other / "3-journal/2025/other.journal").is_file())


class Rules(Ledger):
    def test_conversion_counts_and_selected_scope(self):
        prior = self.account / "1-in/2024/january.csv"
        second = self.account.with_name("savings")
        write(prior, ROW)
        write(second / "1-in/2025/january.csv", ROW)
        self.imports(3, RUN_SCRIPTS)
        self.assert_warm()
        for source in (self.csv, self.category):
            old_timestamp(source)
        self.imports(0, RUN_SCRIPTS)
        write(self.csv, ROW.replace("Coffee", "Coffee corrected"))
        old_timestamp(self.csv)
        self.imports(1, RUN_SCRIPTS)
        write(self.journal, "changed on disk\n")
        self.imports(1, RUN_SCRIPTS)
        self.journal.unlink()
        result = self.imports(1, RUN_SCRIPTS)
        self.assertIn(f"missing output: {self.journal}", result.stdout + result.stderr)
        additional = self.csv.with_name("april.csv")
        write(additional, ROW)
        self.imports(1, RUN_SCRIPTS)

        write(self.category, "account2 expenses:food\n")
        self.imports(2, RUN_SCRIPTS, "--start-year=2025", cwd=self.account)
        self.assertIn(b"expenses:food", self.journal.read_bytes())
        self.imports(2, RUN_SCRIPTS)
        override = self.account / "bank-checking.rules"
        write(override, FIELDS + "account2 expenses:travel\n")
        old_timestamp(override)
        self.imports(3, RUN_SCRIPTS)
        override.unlink()
        self.imports(3, RUN_SCRIPTS)

        leaf = self.category.with_name("leaf.rules")
        write(leaf, "")
        write(self.category, "include leaf.rules\n")
        self.imports(4, RUN_SCRIPTS)
        write(leaf, "account2 expenses:lunch\n")
        result = self.imports(4, RUN_SCRIPTS)
        self.assertIn(f"changed input: {leaf}", result.stdout + result.stderr)
        write(self.category.with_name("other.rules"), "account2 expenses:other\n")
        write(self.category, "include other.rules\n")
        self.imports(4, RUN_SCRIPTS)
        self.full_parity(RUN_SCRIPTS)

        additional.unlink()
        self.imports(0, RUN_SCRIPTS)
        self.assertFalse(self.journal.with_name("april.journal").exists())
        write(additional, ROW)
        self.imports(1, RUN_SCRIPTS)
        self.assert_warm()

    def test_literal_journal_includes(self):
        write(self.base / "directives.journal",
              "commodity €1,000.00\ncommodity 1,000.00 EUR\nP 2000-01-01 EUR €1\n\n"
              "include accounts.journal\ninclude vat-codes\n")
        write(self.base / "accounts.journal", "include journal:tax/accounts.CSV\n")
        write(self.base / "tax/accounts.CSV", "account assets:bank:checking\n")
        write(self.base / "vat-codes", "include tax/vat.custom\n")
        leaf = self.base / "tax/vat.custom"
        write(leaf, "account expenses:vat\n")
        self.imports(1, RUN_SCRIPTS)
        self.assert_warm()
        write(leaf, "account expenses:vat:reduced\n")
        old_timestamp(leaf)
        rebuilt = self.imports(1, SKIP_UNCHANGED)
        self.assertIn(f"changed input: {leaf}", rebuilt.stdout + rebuilt.stderr)
        self.assert_warm(SKIP_UNCHANGED)
        self.full_parity(RUN_SCRIPTS)
        write(leaf, "alias expenses:coffee = expenses:cafe\n")
        fallback = self.imports(1, RUN_SCRIPTS, fallback=True)
        self.assertIn(leaf.name, fallback.stderr)
        leaf.unlink()
        self.full_parity(RUN_SCRIPTS, ok=False)

    def test_directives_and_fallback_scope(self):
        directives = self.base / "directives.journal"
        write(directives, "")
        self.imports(1, RUN_SCRIPTS)
        for line in ("account assets:bank:checking  ; type: C", "commodity 1,000.00000000 BTC  ; lots:",
                     "commodity €1,000.00", "P 2000-01-01 EUR €1"):
            write(directives, directives.read_text(encoding="utf-8") + line + "\n")
            self.imports(1, RUN_SCRIPTS)
            self.imports(0, RUN_SCRIPTS)
        write(self.base / "prices-extra.journal", "")
        write(directives, "include prices-*.journal\n")
        for mode in (RUN_SCRIPTS, SKIP_UNCHANGED):
            verbose = self.imports(1, mode, fallback=True)
            self.assertIn("line 1: include prices-*.journal", verbose.stdout + verbose.stderr)
            result = self.flow(mode, verbose=False)
            self.assertIn("all selected statements", result.stderr)
            self.assertEqual(result.stderr.count(LINK), 1)
        directives.unlink()
        safe = self.account.with_name("savings")
        write(safe / "1-in/2025/january.csv", ROW)
        write(safe / "bank-savings.rules", FIELDS + "account2 expenses:test\n")
        outside = self.tmp / "external.rules"
        write(outside, "account2 expenses:external\n")
        self.imports(2, RUN_SCRIPTS)
        write(self.rules, FIELDS + "include " + outside.as_posix() + "\n")
        result = self.imports(1, RUN_SCRIPTS, fallback=True)
        self.assertIn("1 of 2 selected statements", result.stderr)
        self.full_parity(RUN_SCRIPTS)

    def check_relative_rules(self, include, leaf):
        write(self.rules, FIELDS + "include " + include + "\n")
        write(leaf, "account2 expenses:first\n")
        self.imports(1, RUN_SCRIPTS)
        self.assertIn("expenses:first", self.journal.read_text())
        self.full_parity(RUN_SCRIPTS)
        self.assert_warm()
        write(leaf, "account2 expenses:edited\n")
        self.imports(1, RUN_SCRIPTS)
        self.assertIn("expenses:edited", self.journal.read_text())
        self.assert_warm()

    def test_relative_external_rules(self):
        shared = self.tmp / "shared"
        write(shared / "top.rules", "include nested/leaf.rules\n")
        self.check_relative_rules("../../shared/top.rules", shared / "nested/leaf.rules")

    def test_rules_with_directory_before_parent(self):
        self.check_relative_rules("nested/../local.rules", self.rules.parent / "local.rules")
        self.category.unlink()
        self.category.parent.rmdir()
        # Some platforms resolve parent components before opening the path.
        full = self.command("hledger-flow", "import")
        self.full_parity(RUN_SCRIPTS, ok=full.returncode == 0)

    def link_rules_input(self, link, target):
        try:
            link.symlink_to(target, target_is_directory=target.is_dir())
        except OSError as error:
            if getattr(error, "winerror", None) == 1314:
                self.skipTest("This Windows user cannot create symbolic links")
            raise

    def test_external_rules_symlink(self):
        external = self.tmp / "elsewhere/top.rules"
        write(external, "include leaf.rules\n")
        self.link_rules_input(self.rules.parent / "linked.rules", external)
        write(external.parent / "leaf.rules", "account2 expenses:decoy\n")
        self.check_relative_rules("linked.rules", self.rules.parent / "leaf.rules")

    def test_directory_symlink_before_parent(self):
        nested = self.tmp / "shared/nested"
        nested.mkdir(parents=True)
        self.link_rules_input(self.rules.parent / "linked-dir", nested)
        local = self.rules.parent / "top.rules"
        external = nested.parent / "top.rules"
        write(local, "account2 expenses:local\n")
        write(external, "account2 expenses:external\n")
        include = "linked-dir/../top.rules"
        # Windows normally simplifies dir/.. before opening the directory link.
        uses_external = (self.rules.parent / include).samefile(external)
        self.check_relative_rules(include, external if uses_external else local)

        # Fall back if the two includes need different files at one snapshot path.
        write(self.rules, FIELDS + "include " + include + "\ninclude top.rules\n")
        self.imports(1, RUN_SCRIPTS, fallback=uses_external)
        self.full_parity(RUN_SCRIPTS)

    def test_include_whitespace_and_missing_file(self):
        safe = self.account.with_name("savings")
        write(safe / "1-in/2025/march.csv", ROW)
        write(safe / "bank-savings.rules", FIELDS + "account2 expenses:savings\n")
        self.imports(2, RUN_SCRIPTS)
        write(self.rules, FIELDS + "include nested/categories.rules   \n")
        if os.name != "nt":
            write(self.category.with_name("categories.rules   "), "account2 expenses:with-spaces\n")
        full = self.command("hledger-flow", "import")
        self.full_parity(RUN_SCRIPTS, ok=full.returncode == 0)
        if self.release == "1.99.6":
            self.assertEqual(full.returncode, 0, full.stderr)
            self.assert_warm()
        elif self.release != "1.52.4":
            self.assertEqual(full.returncode, 0, full.stderr)
            for mode in (RUN_SCRIPTS, SKIP_UNCHANGED):
                result = self.imports(1, mode, fallback=True)
                self.assertIn("1 of 2 selected statements", result.stderr)
                self.full_parity(mode)
        write(self.rules, FIELDS + "include missing.rules\n")
        incremental, full = self.full_parity(RUN_SCRIPTS, ok=False)
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
        self.full_parity(RUN_SCRIPTS)
        if self.release != "1.99.6":
            self.imports(1, RUN_SCRIPTS, fallback=True)
            self.assertEqual(self.records(), [])
        else:
            self.assertIn("expenses:normal", self.journal.read_text())
            self.assert_warm()

        write(self.category, "include leaf.rules\n")
        write(leaf, "include categories.rules\n")
        for mode in (RUN_SCRIPTS, SKIP_UNCHANGED):
            failure = self.command("hledger-flow", "import", mode, timeout=10)
            self.assertNotEqual(failure.returncode, 0)
            self.assertRegex(failure.stderr.lower(), "cycl(?:e|ic)")
            self.assertIn("categories.rules", failure.stderr)
        write(leaf, "account2 expenses:repaired\n")
        self.imports(1, RUN_SCRIPTS)
        self.assert_warm()

    def test_fallback_aggregates_different_inputs_once(self):
        self.csv.unlink()
        write(self.csv.with_suffix(".journal"), CONSTRUCT)
        write(self.csv.with_name("another.journal"), CONSTRUCT)
        second = self.account.with_name("savings")
        write(second / "1-in/2025/may.csv", ROW)
        outside = self.tmp / "external.rules"
        write(outside, "account2 expenses:external\n")
        write(second / "bank-savings.rules", FIELDS + "include " + outside.as_posix() + "\n")
        for mode in (RUN_SCRIPTS, SKIP_UNCHANGED):
            result = self.flow(mode, verbose=False)
            self.assertEqual(result.stderr.count(LINK), 1, result.stderr)
            self.assertIn("all selected statements", result.stderr)
            self.assertEqual(result.stderr.count("unsupported input reader"), 1)
            self.assertNotIn(str(self.csv.with_suffix(".journal")), result.stderr)
            verbose = self.flow(mode)
            self.assertIn("unsupported input reader " + str(self.csv.with_suffix(".journal").relative_to(self.base)), verbose.stderr + verbose.stdout)
            self.assertIn("include", result.stderr)
            self.assertFalse(self.snapshots.exists())
            self.assertEqual(self.records(), [])
            self.full_parity(mode)


class Cache(Ledger):
    def test_incremental_reuses_release_cache_directory(self):
        ignore = self.cache / ".gitignore"
        release = self.cache / "release-check.json"
        write(ignore, "*\n")
        write(release, '{"lastFailure":"2026-11-16T00:00:00Z","lastSuccess":null}')
        for path in (ignore, release):
            old_timestamp(path)
        before = [(p.read_bytes(), p.stat().st_mtime_ns) for p in (ignore, release)]
        self.imports(1, RUN_SCRIPTS)
        self.assertEqual(len(self.records()), 1)
        self.assertEqual([(p.read_bytes(), p.stat().st_mtime_ns) for p in (ignore, release)], before)
        self.assertFalse((self.cache / "initialize.lock").exists())

    def test_hledger_arguments_invalidate_journal_records(self):
        for script in (False, True):
            if script:
                self.hook("construct", f"print({CONSTRUCT!r})")
            self.imports(1, SKIP_UNCHANGED)
            record, = self.records()
            saved = json.loads(record.read_bytes())
            inputs = saved[1]["journalRecord"]["stageInputs"]
            args = [value for key, value in inputs if key == "hledger argument"]
            self.assertIn("--conf", args)
            self.assertIn("--ignore-assertions" if script else "--explicit", args)
            inputs.remove(["hledger argument", "print"])
            write(record, json.dumps(saved))
            self.imports(1, SKIP_UNCHANGED)
            self.imports(0, SKIP_UNCHANGED)

    def test_failures_retries_and_git_ignore(self):
        self.command("git", "init", "-q", check=True)
        self.imports(1, RUN_SCRIPTS)
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
        self.imports(1, SKIP_UNCHANGED)
        self.assertEqual(ignore.read_bytes(), b"custom rule\n")
        before = record.read_bytes(), self.journal.read_bytes()
        write(self.csv, "invalid date,New statement,-7\n")
        self.flow(RUN_SCRIPTS, ok=False)
        self.assertEqual((record.read_bytes(), self.journal.read_bytes()), before)
        write(self.csv, ROW.replace("-12.34", "-15.67"))
        self.imports(1, RUN_SCRIPTS)
        record.unlink()
        record.mkdir()  # Force a record publication failure even as root.
        result = self.imports(1, RUN_SCRIPTS)
        self.assertIn("record", result.stderr.lower())
        self.assertIn("15.67", self.journal.read_text())
        record.rmdir()
        self.imports(1, RUN_SCRIPTS)
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
        write(home, inactive)
        self.imports(1, mode)
        self.full_parity(mode)
        self.assert_warm(mode)
        write(home, inactive + "[print]\n--invert\n")
        self.full_parity(mode)
        self.assertNotEqual(self.journal.read_bytes(), default)
        nearer = self.base / "hledger.conf"
        write(nearer, inactive)
        self.imports(1, mode)
        self.assertEqual(self.journal.read_bytes(), default)
        self.full_parity(mode)
        self.assert_warm(mode)
        nearer.unlink()
        self.full_parity(mode)
        self.assertNotEqual(self.journal.read_bytes(), default)

    def test_rules_config_discovery(self):
        self.config_parity(RUN_SCRIPTS)

    def test_construct_config_discovery(self):
        self.hook("construct", f"print({CONSTRUCT!r})")
        self.config_parity(SKIP_UNCHANGED)

    def test_static_queries_reuse_and_rebuild(self):
        config = self.base / "hledger.conf"
        for mode in (RUN_SCRIPTS, SKIP_UNCHANGED):
            for setting in ("not:archive", "not:coffee", "--begin=2025-01-01", "--period=2025"):
                write(config, "[print]\n" + setting + "\n")
                self.imports(1, mode)
                self.imports(0, mode)
                self.full_parity(mode)

    def test_rebuilds_and_fallbacks(self):
        config = self.base / "hledger.conf"
        for mode in (RUN_SCRIPTS, SKIP_UNCHANGED):
            write(config, "[balance]\n--period=thismonth\n")
            self.imports(1, mode)
            self.imports(0, mode)
            write(config, "[balance]\n--period=thisyear\n")
            self.imports(1, mode)
            self.imports(0, mode)
            default = self.journal.read_bytes()
            for lines, number in (("[balance]\n--period=thisyear\n[print]\n--period=thismonth\n", 4),
                                  ("--period=thismonth\n", 1), ("[general]\n--period=thismonth\n", 2),
                                  ("[print]\ndate:thismonth\n", 2), ("[print]\n--forecast\n", 2)):
                write(config, lines)
                for _ in range(2):
                    result = self.imports(1, mode, fallback=True)
                    self.assertIn("non-deterministic hledger config entry: hledger.conf, "
                                  f"line {number}: {lines.splitlines()[number-1]}", result.stderr)
            self.assertEqual(self.journal.read_bytes(), default)
            write(config, "[print]\n--invert\n")
            self.imports(1, mode)
            self.assertNotEqual(self.journal.read_bytes(), default)
            self.assert_warm(mode)
            write(config, "[print]\n--round=soft\n")
            self.imports(1, mode)
            self.assert_warm(mode)
            self.full_parity(mode)
            write(config, "[print]\n--invalid-incremental-test-option\n")
            self.full_parity(mode, ok=False)
        write(config, "[alias]\nmyaccounts = accounts\n")
        accepts_alias = self.command("hledger-flow", "import").returncode == 0
        if self.release == "1.99.6":
            self.assertTrue(accepts_alias)
        if accepts_alias:
            self.imports(1, RUN_SCRIPTS)
            self.imports(0, RUN_SCRIPTS)
        self.full_parity(RUN_SCRIPTS, ok=accepts_alias)
        # Windows finds the home config through the native profile, not HOME.
        if os.name != "nt":
            config.unlink()
            write(self.home / ".hledger.conf", "[print]\ndate:today\n")
            result = self.imports(1, RUN_SCRIPTS, fallback=True)
            self.assertIn("non-deterministic hledger config entry: ~/.hledger.conf, line 2: date:today", result.stderr)


class Diagnostics(Ledger):
    def replay(self, command, cwd=None):
        if os.name == "nt":
            return self.command("powershell.exe", "-NoProfile", "-NonInteractive",
                                "-Command", command + "; exit $LASTEXITCODE", cwd=cwd)
        return self.command("sh", "-c", command, cwd=cwd)

    def test_paste_command_from_account(self):
        write(self.category, "bad banana\n")
        # The config is relative to the import's working directory. Automatic
        # discovery from the ledger root would also find the wrong config.
        write(self.account / "account.conf", "")
        write(self.account / "hledger.conf", "")
        write(self.base / "hledger.conf", "[print]\n--invalid-test-option\n")
        for options in ([], ["--hledger-conf", "account.conf"]):
            result = self.flow(RUN_SCRIPTS, options=options, cwd=self.account, verbose=False, ok=False)
            self.assertIn("bad banana", result.stderr)
            shown = re.search(r"Command:\s*\n\s*(.*--rules-file[^\n]*)", result.stderr)
            self.assertIsNotNone(shown, result.stderr)
            for cwd in (self.base, self.account):
                replay = self.replay(shown[1], cwd)
                self.assertEqual(replay.returncode, result.returncode, replay.stderr)
                self.assertIn("bad banana", replay.stderr)

    def test_missing_rules_has_ordinary_diagnostic(self):
        self.rules.unlink()
        for mode in (RUN_SCRIPTS, SKIP_UNCHANGED):
            incremental, _ = self.full_parity(mode, ok=False)
            self.assertIn("couldn't find an hledger rules file", incremental.stderr)
            self.assertNotIn(NOTICE, incremental.stderr)

    @unittest.skipIf(os.name == "nt", "creating executable symlinks requires Windows privileges")
    def test_command_shows_configured_hledger_symlink(self):
        alias = self.tmp / "hledger-link"
        alias.symlink_to(shutil.which("hledger"))
        for script in (False, True):
            if script:
                self.hook("construct", f"print({CONSTRUCT!r})")
            def shown(*flags):
                result = self.flow(*flags, options=["-H", str(alias)])
                commands = [line for line in (result.stdout + result.stderr).splitlines()
                            if line.strip().startswith("$") and " print " in line]
                self.assertEqual(len(commands), 1)
                return commands[0]
            ordinary = shown()
            self.assertIn(alias.name, ordinary)
            self.assertEqual(shown(RUN_SCRIPTS), ordinary)

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
            result = self.flow(RUN_SCRIPTS, verbose=verbose, ok=False)
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
            replay = self.replay(command)
            self.assertEqual(replay.returncode, ordinary.returncode, replay.stderr)
            self.assertIn(Path(expected).name, replay.stderr)
            self.assertIn("bad banana", replay.stderr)
        marker = ".hledger-flow/cache/snapshots/r00000001/t00000002/import/nested/x.rules"
        write(self.csv, ROW.replace("Coffee", marker))
        write(leaf, "account2 expenses:food\n")
        self.imports(1, RUN_SCRIPTS)
        self.assertIn(marker, self.journal.read_text())
        self.full_parity(RUN_SCRIPTS)

    def test_existing_output_permissions(self):
        self.imports(1)
        self.journal.chmod(0o444)
        self.addCleanup(self.journal.chmod, 0o644)
        write(self.csv, ROW.replace("-12.34", "-7"))
        full = self.command("hledger-flow", "import")
        expected = self.journal.read_bytes()
        inc = self.command("hledger-flow", "import", RUN_SCRIPTS)
        self.assertEqual(inc.returncode, full.returncode, inc.stderr)
        self.assertEqual(self.journal.read_bytes(), expected)
        if os.name != "nt":
            self.assertEqual(self.journal.stat().st_mode & 0o777, 0o444)


class Hooks(Ledger):
    @unittest.skipIf(os.name == "nt", "Bash hooks")
    def test_recursive_shell_sources(self):
        for stage in ("preprocess", "construct"):
            hook = self.account / stage
            write(hook, '#!/usr/bin/env bash\nsource "$HOOK_LIB/middle" "$1" "$2"\n')
            hook.chmod(0o755)
            for name in ("first libs", "second libs"):
                libs = self.tmp / name
                self.env["HOOK_LIB"] = str(libs)
                write(libs / "middle", '. "./leaf"\n')
                # Bash resolves ./leaf in the working directory.
                leaf = self.base / "leaf"
                command = ('sed s/Coffee/Lunch/ "$1" > "$2"' if stage == "preprocess"
                           else 'printf "%s" ' + shlex.quote(CONSTRUCT.replace("Coffee", "Lunch")))
                write(leaf, command + "\n")
                ran = self.flow(SKIP_UNCHANGED)
                self.assertNotIn("Skipped unchanged " + stage, ran.stdout + ran.stderr)
                self.assertIn("Lunch", self.journal.read_text())
                self.imports(0, SKIP_UNCHANGED)
                write(leaf, command.replace("Lunch", "Dinner") + "\n")
                old_timestamp(leaf)
                self.imports(1, SKIP_UNCHANGED)
                self.assertIn("Dinner", self.journal.read_text())
            hook.unlink()

    @unittest.skipIf(os.name == "nt", "Bash hooks")
    def test_unresolved_shell_sources_rerun(self):
        hook = self.account / "preprocess"
        self.env["HOOK_LIB"] = str(self.tmp / "original")
        for name in ("original", "replacement"):
            write(self.tmp / name / "helper", 'cat "$1" > "$2"\n')
        for body in (
                'source "$(printf %s "$HOOK_LIB/helper")"',
                'HOOK_LIB="$HOOK_LIB/../replacement"\n. "$HOOK_LIB/helper"',
                'cd "$HOOK_LIB"\n. ./helper',
                'source "$HOOK_LIB/helper" "$1" "$2" "$HOOK_LIB"',
                '. ./missing || cat "$1" > "$2"',
                'PATH="$HOOK_LIB:$PATH"\n. helper',
                'false && . ./missing\ncat "$1" > "$2"',
                'false && . ./cycle\ncat "$1" > "$2"'):
            with self.subTest(body=body):
                write(self.base / "cycle", ". ./cycle\n")
                counter = self.tmp / "runs"
                write(hook, "#!/bin/bash\n" + body + "\necho run >> " + shlex.quote(str(counter)) + "\n")
                hook.chmod(0o755)
                for _ in range(2):
                    before = counter.stat().st_size if counter.exists() else 0
                    result = self.flow(SKIP_UNCHANGED)
                    self.assertIn(NOTICE, result.stderr)
                    self.assertGreater(counter.stat().st_size, before)

    def test_preprocess_and_mode_switches(self):
        pre = self.hook("preprocess", """
            with Path("pre-runs").open("a") as counter:
                counter.write("run\\n")
            Path(sys.argv[2]).write_bytes(source.read_bytes().split(b"\\n", 1)[1])
        """)
        def count():
            return len((self.base / "pre-runs").read_text().splitlines())
        write(self.csv, "download one\n" + ROW)
        self.imports(1, "--incremental")
        skipped = self.imports(0, SKIP_UNCHANGED)
        self.assertIn("Skipped unchanged preprocess script.", skipped.stdout + skipped.stderr)
        self.assertEqual(count(), 1)
        for file in (self.saved, self.journal):
            old_timestamp(file)
        write(self.csv, "download two\n" + ROW)
        self.imports(0, RUN_SCRIPTS)
        self.assertEqual(count(), 2)
        for file in (self.saved, self.journal):
            self.assertEqual(file.stat().st_mtime, 946684800)
        self.imports(1, "--new-files-only")
        self.imports(0, "--new-files-only")
        self.assertEqual(count(), 3)
        write(self.category, "account2 expenses:travel\n")
        self.imports(1, SKIP_UNCHANGED)
        self.assertEqual(count(), 3)
        write(self.saved, "altered\n")
        self.imports(0, SKIP_UNCHANGED)
        self.saved.unlink()
        self.imports(0, SKIP_UNCHANGED)
        self.assertEqual(count(), 5)
        self.imports(0, SKIP_UNCHANGED, cwd=self.account)
        self.assertTrue((self.account / "pre-runs").is_file())
        old_timestamp(self.csv)
        self.imports(0, SKIP_UNCHANGED, cwd=self.account)
        self.assertEqual((self.account / "pre-runs").read_text().splitlines(), ["run"])
        pre.unlink()
        self.imports(1, SKIP_UNCHANGED)
        write(self.csv, ROW)
        old_timestamp(self.saved)
        failure = self.flow(SKIP_UNCHANGED, ok=False)
        self.assertIn("Restore the preprocess script", failure.stderr)
        self.saved.unlink()
        self.imports(1, SKIP_UNCHANGED)

    def test_construct_hidden_include_and_cwd(self):
        extra = self.base / "extra.journal"
        write(extra, CONSTRUCT)
        self.hook("construct", """
            with Path("construct-runs").open("a") as counter:
                counter.write("run\\n")
            print("include extra.journal")
        """)
        self.imports(1, RUN_SCRIPTS)
        skipped = self.imports(0, SKIP_UNCHANGED)
        self.assertIn("Skipped unchanged construct script.", skipped.stdout + skipped.stderr)
        before = self.journal.read_bytes()
        write(extra, CONSTRUCT.replace("Coffee", "Lunch"))
        self.imports(0, SKIP_UNCHANGED)
        self.assertEqual(self.journal.read_bytes(), before)
        self.imports(1, "--incremental")
        self.assertIn("Lunch", self.journal.read_text())
        self.imports(0, SKIP_UNCHANGED)
        write(self.account / "extra.journal", CONSTRUCT.replace("Coffee", "Account directory"))
        self.imports(1, SKIP_UNCHANGED, cwd=self.account)
        self.assertIn("Account directory", self.journal.read_text())
        self.imports(0, SKIP_UNCHANGED, cwd=self.account)
        self.assertEqual((self.account / "construct-runs").read_text().splitlines(), ["run"])
        self.full_parity(SKIP_UNCHANGED)


    @unittest.skipIf(os.name == "nt", "POSIX executable bit")
    def test_execute_permission_required_before_skipping_script(self):
        for name, body in (("preprocess", "Path(sys.argv[2]).write_bytes(source.read_bytes())"),
                           ("construct", f"print({CONSTRUCT!r})")):
            hook = self.hook(name, body)
            self.imports(1, SKIP_UNCHANGED)
            self.imports(0, SKIP_UNCHANGED)
            before = self.journal.read_bytes()
            hook.chmod(0o644)
            failed, _ = self.full_parity(SKIP_UNCHANGED, ok=False)
            self.assertNotIn(NOTICE, failed.stderr)
            self.assertEqual(self.journal.read_bytes(), before)
            hook.chmod(0o755)
            self.imports(0, SKIP_UNCHANGED)

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
        self.imports(1, RUN_SCRIPTS)
        write(self.base / "category", "travel")
        self.imports(1, RUN_SCRIPTS)
        self.assertIn("expenses:travel", self.journal.read_text())
        write(self.base / "touch-source", "")
        self.imports(0, RUN_SCRIPTS)
        write(self.base / "touch-source", "")
        write(self.csv, ROW.replace("Coffee", "Tea"))
        self.imports(1, SKIP_UNCHANGED, fallback=True)
        result = self.imports(0, SKIP_UNCHANGED)
        self.assertIn("to preprocess ", result.stderr)
        result = self.imports(0, SKIP_UNCHANGED)
        self.assertNotIn("to preprocess ", result.stderr)

        self.hook("construct", """
            if Path("touch-construct-input").exists():
                changed_time = source.stat().st_mtime + 60
                source.write_bytes(source.read_bytes())
                os.utime(source, (changed_time, changed_time))
                Path("touch-construct-input").unlink()
            print("2025-03-01 Coffee\\n    expenses:lunch  4\\n    assets:cash")
        """)
        write(self.base / "touch-construct-input", "")
        self.imports(1, RUN_SCRIPTS)
        write(self.base / "touch-construct-input", "")
        self.imports(1, SKIP_UNCHANGED, fallback=True)
        self.imports(1, SKIP_UNCHANGED)
        self.imports(0, SKIP_UNCHANGED)


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
            result = self.imports(1, RUN_SCRIPTS, fallback=units == 260)
            self.assertEqual((self.base / "hook-runs").read_text(), "x")
            if units == 259:
                self.imports(0, SKIP_UNCHANGED)
            else:
                self.assertIn("overlong Windows snapshot path", result.stderr)
                self.imports(1, SKIP_UNCHANGED, fallback=True)
            self.assertEqual((self.base / "hook-runs").read_text(), "x")
            self.full_parity(RUN_SCRIPTS)
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
            process = subprocess.Popen(["hledger-flow", "import", SKIP_UNCHANGED],
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
        self.imports(1, SKIP_UNCHANGED)
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
        self.assert_warm(SKIP_UNCHANGED)
        self.full_parity(SKIP_UNCHANGED)


if __name__ == "__main__":
    unittest.main()
