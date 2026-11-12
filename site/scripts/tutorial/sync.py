"""Bring the tutorial pages in step with a run of run.sh.

Usage: sync.py HOME_DIR RUN_OUTPUT [--check]

- A ```text or ```hledger block right after a ```bash block becomes the
  output those commands printed in the run.
- A block with title="PATH" becomes files/<part>/PATH, the file the reader
  saves in that part.
- The version block in index.md becomes the run's version output.
- Every other code block must appear in a file the run used, its output or
  a download, so excerpts can't drift either.

The run's home is shown as /home/gawie. With --check, nothing is written:
it fails if a page differs from the run, ignoring the build platform,
commit and core count, which differ between machines.
"""
import difflib
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[2]
PAGES = REPO / "site/src/content/docs/tutorial"
DOWNLOADS = REPO / "site/public/tutorial/Downloads"
FILES = HERE / "files"
PARTS = ["part-1.md", "part-2.md", "part-3.md", "part-4.md", "part-5.md"]

BLOCK = re.compile(r"```(\w*)([^\n]*)\n(.*?)```", re.S)
PAIR = re.compile(r"```bash\n((?:(?!```).)*?)```(\s*)```(text|hledger)\n(.*?)```", re.S)
TITLED = re.compile(r'```(\w+) title="([^"]+)"\n(.*?)```', re.S)
VERSIONS = re.compile(r"```text\n\$ hledger-flow --version\n.*?```", re.S)

# Lines that differ between machines and commits, ignored by --check.
MASKS = [
    (re.compile(r"^(hledger-flow \S+) .*$", re.M), r"\1 BUILD"),
    (re.compile(r"^(hledger \S+), .*$", re.M), r"\1, PLATFORM"),
    (re.compile(r"^(  hledger-flow  \S+) .*$", re.M), r"\1 BUILD"),
    (re.compile(r"^  Cores .*$", re.M), "  Cores N"),
]


def tidy(text, home):
    """Show the run's home as /home/gawie, and re-align status's settings,
    whose column width came from the longer real path."""
    lines = text.replace(home, "/home/gawie").split("\n")
    column = None
    for i, line in enumerate(lines):
        if line.startswith("  hledger       ") and "--hledger-path" in line:
            column = line.index("--hledger-path")
        match = re.match(r"(  hledger\.conf  \S+) +(--hledger-conf)$", line)
        if match and column:
            lines[i] = match.group(1).ljust(column) + match.group(2)
    return "\n".join(lines)


def commands(run):
    """(command, output) pairs, in run order."""
    result = []
    for chunk in run.split("\n===== $ ")[1:]:
        command, _, rest = chunk.partition("\n")
        result.append((command, re.split(r"(?m)^\[rc=\d+\]", rest)[0]))
    return result


def strip(text):
    return "\n".join(line for line in text.split("\n") if line.strip())


def main():
    home, run_path = sys.argv[1], sys.argv[2]
    check = "--check" in sys.argv[3:]
    run = tidy(pathlib.Path(run_path).read_text(), home)
    failures = [line for line in run.split("\n") if line.startswith("FAILED: ")]
    ran = commands(run)
    known = "\n" + strip(run) + "\n" + "\n".join(
        strip(f.read_text()) for root in (FILES, DOWNLOADS) for f in sorted(root.rglob("*")) if f.is_file()) + "\n"

    cursor = 0
    pages = {}
    for name in PARTS:
        page = PAGES / name
        text = page.read_text()

        def output(match):
            nonlocal cursor
            shown = []
            for command in match.group(1).strip().split("\n"):
                for i in range(cursor, len(ran)):
                    if ran[i][0] == command:
                        cursor = i + 1
                        if ran[i][1].strip():
                            shown.append(ran[i][1].rstrip("\n"))
                        break
                else:
                    failures.append(f"{name}: the run has no command {command!r} at this point. Update {name[:6].replace('-', '')}.sh.")
            body = "\n".join(shown) + "\n"
            return f"```bash\n{match.group(1)}```{match.group(2)}```{match.group(3)}\n{body}```"

        def saved(match):
            source = FILES / name[:6].replace("-", "") / match.group(2)
            if not source.is_file():
                failures.append(f"{name}: no file {source.relative_to(REPO)} for the block titled {match.group(2)}.")
                return match.group(0)
            return f'```{match.group(1)} title="{match.group(2)}"\n{source.read_text()}```'

        text = TITLED.sub(saved, PAIR.sub(output, text))
        pages[page] = text

    index = PAGES / "index.md"
    versions = "".join(f"$ {cmd}\n{out.strip()}\n" for cmd, out in ran[:2])
    pages[index] = VERSIONS.sub(lambda m: f"```text\n{versions}```", index.read_text(), count=1)

    # Excerpts: every remaining block must come from somewhere real.
    for page, text in pages.items():
        filled = {m.span() for m in PAIR.finditer(text)} | {m.span() for m in TITLED.finditer(text)} | {m.span() for m in VERSIONS.finditer(text)}
        for m in BLOCK.finditer(text):
            inside = any(start <= m.start() and m.end() <= end for start, end in filled)
            if m.group(1) == "bash" or inside:
                continue
            for part in re.split(r"\n\s*\n", m.group(3).strip()):
                if f"\n{strip(part)}\n" not in known:  # whole lines only
                    failures.append(f"{page.name}: this excerpt isn't in the run, its files or the downloads:\n{part}")

    stale = []
    for page, text in pages.items():
        old = page.read_text()
        if check:
            masked = [mask(t) for t in (old, text)]
            if masked[0] != masked[1]:
                stale.append(page)
                sys.stdout.writelines(difflib.unified_diff(
                    masked[0].splitlines(True), masked[1].splitlines(True), str(page.relative_to(REPO)), "fresh run"))
        elif text != old:
            page.write_text(text)
            print(f"Updated {page.relative_to(REPO)}")

    for failure in failures:
        print(f"error: {failure}", file=sys.stderr)
    if stale:
        print("error: the tutorial's output is out of date. Run site/scripts/tutorial/update.sh and commit the result.", file=sys.stderr)
    sys.exit(1 if failures or stale else 0)


def mask(text):
    for pattern, replacement in MASKS:
        text = pattern.sub(replacement, text)
    return text


if __name__ == "__main__":
    main()
