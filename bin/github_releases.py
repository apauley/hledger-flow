"""GitHub REST requests and terminal formatting for the read-only release reports."""
import argparse
import json
import os
import re
import sys
import urllib.error
import urllib.request

DEFAULT_REPO = "apauley/hledger-flow"
USE_COLOR = sys.stdout.isatty() and not os.environ.get("NO_COLOR")


def c(code, s):
    return f"\033[{code}m{s}\033[0m" if USE_COLOR else str(s)


bold, dim, cyan, green, yellow, magenta = (lambda s, k=k: c(k, s) for k in ("1", "2", "36", "32", "33", "35"))
ANSI = re.compile(r"\033\[[0-9;]*m")


def platform(asset_name):
    n = asset_name.lower()
    # "windows", not "win": "darwin" contains "win".
    found = [os_ for os_, keys in (("linux", ("linux", "ubuntu", "debian", "manjaro")),
                                   ("macos", ("darwin", "mac", "osx")), ("windows", ("windows",)))
             if any(k in n for k in keys)]
    if len(found) > 1:
        return "+".join(found)  # a bundle with binaries for several OSes
    if not found:
        return "other"
    arch = "arm64" if ("arm64" in n or "aarch64" in n) else "arm32" if "arm" in n else "x64"
    return f"{found[0]}-{arch}"


def bar(value, maximum, width):
    if maximum <= 0 or width <= 0:
        return ""
    eighths = max(round(value / maximum * width * 8), 1 if value > 0 else 0)
    full, part = divmod(eighths, 8)
    return "█" * full + ("", "▏", "▎", "▍", "▌", "▋", "▊", "▉")[part]


def table(rows, aligns, bars=None, term_w=80):
    """Print aligned rows; if bars is a list of callables (width -> str), they fill the remaining terminal width."""
    widths = [max(len(ANSI.sub("", str(r[i]))) for r in rows) for i in range(len(aligns))]
    if bars:
        bar_w = max(10, min(50, term_w - sum(widths) - 2 * len(widths) - 3))
        rows = [r + [b(bar_w) if b else ""] for r, b in zip(rows, bars)]
        widths, aligns = widths + [0], aligns + ["<"]
    for r in rows:
        cells = []
        for cell, w, a in zip(r, widths, aligns):
            pad = " " * (w - len(ANSI.sub("", str(cell))))
            cells.append(f"{cell}{pad}" if a == "<" else f"{pad}{cell}")
        print("  " + "  ".join(cells).rstrip())


def repository(value):
    if not re.fullmatch(r"[A-Za-z0-9-]+/[A-Za-z0-9_.-]+", value) or value.split("/")[1] in (".", ".."):
        raise argparse.ArgumentTypeError("expected a GitHub repository in OWNER/REPO form")
    return value


def github_get(repo, endpoint):
    url = f"https://api.github.com/repos/{repo}/releases{endpoint}"
    headers = {"Accept": "application/vnd.github+json", "User-Agent": "hledger-flow-release-report"}
    if token := os.environ.get("GITHUB_TOKEN"):
        headers["Authorization"] = f"Bearer {token}"
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers=headers), timeout=30) as response:
            return json.load(response)
    except urllib.error.HTTPError as error:
        sys.exit(f"GitHub returned {error.code} ({error.reason}) for {url}")
    except (OSError, ValueError) as error:
        sys.exit(f"Cannot read GitHub release data for {repo}: {error}")
