"""Prepare a Cabal build report and reconcile release artifacts with Hackage."""
import gzip
import json
import os
import re
import shlex
import subprocess
import sys
import time
from datetime import datetime, timezone
from html import unescape
from pathlib import Path


def executable_report(package, summary, output):
    # Cabal can append both the library and executable to the same summary.
    records = re.split(r"(?m)(?=^package:)", summary.read_text())
    # Only the executable depends on the package's own library.
    matches = [
        record for record in records
        if re.search(r"(?m)^install-outcome:[ \t]+InstallOk$", record)
        and re.search(r"(?m)^package:[ \t]+" + re.escape(package) + "$", record)
        and re.search(r"(?m)^[ \t]+" + re.escape(package) + "$", record)
    ]
    if len(matches) != 1:
        raise ValueError(f"expected one successful executable build in {summary}")
    first, rest = matches[0].strip().split("\n", 1)
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")
    output.write_text(f"{first}\ntime: {stamp}\n{rest}\n")


def curl_credentials():
    token = os.environ.get("HACKAGE_TOKEN")
    if not token:
        diff = subprocess.check_output(["cabal", "user-config", "diff"], text=True)
        fields = dict(re.findall(r"(?m)^\+ (token|username|password|password-command): (.+)$", diff))
        token = fields.get("token")
    if token:
        return "header = " + json.dumps("Authorization: X-ApiKey " + token) + "\n"
    password = fields.get("password")
    if not password and fields.get("password-command"):
        password = subprocess.check_output(shlex.split(fields["password-command"]), text=True).rstrip("\r\n")
    if not password or not fields.get("username"):
        raise ValueError("configure Hackage upload credentials in Cabal or set HACKAGE_TOKEN")
    return "digest\nuser = " + json.dumps(fields["username"] + ":" + password, ensure_ascii=False) + "\n"


def request(url, *upload, allowed=(200,)):
    # Public reads may redirect to the content CDN. Keep authenticated writes on
    # the original HTTPS endpoint and pass credentials through stdin, outside argv.
    args = ["curl", "--silent", "--show-error", "--max-time", "120", "--http1.1"]
    if upload:
        args += ["--config", "-", *upload]
        config = curl_credentials().encode()
    else:
        url += f"?release-check={time.time_ns()}"
        args += ["--location", "--header", "Cache-Control: no-cache"]
        config = b""
    raw = subprocess.check_output([*args, "--write-out", "\n%{http_code}", url], input=config)
    body, code = raw.rsplit(b"\n", 1)
    if int(code) not in allowed:
        raise ValueError(f"{url} returned HTTP {code.decode()}: {body[:300].decode(errors='replace')}")
    return None if code == b"404" else body


def sync_docs(url, archive):
    expected = gzip.decompress(archive.read_bytes())
    if request(url + "/docs.tar", allowed=(200, 404)) != expected:
        request(url + "/docs", "--header", "Content-Type: application/x-tar",
                "--header", "Content-Encoding: gzip", "--upload-file", str(archive), allowed=(200, 204))
        if request(url + "/docs.tar") != expected:
            raise ValueError(f"uploaded docs differ at {url}; retry the release step")
    if b"<title>hledger-flow (Index)</title>" not in request(url + "/docs/doc-index.html"):
        raise ValueError(f"could not verify the published documentation index at {url}")


def has_report(url, expected):
    # Hackage's text listing contains (BuildReportId N, BuildReport {...}) pairs.
    listing = request(url + "/reports/.txt").decode().strip()
    ids = re.findall(r"\(BuildReportId (\d+),", listing)
    if not ids and listing != "[]":
        raise ValueError(f"unrecognized build report list at {url}")
    for ident in reversed(ids):
        if request(f"{url}/reports/{ident}.txt").decode().split() == expected:
            return True
    return False


def sync_report(url, report):
    expected = report.read_text().split()
    if not has_report(url, expected):
        request(url + "/reports/", "--header", "Content-Type: text/plain",
                "--data-binary", "@" + str(report), allowed=(303,))
        if not has_report(url, expected):
            raise ValueError(f"submitted report is not visible at {url}/reports/; retry the release step")


def verify_badges(url):
    page = unescape(request(url).decode())
    badges = re.search(r'<div id="badges"[^>]*>(.*?)</div>', page, re.S)
    for label, message in (("Build", "InstallOk"), ("Documentation", "Available")):
        if not badges or f"label={label}&message={message}&color=success" not in badges[1]:
            raise ValueError(f"expected green {label} badge at {url}; inspect the Hackage page and retry")


def main(stage, package, *paths):
    if not re.fullmatch(r"hledger-flow-\d+(?:\.\d+)+", package):
        raise ValueError("invalid release package")
    if stage == "report":
        executable_report(package, *map(Path, paths))
        return
    if stage in ("upload-candidate", "upload-publish"):
        endpoint = "packages/candidates/" if stage == "upload-candidate" else "packages/"
        warnings = request("https://hackage.haskell.org/" + endpoint,
                           "--header", "Accept: text/plain", "--form", "package=@" + paths[0],
                           allowed=(200, 204))
        if warnings:
            print(warnings.decode(errors="replace"))
        return
    url = "https://hackage.haskell.org/package/" + package
    if stage == "badges":
        verify_badges(url)
        return
    directory = Path(paths[0])
    if stage == "candidate":
        url += "/candidate"
    elif stage != "publish":
        raise ValueError("expected candidate, publish, badges or report")
    sync_docs(url, directory / (package + "-docs.tar.gz"))
    if stage == "publish":
        sync_report(url, directory / (package + ".report"))
        verify_badges(url)


if __name__ == "__main__":
    try:
        main(*sys.argv[1:])
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        sys.exit(str(error))
