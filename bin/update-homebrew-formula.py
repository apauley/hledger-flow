"""Update the versioned fields in a temporary FernTrail tap checkout."""
import hashlib
import json
from pathlib import Path
import re
import sys


def replace_once(text, pattern, replacement):
    updated, count = re.subn(pattern, replacement, text, flags=re.MULTILINE)
    if count != 1:
        raise ValueError(f"expected one match for {pattern!r}, found {count}")
    return updated


def pin(text, url, checksum):
    pattern = r'^( +url "' + re.escape(url) + r'"\n +sha256 ")[0-9a-f]{64}"$'
    return replace_once(text, pattern, lambda m: m[1] + checksum + '"')


def update(tag, assets, license_bytes, tap):
    version_pattern = r"v([0-9]+(?:\.[0-9]+)*)"
    if not re.fullmatch(version_pattern, tag):
        raise ValueError(f"invalid release tag {tag!r}")
    formula = tap / "Formula/hledger-flow.rb"
    text = formula.read_text(encoding="utf-8")
    tags = re.findall(r'^  tag = "' + version_pattern + r'"$', text, re.MULTILINE)
    if len(tags) != 1:
        raise ValueError("expected one release tag in the tap formula")
    old = tags[0]
    if tuple(map(int, old.split("."))) > tuple(map(int, tag[1:].split("."))):
        raise ValueError(f"tap already contains a newer release (v{old})")
    text = text.replace(f'  tag = "v{old}"', f'  tag = "{tag}"')

    for platform in ("macos-arm64", "macos-x64", "linux-arm64", "linux-x64"):
        archive = f"hledger-flow-{platform}.tar.gz"
        digests = [asset.get("digest") for asset in assets if asset["name"] == archive]
        if len(digests) != 1 or not re.fullmatch(r"sha256:[0-9a-f]{64}", digests[0] or ""):
            raise ValueError(f"missing or ambiguous SHA-256 for {archive} on {tag}")
        text = pin(text, "#{release}/" + archive, digests[0][7:])
    text = pin(text, "https://raw.githubusercontent.com/apauley/hledger-flow/#{tag}/LICENSE",
               hashlib.sha256(license_bytes).hexdigest())

    readme = tap / "README.md"
    docs = readme.read_text(encoding="utf-8").replace(f"**hledger-flow {old}**", f"**hledger-flow {tag[1:]}**")
    upstream = "https://github.com/apauley/hledger-flow"
    docs = docs.replace(f"{upstream}/releases/tag/v{old})", f"{upstream}/releases/tag/{tag})")
    docs = docs.replace(f"{upstream}/blob/v{old}/LICENSE", f"{upstream}/blob/{tag}/LICENSE")
    # Keep repository text UTF-8 with LF even when the release runs on Windows.
    formula.write_bytes(text.encode("utf-8"))
    readme.write_bytes(docs.encode("utf-8"))


if __name__ == "__main__":
    tag, assets_path, license_path, tap_path = sys.argv[1:]
    try:
        update(tag, json.loads(Path(assets_path).read_text())["assets"],
               Path(license_path).read_bytes(), Path(tap_path))
    except (OSError, KeyError, ValueError) as error:
        sys.exit(f"error: cannot update the Homebrew tap: {error}")
