"""Stage the five tested release inputs and their provenance, or request a tag build."""

import hashlib
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import zipfile


PLATFORMS = ("linux-x64", "linux-arm64", "macos-x64", "macos-arm64", "windows-x64")


def api(path):
    result = subprocess.run(
        ["gh", "api", path], stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True
    )
    return result.stdout


def fetch_if_available(path):
    try:
        return api(path)
    except subprocess.CalledProcessError as error:
        # The run or artifact can disappear after it was listed.
        if b"(HTTP 404)" in error.stderr or b"(HTTP 410)" in error.stderr:
            return None
        raise


def stage(repo, sha, destination):
    base = f"repos/{repo}/actions"
    runs = json.loads(api(
        f"{base}/workflows/build.yaml/runs?event=push&branch=master"
        f"&head_sha={sha}&status=success&per_page=1"
    ))["workflow_runs"]
    if not runs:
        print(f"No successful master push for {sha}. Rebuilding.")
        return
    run = runs[0]
    expected = {
        "event": "push", "head_branch": "master", "head_sha": sha,
        "conclusion": "success", "path": ".github/workflows/build.yaml",
    }
    if run["head_repository"]["full_name"] != repo or any(run[key] != value for key, value in expected.items()):
        raise ValueError("GitHub returned a build outside the requested workflow, ref or commit")

    run_id = run["id"]
    inventory = fetch_if_available(f"{base}/runs/{run_id}/artifacts?per_page=100")
    if inventory is None:
        print(f"Run {run_id} is no longer available. Rebuilding.")
        return
    artifacts = {a["name"]: a for a in json.loads(inventory)["artifacts"] if not a["expired"]}
    missing = [p for p in PLATFORMS if f"hledger-flow-{p}" not in artifacts]
    if missing:
        print(f"Missing or expired artifacts in run {run_id}: {', '.join(missing)}. Rebuilding.")
        return

    materials = []
    for platform in PLATFORMS:
        name = f"hledger-flow-{platform}"
        artifact = artifacts[name]
        archive = fetch_if_available(f"{base}/artifacts/{artifact['id']}/zip")
        if archive is None:
            print(f"{name} is no longer available in run {run_id}. Rebuilding.")
            return
        if "sha256:" + hashlib.sha256(archive).hexdigest() != artifact["digest"]:
            raise ValueError(f"{name} differs from GitHub's recorded SHA-256")

        filename = name + (".zip" if platform == "windows-x64" else ".tar")
        with zipfile.ZipFile(io.BytesIO(archive)) as wrapper:
            if wrapper.namelist() != [filename]:
                raise ValueError(f"Expected exactly {filename} in {name}")
            payload = wrapper.read(filename)
        (destination / filename).write_bytes(payload)
        materials.append({
            "name": filename,
            "digest": {"sha256": hashlib.sha256(payload).hexdigest()},
            "annotations": {"artifactId": artifact["id"], "archiveDigest": artifact["digest"]},
        })

    url = run["html_url"]
    predicate = {
        "name": "Package tested master binaries",
        "environment": {"buildRun": url, "sourceCommit": sha, "sourceRef": "refs/heads/master"},
        "materials": materials,
    }
    (destination / "build-inputs.json").write_text(json.dumps(predicate) + "\n")
    print(f"Verified five release inputs from {url} at {sha}.")
    return run_id


if __name__ == "__main__":
    destination = Path(sys.argv[1])
    destination.mkdir()
    try:
        run_id = stage(os.environ["GITHUB_REPOSITORY"], os.environ["GITHUB_SHA"], destination)
    except subprocess.CalledProcessError as error:
        sys.exit(error.stderr.decode(errors="replace"))
    if run_id:
        with open(os.environ["GITHUB_OUTPUT"], "a") as output:
            print(f"run_id={run_id}", file=output)
