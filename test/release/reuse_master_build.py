"""Exercise tag artifact selection without credentials or a live release."""

import hashlib
import io
import json
from pathlib import Path
import runpy
import subprocess
import tempfile
import unittest
from unittest.mock import patch
import zipfile


stage = runpy.run_path(".github/scripts/reuse-master-build.py")["stage"]
REPO = "owner/repo"
SHA = "1" * 40
RUN = {"id": 17, "head_branch": "master", "head_sha": SHA, "conclusion": "success",
       "head_repository": {"full_name": REPO}, "path": ".github/workflows/build.yaml",
       "html_url": f"https://github.com/{REPO}/actions/runs/17", "event": "push"}


class MasterBuildTest(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.dest = Path(temp.name)
        self.runs = [RUN.copy()]
        self.artifacts = []
        self.archives = {}
        self.files = {}
        for i, platform in enumerate(("linux-x64", "linux-arm64", "macos-x64", "macos-arm64", "windows-x64")):
            name = "hledger-flow-" + platform
            filename = name + (".zip" if platform.startswith("windows") else ".tar")
            payload = platform.encode()
            self.files[filename] = payload
            buf = io.BytesIO()
            with zipfile.ZipFile(buf, "w") as wrapper:
                wrapper.writestr(filename, payload)
            archive = buf.getvalue()
            self.artifacts.append({"id": i, "name": name, "expired": False,
                                   "digest": "sha256:" + hashlib.sha256(archive).hexdigest()})
            self.archives[f"repos/{REPO}/actions/artifacts/{i}/zip"] = archive
        self.addCleanup(patch.stopall)
        patch.dict(stage.__globals__, api=self.api).start()

    def api(self, path):
        base = f"repos/{REPO}/actions"
        if path == f"{base}/workflows/build.yaml/runs?event=push&branch=master&head_sha={SHA}&status=success&per_page=1":
            return json.dumps({"workflow_runs": self.runs})
        if path == f"{base}/runs/17/artifacts?per_page=100":
            return json.dumps({"artifacts": self.artifacts})
        result = self.archives[path]
        if isinstance(result, Exception):
            raise result
        return result

    def test_reuse_binds_verified_inputs_to_master_run(self):
        self.assertEqual(stage(REPO, SHA, self.dest), 17)
        link = json.loads((self.dest / "build-inputs.json").read_text())
        self.assertEqual(link["environment"]["buildRun"], RUN["html_url"])
        self.assertEqual(link["environment"]["sourceCommit"], SHA)
        self.assertEqual(len(link["materials"]), 5)
        for material in link["materials"]:
            name = material["name"]
            self.assertEqual((self.dest / name).read_bytes(), self.files[name])
            self.assertEqual(material["digest"]["sha256"], hashlib.sha256(self.files[name]).hexdigest())

    def test_no_run_or_incomplete_set_rebuilds(self):
        self.runs = []
        self.assertIsNone(stage(REPO, SHA, self.dest))
        self.runs = [RUN]
        self.artifacts[-1]["expired"] = True
        self.assertIsNone(stage(REPO, SHA, self.dest))
        self.artifacts.pop()
        self.assertIsNone(stage(REPO, SHA, self.dest))
        self.assertFalse((self.dest / "build-inputs.json").exists())

    def test_wrong_source_cannot_supply_inputs(self):
        for key, value in (("head_sha", "2" * 40), ("event", "pull_request"),
                           ("head_repository", {"full_name": "other/repo"}),
                           ("head_branch", "other"), ("path", "other.yaml"), ("conclusion", "failure")):
            with self.subTest(key=key), self.assertRaises(ValueError):
                self.runs = [{**RUN, key: value}]
                stage(REPO, SHA, self.dest)

    def test_expiry_during_download_rebuilds_but_api_errors_fail(self):
        path = next(reversed(self.archives))
        for code in (404, 410, 403, 500):
            with self.subTest(code=code):
                self.archives[path] = subprocess.CalledProcessError(1, "gh", stderr=f"gh: error (HTTP {code})".encode())
                if code in (404, 410):
                    self.assertIsNone(stage(REPO, SHA, self.dest))
                else:
                    with self.assertRaises(subprocess.CalledProcessError):
                        stage(REPO, SHA, self.dest)
                self.assertFalse((self.dest / "build-inputs.json").exists())

    def test_corrupt_or_unexpected_archive_fails(self):
        path = next(iter(self.archives))
        self.archives[path] = self.archives[next(reversed(self.archives))]
        with self.assertRaisesRegex(ValueError, "SHA-256"):
            stage(REPO, SHA, self.dest)
        self.artifacts[0]["digest"] = self.artifacts[-1]["digest"]
        with self.assertRaisesRegex(ValueError, "Expected exactly"):
            stage(REPO, SHA, self.dest)


if __name__ == "__main__":
    unittest.main()
