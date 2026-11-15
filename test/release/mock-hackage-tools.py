"""Run the release helper against this fixture's Bash tools, including on Windows."""
from pathlib import Path
import runpy
import shlex
import subprocess
import sys
from unittest.mock import patch


bash, helper, *args = sys.argv[1:]
mock_dir = Path(__file__).resolve().parent / "mock"
check_output = subprocess.check_output


def mock_output(argv, **kwargs):
    tool, *arguments = argv
    if tool not in ("curl", "cabal"):
        raise ValueError(f"unexpected external command in offline Hackage test: {tool}")
    script = mock_dir / tool
    if not script.is_file():
        raise FileNotFoundError(f"missing offline Hackage mock: {tool}")
    # Pass a single command string to Git Bash. In particular, @report is
    # curl data, and must not be expanded as a Windows response file.
    command = 'exec "$BASH" ' + shlex.join([script.as_posix(), *arguments])
    return check_output([bash, "-c", command], **kwargs)


sys.argv = [helper, *args]
with patch.object(subprocess, "check_output", mock_output):
    runpy.run_path(helper, run_name="__main__")
