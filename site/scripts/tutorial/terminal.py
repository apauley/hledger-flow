"""Run one tutorial command with terminal output and no interactive input."""
import errno
import os
import pty
import subprocess
import sys

master, slave = pty.openpty()
with os.fdopen(master, "rb", buffering=0) as output:
    with os.fdopen(slave, "wb") as terminal:
        child = subprocess.Popen(sys.argv[1:], stdin=subprocess.DEVNULL,
                                 stdout=terminal, stderr=terminal)
    while True:
        try:
            chunk = output.read(65536)
        except OSError as error:
            if error.errno != errno.EIO:
                raise
            break  # Linux reports EIO when the last slave handle closes.
        if not chunk:
            break
        sys.stdout.buffer.write(chunk)
sys.exit(child.wait())
