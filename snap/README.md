# Build and install a Snap package

On x86-64 Linux, this recipe packages the published hledger-flow executable
with the CLI from the [hledger 1.99.6 pre-release](https://github.com/hledgerorg/hledger/releases/tag/1.99.6).
The build downloads the Linux x64 archive for the version in `snapcraft.yaml`
from the matching public GitHub release. It verifies the archive against the
immutable release’s SHA-256 and checks the executable’s `--version`.
Snapcraft verifies the bundled hledger against the checksum in the recipe.
The bundled hledger is available to hledger-flow and its account scripts.

You need snap installed, Snapcraft, and a configured Snapcraft build
provider. If you do not already have Snapcraft, install it with:

```sh
sudo snap install snapcraft --classic
```

From the root of the repository, build the snap on an x86-64 machine.

```sh
snap_version=$(awk '/^version:/ {gsub(/["\047]/, "", $2); print $2}' snap/snapcraft.yaml)
snapcraft pack --platform=amd64
```

Install the resulting local package:

```sh
sudo snap install --dangerous "./hledger-flow_${snap_version}_amd64.snap"
snap run hledger-flow --version
```

`--dangerous` permits installation of the unsigned local build. The snap
runs under strict confinement. Keep the ledger in a non-hidden directory
under your home directory. If the `home` plug is disconnected, run
`sudo snap connect hledger-flow:home`.

Account scripts can call `hledger` by name. Other commands and interpreters
used by `preprocess` and `construct` must also be accessible inside the snap.

See the [usage guide](../docs/README.md) for importing statements and
producing reports.

Read the changes for the installed version with
`cat /snap/hledger-flow/current/usr/share/doc/hledger-flow/release-notes.md`.
The full `ChangeLog.md` is in the same directory.
