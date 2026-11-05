# Release Procedure

This project has 2 release outputs:

- GitHub releases with prebuilt binaries for Linux, macOS (signed and notarized), and Windows
- A source package on [Hackage](https://hackage.haskell.org/package/hledger-flow)

Every step is a `bin/release` subcommand. Each one checks its preconditions,
refuses to run in the wrong state, and asks before anything permanent. Run
`bin/release` without arguments for a summary.

Requirements: `stack`, `cabal`, `gh` (authenticated), `jq`, `perl`, and a GPG
or SSH signing key for signed tags.

## Quick reference

``` bash
bin/release deps              # optional: latest Stackage LTS + cabal bounds
bin/release precheck          # review "## Unreleased" against the commits, suggest versions
bin/release bump major        # major|minor|patch|<version>: ChangeLog + package.yaml
bin/release ci                # push master, wait for green CI
bin/release tag               # all checks, signed tag, wait for release build, verify assets
bin/release hackage           # candidate upload, review, permanent publish
bin/release publish           # publish the GitHub draft release
```

Every command can be re-run after fixing a failure. Before `tag`, fixing a
problem is just another commit on `master`; nothing permanent happens until
the tag is pushed.

## During development: keep the ChangeLog current

`ChangeLog.md` always starts with an `## Unreleased` section. Add a line there
in the same commit as any user-visible change. `bin/release precheck` and
`bin/release bump` refuse to run while that section is empty.

## Step 1: Update dependencies (optional)

``` bash
bin/release deps              # or: bin/release deps lts-24.65
```

This:

1.  Sets `resolver` in `stack.yaml` to the latest Stackage LTS (or the one given).
2.  Rewrites every bounded dependency in `package.yaml` to
    `>= <snapshot version> && < <next PVP major>`, e.g. `path 0.9.6` becomes
    `path >= 0.9.6 && < 0.10`. `base` and unbounded test dependencies are left alone.
3.  Regenerates `hledger-flow.cabal`, runs `bin/build-and-test` and `cabal check`.
4.  Shows the diff and commits `stack.yaml`, `stack.yaml.lock`, `package.yaml` and
    `hledger-flow.cabal` as `Update stackage to <resolver>`.

If the build breaks on the new snapshot, fix the code, commit, and re-run.

## Step 2: Check the unreleased work

``` bash
bin/release precheck
```

Run this at the start of every release, before the version number is chosen.
It does the checks that don't depend on the version:

1.  On `master` with a clean working tree.
2.  The signing secrets and variables exist and immutable releases are enabled
    in the repository (see step 5).
3.  The committed `hledger-flow.cabal` matches `package.yaml`, and `cabal check` passes.
4.  `ChangeLog.md` starts with a non-empty `## Unreleased` section.

It then prints every commit since the previous release tag next to the
`## Unreleased` section, and asks you to confirm the section covers every
user-visible change. If it doesn't, answer `n`, update `ChangeLog.md`, commit,
and run it again.

Finally it prints the three candidate version numbers.

## Step 3: Choose the version number and bump it

Versions follow the [Haskell PVP](https://pvp.haskell.org/) `A.B.C.D` scheme,
applied to what users of the command-line tool see. Read the reviewed
`## Unreleased` section and pick the highest level that any entry needs.
The level names are PVP's, not SemVer's: `minor` bumps the third component.

| Level   | Example         | Use when any entry...                                                                                                                                                                                                                                        |
|---------|-----------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `major` | 0.16.4 → 0.17.0 | requires users to change something: a removed or renamed option or subcommand, a changed directory convention or script hook interface, generated journal or include output that changes for unchanged input, a raised minimum `hledger` version, a dropped platform, or a breaking library API change |
| `minor` | 0.16.4 → 0.16.5 | adds something or changes behaviour compatibly: a new option, feature, report or platform, a backwards-compatible behaviour change, or a Stackage/GHC update                                                                                                   |
| `patch` | 0.16.4 → 0.16.4.1 | only fixes bugs or documentation, with no new behaviour to learn and no change to generated files                                                                                                                                                          |

Generated files count as an interface: users keep them in version control, so a
release that changes them for unchanged input shows up as a diff in every
user's repository (as 0.15.0 did). This includes bug fixes: a fix that corrects
a generated journal, include file, or report is still `major`.

``` bash
bin/release bump major        # or minor, patch, or an explicit version like 0.17.0
```

This checks the new version is greater than the current one and not yet tagged,
renames `## Unreleased` in `ChangeLog.md` to `## <version>`, sets `version:` in
`package.yaml`, regenerates `hledger-flow.cabal`, runs the ChangeLog check from
step 5, and commits all three files as
`Bump version to <version>`.

Add a fresh `## Unreleased` heading to `ChangeLog.md` after the release.

## Step 4: Push and wait for CI

``` bash
bin/release ci
```

Pushes `master` and follows the `Build & Release` run for `HEAD` until it
finishes. If it fails, fix, commit, and run it again.

## Step 5: Check, tag, and build the release

``` bash
bin/release tag
```

This first runs `bin/release check`, which stops on the first failure:

1.  On `master`, clean working tree, `HEAD` equals `origin/master`.
2.  Tag `v<version>` does not exist yet.
3.  The `Build & Release` run for `HEAD` on `master` succeeded.
4.  The signing secrets (`MACOS_CERTIFICATE_BASE64`, `MACOS_CERTIFICATE_PASSWORD`,
    `APPLE_API_KEY_BASE64`) and variables (`APPLE_API_KEY_ID`, `APPLE_API_ISSUER_ID`)
    exist in the repository, and immutable releases are enabled
    (Settings → General → Releases).
5.  The committed `hledger-flow.cabal` matches `package.yaml`; `cabal check` and `cabal sdist` pass.
6.  **ChangeLog.md is up to date** (`bin/release changelog`):
    - the first section is `## <version>`, matching `package.yaml`
      (so no `## Unreleased` section is left over);
    - that section is not empty;
    - `ChangeLog.md` changed since the previous release tag.

    It prints every commit since the previous tag next to the ChangeLog entry.
    `precheck` already reviewed the commits up to the version bump, so it
    only asks about commits made after the last `ChangeLog.md` change (for
    example, fixes made while CI was failing). If the entry is missing
    something, answer `n`, edit `ChangeLog.md`, commit, and start again from
    step 4.

This is the final gate: it runs right before the tag is created, because the
tag is the first step that can't be redone with another commit.

If everything passes, it creates the signed tag (`bin/tag-release`), pushes it,
follows the tag's `Build & Release` run, and verifies the draft release has
all of `hledger-flow-linux-x64.tar.gz`, `hledger-flow-linux-arm64.tar.gz`,
`hledger-flow-macos-x64.tar.gz`, `hledger-flow-macos-arm64.tar.gz`, and
`hledger-flow-windows-x64.zip`.

The `release` job attests the build provenance of every archive before
uploading it. `bin/release tag` downloads each asset and checks with
`gh attestation verify` that it was built by `build.yaml` in this repository
from `refs/tags/v<version>` on a GitHub-hosted runner.

CI enforces the same ChangeLog check: the tag's `release` job fails if the tag,
`package.yaml`, and `ChangeLog.md` disagree, and it uses the ChangeLog section
as the GitHub release notes.

A failed `Notarize` job is reported as a warning and does not block the
release; follow [Retry notarization](#retry-in-github-actions). Any other failed job stops the command.

The Windows ZIP contains `hledger-flow.exe`; CI checks its `--help` and
`--version` with only Windows system directories on `PATH`. Users still need
`hledger` installed to run imports and reports.

macOS signing is made possible by [FernTrail B.V.](https://ferntrail.tech/)

## Step 6: Publish to Hackage

``` bash
bin/release hackage
```

Verifies the GitHub release assets, builds the sdist, and uploads a candidate.
Review the candidate page it prints, then confirm to publish. Hackage
publishes are permanent.

## Step 7: Publish the GitHub release

``` bash
bin/release publish
```

Verifies the assets and that the version is live on Hackage, then publishes the
draft as the latest release, checks the release attestation GitHub creates on
publication (`gh release verify`), and prints its URL.

Publishing is permanent: an immutable release's tag and assets can't be
changed or replaced afterwards. A problem found later needs a new version.

Download counts: `bin/show-release-downloads`.

## Recovery: notarization

### Retry in GitHub Actions

1.  Inspect the failed `Notarize` job's submission ID and Apple log. For a timeout, check the existing submission before retrying.
2.  Confirm the original run is within GitHub's 30-day rerun window and its `signed-hledger-flow-*` artifacts have not expired. Otherwise, use the Mac procedure below.
3.  Re-run only the failed jobs. After publication, never select `Re-run all jobs`.
4.  Confirm both `Notarize` jobs pass: look for `Apple notarization ticket is already available` or a new `Accepted` submission.

``` bash
gh run rerun RUN_ID --failed --repo apauley/hledger-flow
```

### Notarize an existing release from a Mac

Use Xcode's `notarytool` and the hledger-flow App Store Connect `.p8` key. Store credentials once:

``` bash
xcrun notarytool store-credentials hledger-flow \
  --key /absolute/path/to/AuthKey_KEY_ID.p8 \
  --key-id KEY_ID --issuer ISSUER_ID
```

Download, verify, and submit the exact release executable. Do not rebuild, strip, or re-sign it:

``` bash
TAG='v<version>'
PLATFORM=macos-arm64
work_dir="$(mktemp -d)"
gh release download "$TAG" --repo apauley/hledger-flow \
  --pattern "hledger-flow-${PLATFORM}.tar.gz" --dir "$work_dir"
tar -xzf "$work_dir/hledger-flow-${PLATFORM}.tar.gz" -C "$work_dir"
codesign --verify --strict --verbose=4 \
  --test-requirement '=anchor apple generic and certificate leaf[subject.OU] = "4UJZ75B7VG"' \
  "$work_dir/hledger-flow"
ditto -c -k --keepParent "$work_dir/hledger-flow" "$work_dir/notary.zip"
xcrun notarytool submit "$work_dir/notary.zip" --keychain-profile hledger-flow
```

Wait for completion and inspect the log even if the wait fails. After a timeout, repeat `wait` with the same ID:

``` bash
SUBMISSION_ID='UUID-from-submit'
xcrun notarytool wait "$SUBMISSION_ID" --keychain-profile hledger-flow --timeout 30m
xcrun notarytool log "$SUBMISSION_ID" --keychain-profile hledger-flow
```

After Apple reports `Accepted`, verify the online ticket:

``` bash
codesign --verify --strict --verbose=4 \
  --test-requirement '=notarized' --check-notarization "$work_dir/hledger-flow"
```

Repeat with `PLATFORM=macos-x64`. Keep the existing release assets; do not upload replacements. For an Apple rejection, fix the issue in the log before retrying.

## After rotating Apple credentials

`bin/release check` only confirms the secrets exist. After rotating them,
manually run `Build & Release` on `master` in GitHub Actions and confirm both
`Sign` and `Notarize` jobs pass with identity
`Developer ID Application: FernTrail B.V. (4UJZ75B7VG)` before the next release.
