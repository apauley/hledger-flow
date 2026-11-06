# Release Procedure

Users can get a release through three channels:

- GitHub releases with prebuilt binaries for Linux, macOS (signed and notarized), and Windows
- A source package on [Hackage](https://hackage.haskell.org/package/hledger-flow)
- The `ferntrail/tap/hledger-flow` formula in the [FernTrail Homebrew tap](https://github.com/FernTrail/homebrew-tap)

Every step is a `bin/release` command. Commands never prompt: each one checks
its preconditions, stops with an error that names the fix, and can be re-run
once the fix is in. Run `bin/release` without arguments for a summary.

Three commands are permanent: `push-tag`, `hackage-publish` and `publish`.
Everything before `push-tag` is local or an ordinary commit on `master`, so a
problem is fixed with another commit. The steps marked **Review** are the
points to stop and check the results before going further.

Requirements: `stack`, `cabal`, `gh` (authenticated), `jq`, `perl`, Python 3, a GPG or
SSH signing key for the tag (creating the tag may ask you to unlock it), and
permission to dispatch GitHub Actions workflows, and permission to push to
`FernTrail/homebrew-tap` on `main`. Configure Git credentials for the protocol
used by `gh repo clone`. The release command clones the tap into a temporary
directory and leaves your local tap checkout alone.

### Homebrew tap identity

The tap commit uses the tap's own Git identity, not this repository's. Tell
`bin/release` where your tap checkout is, once, in this checkout:

``` bash
git config release.tapDir /path/to/homebrew-tap
```

It reads `user.name` and `user.email` from that checkout and only reads it.
Without `release.tapDir`, Git resolves the identity as it would for any
repository whose origin is the tap. To cover that case, and every other
FernTrail repository on your machine, add a conditional include to
`~/.gitconfig` (Git 2.36 or later):

``` ini
[includeIf "hasconfig:remote.*.url:git@github.com:FernTrail/**"]
  path = ~/.gitconfig-ferntrail
[includeIf "hasconfig:remote.*.url:https://github.com/FernTrail/**"]
  path = ~/.gitconfig-ferntrail
```

with `~/.gitconfig-ferntrail` holding the `[user]` name and email. `precheck`,
`check` and `publish` print the identity the tap commit will use, stop if there
is none, and warn if it differs from the author of the tap's latest commit.

For Hackage, create a dedicated token on the maintainer's
[Hackage account management page](https://hackage.haskell.org/users/account-management).
Store it as the repository Actions secret `HACKAGE_TOKEN`, for example with
`gh secret set HACKAGE_TOKEN -R apauley/hledger-flow` (paste at the prompt).
Candidate upload and Hackage publication use that secret on a GitHub-hosted runner. Local Cabal
credentials are optional.

## Quick reference

``` bash
bin/release deps              # optional: latest Stackage LTS + cabal bounds
bin/release precheck          # show "## Unreleased" next to the commits, list next versions
#   Review: the ChangeLog entries and the version number
bin/release bump major        # major|minor|patch|<version>: ChangeLog + package.yaml
bin/release ci                # push master, wait for green CI
bin/release tag               # all checks, then create the signed tag locally
#   Review: the tagged commit and its release notes
bin/release push-tag          # PERMANENT: push the tag, verify binaries, upload the candidate
#   Review: the GitHub draft and the Hackage candidate
bin/release publish           # PERMANENT: publish on Hackage and GitHub, update the tap, wait for tap CI
```

## Step 1: Update dependencies (optional)

``` bash
bin/release deps              # or: bin/release deps lts-24.65
```

This:

1.  Sets `resolver` in `stack.yaml` to the latest Stackage LTS (or the one given).
2.  Rewrites every bounded dependency in `package.yaml` to
    `>= <snapshot version> && < <next PVP major>`, e.g. `path 0.9.6` becomes
    `path >= 0.9.6 && < 0.10`. `base`, unbounded test dependencies, and
    dependencies with several version ranges are left alone.
3.  Regenerates `hledger-flow.cabal`, runs `bin/build-and-test` and `cabal check`.
4.  Shows the diff and commits `stack.yaml`, `stack.yaml.lock`, `package.yaml` and
    `hledger-flow.cabal` as `Update stackage to <resolver>`.

If the build breaks on the new snapshot, fix the code, commit, and re-run.

## Step 2: Review the unreleased work

``` bash
bin/release precheck
```

Run this at the start of every release, before the version number is chosen.
It does the checks that don't depend on the version:

1.  On `master` with a clean working tree.
2.  The signing secrets and variables exist and immutable releases are enabled
    in the repository (see step 5). The Hackage credential for the selected
    upload method is configured. Python 3 is available and the authenticated
    GitHub account has write access to the tap.
3.  The committed `hledger-flow.cabal` matches `package.yaml`, and `cabal check` passes.
4.  `ChangeLog.md` starts with a non-empty `## Unreleased` section.

It then prints every commit since the previous release tag next to the
`## Unreleased` section, followed by the candidate version numbers.

**Review** the section against the commits using the
[writing guidelines at the top of ChangeLog.md](ChangeLog.md?plain=1).

Commit any changes to `ChangeLog.md` and run `precheck` again, then choose the
version number.

## Step 3: Choose the version number and bump it

Versions follow the [Haskell PVP](https://pvp.haskell.org/) `A.B.C.D` scheme,
applied to what users of the command-line tool see. Read the reviewed
`## Unreleased` section and pick the highest level that any entry needs.
The level names are PVP's, not SemVer's: `minor` bumps the third component.

| Level   | Example         | Use when any entry...                                                                                                                                                                                                                                        |
|---------|-----------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `major` | 0.16.4 → 0.17.0 | requires users to change something: a removed or renamed option or subcommand, a changed directory convention or script hook interface, generated journal or include output that changes for unchanged input, a raised minimum `hledger` version, or a dropped platform |
| `minor` | 0.16.4 → 0.16.5 | adds something or changes behaviour compatibly: a new option, feature, report or platform, a backwards-compatible behaviour change, or a Stackage/GHC update                                                                                                   |
| `patch` | 0.16.4 → 0.16.4.1 | only fixes bugs or documentation, with no new behaviour to learn and no change to generated files                                                                                                                                                          |

Generated files count as an interface: users keep them in version control, so a
release that changes them for unchanged input shows up as a diff in every
user's repository (as 0.15.0 did). This includes bug fixes: a fix that corrects
a generated journal, include file, or report is still `major`.

**Review**: settle the ChangeLog wording and the version number before bumping.
They become the release notes and the tag.

``` bash
bin/release bump major        # or minor, patch, or an explicit version like 0.17.0
```

This checks the new version is greater than the current one and not yet tagged,
renames `## Unreleased` in `ChangeLog.md` to `## <version>`, sets `version:` in
`package.yaml`, regenerates `hledger-flow.cabal`, runs the ChangeLog check from
step 5, and commits all three files as `Bump version to <version>`.

Add a fresh `## Unreleased` heading to `ChangeLog.md` after the release.

## Step 4: Push and wait for CI

``` bash
bin/release ci
```

Pushes `master` and waits for the `Build & Release` run for `HEAD`, printing
its URL. If the command stops before the run finishes, run it again to keep
waiting.

When jobs fail, it lists them with the commands to read their logs and to
rerun them. For a transient failure, such as a network error, rerun the failed
jobs and run `bin/release ci` again to wait for them:

``` bash
gh run rerun RUN_ID --failed -R apauley/hledger-flow
bin/release ci
```

Otherwise fix the problem, commit, and run `bin/release ci` again.

## Step 5: Check and tag

``` bash
bin/release tag
```

This first runs `bin/release check`, which stops on the first failure:

1.  On `master`, clean working tree, `HEAD` equals `origin/master`.
2.  Tag `v<version>` isn't on `origin`. A local tag that was never pushed is
    reported with the command to delete it.
3.  The `Build & Release` run for `HEAD` on `master` succeeded.
4.  The signing secrets (`MACOS_CERTIFICATE_BASE64`, `MACOS_CERTIFICATE_PASSWORD`,
    `APPLE_API_KEY_BASE64`) and variables (`APPLE_API_KEY_ID`, `APPLE_API_ISSUER_ID`)
    exist in the repository, and immutable releases are enabled
    (Settings → General → Releases). The Hackage credential check from step 2 runs again.
5.  The committed `hledger-flow.cabal` matches `package.yaml`; `cabal check` and `cabal sdist` pass.
6.  **ChangeLog.md is up to date** (`bin/release changelog`):
    - the first section is `## <version>`, matching `package.yaml`
      (so no `## Unreleased` section is left over);
    - that section is not empty;
    - `ChangeLog.md` changed since the previous release tag.

    It prints every commit since the previous tag next to the ChangeLog entry,
    and lists separately any commits made after the last `ChangeLog.md` change,
    such as fixes made while CI was failing. Step 2 reviewed everything before
    those.

If everything passes, it creates the signed tag locally (`bin/tag-release`)
and prints its commit and release notes. Nothing has been pushed yet.

**Review** before pushing the tag: the tagged commit, the release notes, and
any commits listed as made after the last `ChangeLog.md` change. To change
anything, delete the local tag with `git tag -d v<version>`, commit the fix,
and continue from step 4.

## Step 6: Push the tag and build the release

``` bash
bin/release push-tag
```

**Permanent**: the pushed tag is public and starts the release build in CI.

It checks the local tag points at `origin/master`, pushes it, waits for the
tag's `Build & Release` run, and verifies the draft release has all of
`hledger-flow-linux-x64.tar.gz`, `hledger-flow-linux-arm64.tar.gz`,
`hledger-flow-macos-x64.tar.gz`, `hledger-flow-macos-arm64.tar.gz`, and
`hledger-flow-windows-x64.zip`. Like `ci`, it can be re-run to keep waiting, and
reports failed jobs with the commands to inspect and rerun them.

The `release` job attests the build provenance of each binary archive before
uploading it. `bin/release push-tag` downloads each asset and checks with
`gh attestation verify` that it was built by `build.yaml` in this repository
from `refs/tags/v<version>` on a GitHub-hosted runner.

CI enforces the same ChangeLog check: the tag's `release` job fails if the tag,
`package.yaml`, and `ChangeLog.md` disagree. The GitHub release notes are
the macOS download notice in `.github/release-notice.md` followed by the
ChangeLog section (`bin/release notes`).

A failed `Notarize` job is reported as a warning and does not block the
release; follow [Retry notarization](#retry-in-github-actions). Any other failed job stops the command.

The Windows ZIP contains `hledger-flow.exe`; CI checks its `--help` and
`--version` with only Windows system directories on `PATH`. Users still need
`hledger` installed to run imports and reports.

macOS signing is made possible by [FernTrail B.V.](https://ferntrail.tech/)

After verifying the five binaries, `push-tag` dispatches the Hackage candidate
build at the same tag and waits for it. CI attaches the Cabal source archive to
the GitHub draft as `hledger-flow-<version>.tar.gz`. GitHub records its SHA-256.
CI uploads those bytes as the Hackage candidate and checks the download against
the recorded digest. Retries reuse the existing source asset.

**Review** both links printed by `push-tag`: the GitHub draft's notes and
archives, and the candidate page's metadata, README, ChangeLog and file list.
If the candidate build fails, retry just `bin/release hackage`. At this review,
check that the new binaries still meet the platform and library requirements in
the tap formula. If they change, prepare the corresponding tap changes before
publication.

## Step 7: Publish the release and update Homebrew

``` bash
bin/release publish
```

**Permanent**: after the maintainer approves both releases, this command reads
the source checksum from the tagged GitHub release. CI downloads the Hackage
candidate, verifies it against that checksum, and publishes it. The command
checks the public Hackage archive before publishing the GitHub draft as latest,
then verifies GitHub's release attestation. A digest mismatch stops publication.

After GitHub verification, the command reads the four macOS and Linux archive
checksums from the published release and hashes the license from the same tag.
It updates the formula and release references in the tap README, pushes a
commit to `FernTrail/homebrew-tap` on `main`, and waits for the tap’s
`Test formulae` workflow on macOS and Linux. Success means users can upgrade
with `brew update` and `brew upgrade ferntrail/tap/hledger-flow`.

If publication is interrupted, re-run `bin/release publish`. It verifies the
public Hackage and GitHub releases and resumes the tap update. A concurrent
tap push can reject the update. A retry reads the latest tap commit and
preserves the other maintainer’s changes. If the formula already matches,
the command checks CI for the current tap revision. If tap CI fails, inspect the
linked run, fix the tap or rerun the failed jobs, then retry publication.
The command refuses to downgrade a newer formula. Hackage and GitHub releases
remain public while a tap update is being retried.

### Hackage recovery and local fallback

`bin/release hackage` prepares only the candidate. After explicit approval,
`bin/release hackage-publish` can recover just the Hackage publication. Both
use the source asset and checksum from the same GitHub release.

If the Hackage workflow is unavailable, use a clean checkout of the pushed
release tag and prefix the command with `HACKAGE_MODE=local`. The same review
and checksum checks apply. To select this route for the whole release, export
`HACKAGE_MODE=local` before `precheck`. The local check accepts a Hackage
token or a username together with a password or password command in the active
Cabal configuration. The default precheck looks for the repository secret by
name. The candidate upload confirms that Hackage still accepts the credential.

Download counts: `bin/show-release-downloads`.

## Recovery: notarization

### Retry in GitHub Actions

1.  After Apple accepts a submission, CI checks for the online ticket every
    30 seconds for up to five minutes. If it times out, verify the existing
    ticket on the signed executable before resubmitting.
2.  Inspect the failed `Notarize` job's submission ID and Apple log. For a timeout, check the existing submission before retrying.
3.  Confirm the original run is within GitHub's 30-day rerun window and its `signed-hledger-flow-*` artifacts have not expired. Otherwise, use the Mac procedure below.
4.  Re-run only the failed jobs. After publication, never select `Re-run all jobs`.
5.  Confirm both `Notarize` jobs pass: look for `Apple notarization ticket is already available` or a new `Accepted` submission.

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
