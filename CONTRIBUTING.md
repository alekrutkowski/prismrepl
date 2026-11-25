# Contributing

Use R 4.5.0 or newer and a matching C toolchain. Keep runtime dependencies small
and avoid accessing undocumented R internals. Regenerate exported help pages
with `roxygen2::roxygenise()` after changing the documented API.

Run `testthat::test_local()` while developing. For the final package check,
build the source archive and check that exact file. For version 0.6.4, run
these commands from the parent of a checkout named `prismrepl`:

```sh
R CMD build prismrepl
python3 prismrepl/tools/check-source-archive.py prismrepl_0.6.4.tar.gz --description prismrepl/DESCRIPTION
R CMD check --as-cran prismrepl_0.6.4.tar.gz
```

Use the checkout's actual directory name and the version in `DESCRIPTION`.
On Windows, use `R.exe` and `py -3`; [RELEASE.md](RELEASE.md) contains the
PowerShell sequence, installation instructions and checksum steps. A final
submission check includes the manual; `--no-manual` is a preliminary local
convenience.
Ubuntu and WSL users can install the documentation prerequisites listed in
[RELEASE.md](RELEASE.md#ubuntu-and-wsl-check-prerequisites).

The archive gate checks the generated `Packaged` timestamp, package root,
required sources and repository-only exclusions, including `README.md`.
Do not add `Packaged` to the source `DESCRIPTION` yourself. Do not rename or
manually re-tar a GitHub source ZIP for CRAN. Extract the checkout and let
`R CMD build` create `prismrepl_VERSION.tar.gz`. Check, retain and submit
that same unchanged file; repeat all three commands after any source or
metadata edit. This follows the
[CRAN submission policy](https://cran.r-project.org/web/packages/policies.html#Submission).

The GitHub workflow repeats checks across supported operating systems and
architectures. `tools/pty-smoke.py` tests an installed package in a real POSIX
terminal; `tools/native/check.sh` exercises the decoder with sanitizers.
Python is used by the archive gate and these developer scripts.
After installing the current checkout, `Rscript tools/test-cran-worker.R`
checks actual background-worker success, failure, cancellation, timeout,
repository switching, unload and normal R exit against a temporary local
repository. Both workflows run this offline probe on each platform.

Tests must not rely on the network, create files outside temporary directories,
open a browser, or start the interactive console on CRAN. Interactive terminal
checks belong in the separate CI/developer scripts.
`tests/testthat/setup-settings.R` scopes the temporary preference override to
`testthat::teardown_env()`, so it survives every test file and restores the
caller's environment after the suite. Pure configuration fixtures should use
`settings_file = NULL`; persistence fixtures should use their own temporary
paths. `Rscript tools/test-settings-isolation.R` checks setup lifetime across
files, cleanup, and preservation of an inherited preference file.

For terminal bug reports, include `prismrepl::repl_diagnostics()`, terminal
name/version, a small reproducible expression, and whether the problem occurs
in `mode = "basic"`. Remove private paths or source text before sharing.
Contributions are accepted under the project's MIT licence.
