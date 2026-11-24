# Contributing

Use R 4.6 or newer and a matching C toolchain. Keep runtime dependencies small
and avoid accessing undocumented R internals. Regenerate exported help pages
with `roxygen2::roxygenise()` after changing the documented API.

Run `testthat::test_local()` and `R CMD check --as-cran --no-manual` on a built
source archive. The GitHub workflow repeats checks across supported operating
systems and architectures. `tools/pty-smoke.py` tests an installed package in
a real POSIX terminal; `tools/native/check.sh` exercises the decoder with
sanitizers. Python is only needed for these developer scripts.

Tests must not rely on the network, create files outside temporary directories,
open a browser, or start the interactive console on CRAN. Interactive terminal
checks belong in the separate CI/developer scripts.

For terminal bug reports, include `prismrepl::repl_diagnostics()`, terminal
name/version, a small reproducible expression, and whether the problem occurs
in `mode = "basic"`. Remove private paths or source text before sharing.
Contributions are accepted under the project's MIT licence.
