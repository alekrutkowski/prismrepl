# Validation of prismrepl 0.6.5

Validated on 27 November 2026. The results below apply to the exact delivered
source archive. The GitHub ZIP includes the source, current README, release
instructions, workflows, developer tools and the evidence referenced here.

## Exact source archive

- File: `prismrepl_0.6.5.tar.gz`.
- Built with `R CMD build` using R 4.6.2.
- Build timestamp: `2026-11-27 10:33:02 UTC`.
- Source files: 131, with no compiled objects or repository-only files.
- SHA-256: `f25b8b9773dbd9633ba4392a24c9f4cd31fde151bbf6cc97ae7c1e9e5db4635c`.

The [build log](tools/validation/R-CMD-build.log) and
[source archive gate](tools/validation/source-archive.log) identify this file.
All 130 source files other than DESCRIPTION match the development source byte
for byte. R adds build metadata to DESCRIPTION. The archive remains unchanged
after checking; repository-only documentation and evidence are excluded by
`.Rbuildignore`.

## Complete final-archive checks

Both runs used Linux x86-64, Debian 13, and rlang 1.3.0. Both included examples,
compiled-code checks, the full test suite, PDF generation and HTML validation.
The manual used `R_RD4PDF=times,hyper`, with PDF checking enabled.

| Result                       | R 4.5.0, audited clock | R 4.6.2, ordinary check |
| ---------------------------- | ---------------------- | ----------------------- |
| Errors                       | 0                      | 0                       |
| Warnings                     | 0                      | 0                       |
| NOTEs                        | 1: New submission      | 1: New submission       |
| Passed test assertions       | 3,610                  | 3,610                   |
| Test warnings or failures    | 0                      | 0                       |
| Skipped tests                | 0                      | 0                       |
| Future file timestamps       | OK                     | OK                      |
| Compilation flags            | OK                     | OK                      |
| Examples                     | OK                     | OK                      |
| PDF manual                   | OK                     | OK                      |
| HTML manual                  | OK                     | OK                      |
| Check directory and detritus | OK                     | OK                      |

Evidence:

- R 4.5.0: [00check.log](tools/validation/R-4.5.0-audited/00check.log),
  [test results](tools/validation/R-4.5.0-audited/testthat.Rout),
  [compilation](tools/validation/R-4.5.0-audited/00install.out),
  [clock and environment audit](tools/validation/R-4.5.0-audited/preflight.txt).
- R 4.6.2: [00check.log](tools/validation/R-4.6.2-ordinary/00check.log),
  [test results](tools/validation/R-4.6.2-ordinary/testthat.Rout),
  [compilation](tools/validation/R-4.6.2-ordinary/00install.out).

The NOTE contains the maintainer identity and `New submission`. These local
checks do not constitute a CRAN acceptance decision.

### Clock and compiler settings

The R 4.6.2 run was an ordinary `R CMD check --as-cran`, without the custom
check driver or a clock override. The R 4.5.0 run used
[tools/check-package.R](tools/check-package.R). It first verified current UTC
against fresh, certificate-verified HTTPS responses from CRAN, Google and
Cloudflare. It then set the following values in a temporary child check profile:

```text
_R_CHECK_COMPILATION_FLAGS_=TRUE
_R_CHECK_SYSTEM_CLOCK_=FALSE
_R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE
_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_=5m
```

This replaces R 4.5.0's external-clock lookup for that check only. The actual
future-file comparison and compilation-flag checking remain enabled. The audit
records the accepted time observations, effective settings, source checksum and
exit status. No source timestamps, compiler flags, installed R files or user
profiles were changed by this run.

Neither R build used here supplies `-mno-omit-leaf-frame-pointer`; consequently
no flag was added to the checker's known-flags list. The explicit Ubuntu/WSL
setup tool remains included in [tools/fix-check-flags.R](tools/fix-check-flags.R).
Its use and the distinction between local checker configuration and package
code are documented in [RELEASE.md](RELEASE.md). Installing this package does
not apply that setup or modify CRAN's checking environment.

## Custom-command coverage

The final full suite includes 347 assertions specific to custom commands:

| Area                        | Assertions | Coverage                                                                                    |
| --------------------------- | ---------- | ------------------------------------------------------------------------------------------- |
| String argument parser      | 157        | Zero, one, multiple and variadic arguments; grouping; empty strings; Unicode; literal paths |
| Registration and storage    | 102        | Names, aliases, collisions, serialization, closure state, locks, failed writes and recovery |
| Console integration         | 88         | Dispatch, conditions, visibility, completion, palettes, restart and directory stability     |

These assertions verify that a one-formal function receives the complete
remaining text with internal spaces preserved. Multiple-formal and variadic
functions receive literal character scalars split on whitespace, with quoted
strings preserving internal spaces. Numeric-looking values, `NA`, `$HOME` and
R-looking text remain strings. Backslashes remain literal, including quoted
Windows directory paths ending in a backslash.

Fresh matching R subprocesses verify that a serialized closure retains its
captured local values while references to `.GlobalEnv` use the new session's
workspace. Listing and completion inspect metadata without restoring function
bodies. Tests also cover explicit replacement/removal, separate registries,
session-only definitions, malformed storage, unavailable writer locks and
retention of the previous file after failed saves.

Console tests verify normal visible/invisible results, `.Last.value`, multiline
message/warning/error blocks and the submitted colon text in diagnostic context.
The external-command path restores the terminal and screen after success,
error or interruption. A relative registry path is fixed when creating the
configuration; changing directory through `:cd` does not silently select a
different registry.

## Live terminal verification

[tools/pty-custom-commands.py](tools/pty-custom-commands.py) ran against the
installed package from the final R 4.5.0 archive check. Its
[log](tools/validation/pty-custom-commands-R4.5.0.log) records a passing run.
The test uses two separate R processes and a real Linux pseudo-terminal.
It verifies saved definitions, captured values, aliases, Tab completion,
zero/one/multiple/variadic arguments, Unicode, quoted and empty strings,
description-based palette search, staging before final Enter, condition blocks,
error recovery and colon-command history. It also checks that the alternate
screen does not scroll and that terminal modes are restored exactly.

Both GitHub workflows run this test in their POSIX terminal-test step.
Windows console tests remain configured separately.

## Isolation, documentation and scope

The full test harness redirects both `PRISMREPL_CONFIG_FILE` and
`PRISMREPL_COMMANDS_FILE` to temporary locations, then restores inherited
settings. The [isolation probe](tools/validation/settings-isolation.log)
checks that the original sentinel files remain unchanged. Package loading
creates no custom registry and executes no custom function. Registration and
removal are explicit user operations; examples use temporary paths.

Roxygen documentation is generated for the new public functions and updated
configuration/completion arguments. The README and installed guide contain the
same custom-command tutorial. Reproducible renderer captures identify version
0.6.5; see [tools/figures](tools/figures). The release introduces no additional
runtime dependencies, and the native C sources are unchanged.

The public base snapshot used for this work is commit
`d82c2c712f8454143da298d0cc7896cdcba43037` of
[alekrutkowski/prismrepl](https://github.com/alekrutkowski/prismrepl).

Execution reported here was Linux x86-64. Native Windows, WSL2, macOS, ARM64
and R-devel runs are not claimed. The supplied GitHub workflows retain checks
and release builds for their configured platforms, including the R 4.5.0
Windows compatibility job. They must run in GitHub before their results can be
reported. The GitHub ZIP is a source checkout, not a precompiled Windows binary.
