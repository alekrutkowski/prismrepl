# Validation of prismrepl 0.6.4

Validation date: 25 November 2026. The package supports R >= 4.5.0.
No CRAN submission or GitHub publication was performed.

## Exact source archive

- File: `prismrepl_0.6.4.tar.gz`.
- Built with R 4.6.2 using `R CMD build`.
- Archived `Packaged`: `2026-11-25 15:29:23 UTC; agent`.
- Source gate: 124 files, required metadata and package structure present,
  repository-only files excluded.
- SHA-256: `6433f91dc1e8aaa82990c000806a91317003756c8542696686c9ed1061e92fed`.

[Build log](tools/validation/R-CMD-build.log) and
[source-archive verification](tools/validation/source-archive.log).
The complete checks below used these exact bytes. The GitHub ZIP contains the
corresponding source, workflows, figures, developer tools and evidence.
Developer tools, validation records and submission comments are excluded from
the CRAN source archive by `.Rbuildignore`.

## Complete package checks

Both recorded checks ran on Debian GNU/Linux 13, x86-64, with rlang 1.3.0.
The R 4.5.0 check used the supplied audited UTC driver. The R 4.6.2 check used
ordinary `R CMD check --as-cran` with a private R installation whose compiler
defaults reproduce Ubuntu's inherited leaf-frame flag. These configurations
are described below.

| Result                       | R 4.5.0, audited UTC | R 4.6.2, inherited flag |
| ---------------------------- | -------------------- | ----------------------- |
| Errors                       | 0                    | 0                       |
| Warnings                     | 0                    | 0                       |
| NOTEs                        | 1                    | 1                       |
| Remaining NOTE               | New submission       | New submission          |
| Passed test assertions       | 3,263                | 3,263                   |
| Test failures/warnings/skips | 0 / 0 / 0            | 0 / 0 / 0               |
| Compilation flags            | OK                   | OK                      |
| Future file timestamps       | OK                   | OK                      |
| PDF and HTML manuals         | OK                   | OK                      |
| Checker exit status          | 0                    | 0                       |
| Source archive unchanged     | Yes                  | Yes                     |

Evidence for [R 4.5.0](tools/validation/R-4.5.0-audited/prismrepl.Rcheck/00check.log):
[audit](tools/validation/R-4.5.0-audited/preflight.txt),
[installation](tools/validation/R-4.5.0-audited/prismrepl.Rcheck/00install.out),
[tests](tools/validation/R-4.5.0-audited/prismrepl.Rcheck/tests/testthat.Rout).
Evidence for [R 4.6.2](tools/validation/R-4.6.2-inherited/prismrepl.Rcheck/00check.log):
[build configuration](tools/validation/R-4.6.2-inherited/inherited-build.txt),
[installation](tools/validation/R-4.6.2-inherited/prismrepl.Rcheck/00install.out),
[tests](tools/validation/R-4.6.2-inherited/prismrepl.Rcheck/tests/testthat.Rout).

The compiler was Debian GCC 14.2.0. Documentation used standard Times fonts
through `R_RD4PDF=times,hyper`, LaTeX, texinfo, HTML Tidy and qpdf. Manual checks
were enabled. Native Windows, WSL2, macOS, ARM64 and R-devel execution is not
claimed. Their configured GitHub jobs are not represented as completed checks.

An additional [ordinary R 4.5.0 check](tools/validation/R-4.5.0-ordinary/prismrepl.Rcheck/00check.log)
passed all 3,263 assertions and both manuals, with zero errors and warnings.
It retained two NOTEs: New submission and `unable to verify current time`.
The package-test fix therefore also passes without the optional UTC driver;
the older external-time lookup remains a separate environment limitation.

## R 4.5 binding inspection

The fallback distinguishes directly missing bindings from forced promises
whose cached value is R's missing-argument sentinel. A reproduction with 300
different global binding-name patterns exposed the environment-cloning problem
under rlang 1.3.0. The correction passed all patterns under both rlang 1.1.5
and 1.3.0.

Exported, vectorized rlang helpers identify delayed bindings. Active and
unforced bindings are excluded before values are read. For rare cached
missing-value candidates, the supported `R_getVar()` and `R_tryCatchError()`
APIs distinguish the cases without cloning an environment. Unrelated errors
are rethrown. The R >= 4.6 `R_GetBindingType()` path is unchanged. No
undocumented C API or object-payload copying was introduced.

The focused native tests passed 71 assertions in each combination:

| R version | rlang version | Failures | Warnings | Skips |
| --------- | ------------- | -------- | -------- | ----- |
| 4.5.0     | 1.1.5         | 0        | 0        | 0     |
| 4.5.0     | 1.3.0         | 0        | 0        | 0     |
| 4.6.2     | 1.3.0         | 0        | 0        | 0     |

The new assertions fail against the unfixed implementation with rlang 1.3.0.
They also pass in both complete final-archive checks. See the
[native regression evidence](tools/validation/binding).

The final archive was installed into an isolated R 4.5.0 library with rlang
1.3.0. A real pseudo-terminal explorer test passed special-binding protection,
nested exploration, atomic leaves, filtering, Unicode, resizing and terminal
restoration. Its [log](tools/validation/terminal/pty-explorer-R4.5.0.log) is
included. Renderer-produced figures were refreshed, including the batch
attachment entry in the command palette.

## Ordinary checks with the Ubuntu compiler flag

The standalone `prismrepl-fix-check-flags.R` is assembled from
[check-profile.R](tools/check-profile.R) and
[fix-check-flags.R](tools/fix-check-flags.R). It verifies the selected R build's
compiler defaults with user and site Makevars disabled. Only an inherited
`-mno-omit-leaf-frame-pointer` is added to the selected persistent check
environment's `_R_CHECK_COMPILATION_FLAGS_KNOWN_` setting. Compilation-flag
checking stays enabled. R documents this use in
[R Internals, Tools](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools).

The tool preserves the effective profile and existing known flags, writes a
backup, verifies the result in a fresh matching R process, and supports
inspection and reversal of its unchanged block. It changes no compiler
commands, Makevars, source timestamps, clock settings or installed R files.
Package installation and loading never invoke it. Rerun the tool when changing
R builds because a user profile can be shared by several installations.

For the final full-package check, the interpreter and libR were byte-identical
copies of installed R 4.6.2. Only the private launcher's path and private
Makeconf's default C flag variables were adjusted. The leaf-frame flag was
added to those defaults to reproduce Ubuntu's inherited setting. The explicit
repair was applied to a private profile selected through `R_CHECK_ENVIRON`.
No live R installation or actual user profile was changed.

The subsequent command was ordinary `R CMD check --as-cran`. The selected
profile contained only the verified inherited-flag setting and
`_R_CHECK_COMPILATION_FLAGS_=TRUE`. There was no custom driver or clock,
timestamp or incoming-check suppression. The flag remained in all five C
compilation commands; the compilation-flags section reported `OK`.

A small compiled fixture separately checked diagnostic preservation:

| Fixture case                                           | Observed ordinary check result                     |
| ------------------------------------------------------ | -------------------------------------------------- |
| Inherited leaf-frame plus package-added `-Wconversion` | Both flags reported before repair                  |
| Same source and compiler commands after repair         | `-Wconversion` and its Makevars WARNING retained   |
| Inherited leaf-frame only, after repair                | Compilation flags `OK`; overall `Status: OK`       |

Before/after compiler commands were byte-identical. Remote incoming feasibility
was disabled for this deliberately unsubmitted fixture only. Compiler, clock,
file timestamp and manual checks remained enabled.
[Fixture evidence](tools/validation/compiler-flags/native-summary.txt) and
check/installation logs are included. The helper's offline regression tests
passed on R 4.5.0 and R 4.6.2 and are included in the GitHub check workflow.

## Audited R 4.5 clock verification

The optional `prismrepl-check.R` is assembled from
[check-clock.R](tools/check-clock.R), [check-profile.R](tools/check-profile.R)
and [check-package.R](tools/check-package.R). It runs the complete check after
independent UTC verification and does not persist its settings.

For the recorded R 4.5.0 run, fresh certificate-verified HTTPS responses from
CRAN, Google and Cloudflare agreed with the local clock. The verifier requires
two independent agreeing sources, rejects stale or inconsistent responses,
and detects wall-clock jumps using an elapsed clock. Only after verification
does the private child profile apply:

```text
_R_CHECK_SYSTEM_CLOCK_=FALSE
_R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE
_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_=5m
_R_CHECK_COMPILATION_FLAGS_=TRUE
```

R's duplicate remote-time lookup is replaced by the recorded preflight. The
actual future-file timestamp check remains enabled. The driver preserves the
effective original profile, verifies child settings in a matching R process,
checks the source hash before and after, and removes its private profile on
exit. That R 4.5 build lacked the leaf-frame flag, so no additional known flag
was introduced.

These are disclosed local settings. A later ordinary check still uses R's
own remote-time lookup. CRAN uses its own builds and configuration. Neither
tool changes the submitted package or guarantees CRAN acceptance.

## Batch package commands

`:attach` and `:a` accept multiple package names. Literal namespaced
`base::library()` calls run in the supplied order, with repeated names removed.
A loading error stops the sequence; earlier successful attachments remain.
Single-package calls, invisible returns and `.Last.value` semantics are
preserved. No package is installed automatically.

The existing multi-package `:detach` and `:de` retain confirmation, core-package
protection and per-package results. Namespaces remain loaded. There is no
`:detatch` command or alias. Tab completion works at each package argument,
and complete commands remain in history.

Fresh R 4.5.0 and R 4.6.2 installations each passed 175 focused assertions
across batch, argument and palette tests, including 58 dedicated batch
assertions. Tests cover literal-safe arguments, masked function names, order,
deduplication, failures, completion and history. These also pass in the
complete final-archive checks. [Focused logs](tools/validation/batch-packages)
are included.

## Static diagnostics and release boundaries

The supplied checktor report was reviewed against the source. Remaining
output, package-management and directory-change findings describe intentional
interactive commands or captured previews. Git and clipboard subprocess calls
are bounded and use quoted arguments or stdin. NEWS is optional and remains
omitted. The [assessment](tools/validation/checktor-assessment.md) explains
each category without presenting the heuristic scanner as a CRAN gate.

The GitHub bundle retains OS/architecture checks, R 4.5.0 jobs and binary
release workflows. The CRAN archive contains source, manual pages, the
installed guide and tests, without compiled binaries, developer logs, release
ZIPs or local check helpers. Only the built tarball is intended for CRAN upload.
