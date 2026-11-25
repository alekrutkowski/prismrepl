## Resubmission

prismrepl 0.6.4 supports R >= 4.5.0. The R 4.5 binding inspector now
distinguishes a directly missing binding from a forced promise containing
the missing-argument sentinel without cloning an environment. It uses
exported rlang helpers and supported R C APIs, excludes active and delayed
bindings before reading values, and retains the optimized R >= 4.6 path.
Native routines are registered.

The regression was reproduced with rlang 1.3.0. Focused tests pass on
R 4.5.0 with rlang 1.1.5 and 1.3.0 and on R 4.6.2 with rlang 1.3.0.
Batch :attach preserves single-package behavior and stops at the first
error. Existing batch :detach behavior and protections remain.

## Complete checks of the source archive

The archive was produced by R CMD build under R 4.6.2. Its SHA-256 is:
6433f91dc1e8aaa82990c000806a91317003756c8542696686c9ed1061e92fed

This exact archive completed R CMD check --as-cran, including PDF and HTML
manuals, on Debian GNU/Linux 13, x86-64:

- R 4.5.0 with rlang 1.3.0, audited UTC configuration: 0 errors,
  0 warnings, 1 NOTE (New submission).
- R 4.6.2 with rlang 1.3.0, inherited-flag configuration below:
  0 errors, 0 warnings, 1 NOTE (New submission).

Each suite passed 3,263 assertions with no failures, warnings or skips.
Compilation flags and future file timestamps reported OK. The archive's
SHA-256 was unchanged after checking. Full logs are retained.

An additional ordinary R 4.5.0 check also passed all 3,263 assertions and
both manuals, with no errors or warnings. It retained New submission and
unable to verify current time NOTEs; the package test fix does not depend
on the UTC driver.

## Local checker configuration

The R 4.6.2 run used a private copy of the installed runtime. Its interpreter
and libR were byte-identical; its launcher path and private Makeconf were
adjusted to reproduce Ubuntu's -mno-omit-leaf-frame-pointer in R's build
defaults. The flag remained in all five package C compilation commands.

After verifying provenance with R CMD config without user or site Makevars,
a separate development tool added the inherited flag to
_R_CHECK_COMPILATION_FLAGS_KNOWN_ in a private selected check environment.
This follows the documented R Internals control for inherited build flags.
_R_CHECK_COMPILATION_FLAGS_=TRUE remained enabled. The subsequent command
was ordinary R CMD check --as-cran, with no custom driver, clock suppression
or incoming-check suppression. A separate fixture confirmed that an unrelated
package-added -Wconversion remains reported. The package supplies no
leaf-frame flag and no Unix CFLAGS override.

The R 4.5.0 run used the audited local driver because the older checker's
external clock lookup was unavailable. Fresh certificate-verified HTTPS
responses from CRAN, Google and Cloudflare agreed with the local UTC clock.
Only after that preflight did a temporary child profile set
_R_CHECK_SYSTEM_CLOCK_=FALSE. The actual timestamp check stayed enabled
through _R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE with five-minute leeway.
Compilation-flag checking remained enabled. That R build lacked the
leaf-frame flag, so no additional known flag was introduced.

These local configurations are disclosed. Both development tools and these
comments are excluded from the source archive. Package installation, loading
and tests never run the repair or change checker profiles. The tools do not
modify compiler commands or source timestamps or influence CRAN's settings.

Native validation here was Linux x86-64. Windows, WSL2, macOS, ARM64 and
R-devel execution is not claimed. Cross-platform jobs remain configured
in GitHub Actions. See VALIDATION.md for the archive and evidence scope.

## Intentional interactive behavior

The REPL starts only when the user calls repl(). Loading does not start a
console or alter terminal, history or startup files. Optional preferences
and history writes follow explicit console use.

The :attach, :install and :installgh commands perform user-requested actions.
No software is installed on load, in examples or tests. The :shell command
also requires an explicit submitted command; its tests use small local
fixtures without network or installation. The :cd command intentionally
changes R's working directory. Preview print output is captured as text;
terminal prompts and display output are intentional. Git and clipboard
helpers use bounded execution and appropriate quoting or stdin. AI links
open only on explicit user request.

NEWS is optional and omitted. This file is excluded by .Rbuildignore.
Relevant contents can be pasted into the resubmission form's Optional
comment field.
