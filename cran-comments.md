## Submission

prismrepl 0.6.5 supports R >= 4.5.0. It provides an explicitly started terminal
console with structured output, editing, completion, documentation and object
inspection. Persistent custom colon commands can be registered as R functions;
arguments are passed as literal scalar character strings. Saved definitions
are restored only when the user invokes the command. Metadata-only help and
completion do not execute command bodies.

Project URL: https://github.com/alekrutkowski/prismrepl

## Exact source and complete checks

The submitted artifact is the source tarball produced by R CMD build:

prismrepl_0.6.5.tar.gz
SHA-256: f25b8b9773dbd9633ba4392a24c9f4cd31fde151bbf6cc97ae7c1e9e5db4635c

On 27 November 2026, this exact archive passed complete --as-cran checks on
Linux x86-64, Debian 13, using R 4.5.0 and R 4.6.2 with rlang 1.3.0.
Each run reported 0 ERRORs, 0 WARNINGs and 1 NOTE, containing the maintainer
identity and New submission. Each test run passed 3,610 assertions with no
failures, warnings or skips. Examples, PDF and HTML manuals, future-file
timestamps and compilation flags all checked OK. PDF generation remained
enabled, using R_RD4PDF=times,hyper.

R 4.6.2 used ordinary R CMD check --as-cran. For R 4.5.0, an explicit local
check driver verified UTC independently from fresh HTTPS Date responses from
CRAN, Google and Cloudflare. It then set _R_CHECK_SYSTEM_CLOCK_=FALSE in a
private child-process profile, while keeping
_R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE,
_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_=5m and
_R_CHECK_COMPILATION_FLAGS_=TRUE. The audit and logs are provided in the
repository's tools/validation directory. The driver changed no source
file timestamp, compiler flag or persistent user configuration.

Neither R build in these runs inherited -mno-omit-leaf-frame-pointer; no
known-flag exception was added. Optional local diagnostic/setup tools are
excluded from the CRAN source tarball and are never run by installation,
loading, examples or package tests.

Native execution here was Linux x86-64. Windows, WSL2, macOS, ARM64 and
R-devel execution is not claimed. Their configured GitHub jobs require
separate execution. See VALIDATION.md for evidence and scope.

## Intentional interactive behavior

The console starts only after an explicit call to repl(). Loading does not
start a console or alter terminal, history, preference or startup files.
Persistent preferences/history follow console use. Custom command files are
written only by explicit registration or removal; the default registry is
separate from the data-only preference file. Examples and tests use temporary
registries, with environment overrides restored on exit.

The :attach, :install and :installgh commands perform user-requested package
operations. No software is installed on load, in examples or tests. The :shell
command and custom R commands also require explicit invocation. The :cd command
intentionally changes the working directory. Preview print output is captured
as text; input prompts and terminal output are part of the console's purpose.
Git and clipboard helpers use bounded execution and quoted arguments or stdin.
AI links require explicit use and do not transmit prompts during package load.

No new runtime dependency is added by custom commands. The native C sources
are unchanged. NEWS is optional and omitted. These comments and validation
records are excluded by .Rbuildignore; relevant text can be included in the
submission form's Optional comment field.
