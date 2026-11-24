## New submission

prismrepl is an explicitly started interactive terminal console for R, with
Unicode editing, completion, searchable history, structured conditions,
workspace/file exploration, installed documentation, a function editor and
configurable browser links for error explanations. Its responsive interface
provides matching-delimiter colors, automatic pairing, selectable themes,
persistent preferences and OS appearance detection for an initial theme.

Loading the package starts no terminal, writes no preferences and accesses
no network. Configuration reads a bounded data-only preference file and can
read local desktop appearance settings. An initial resolved theme is saved
only when the console starts with persistence enabled. Explicit preference
commands and the settings API save user-requested choices.

CRAN package-name downloads start after an explicit completion or refresh
request and run outside the input loop. Examples use temporary paths and
guard interactive entry points. Tests run offline without opening browsers
or native terminal sessions; separate developer/CI scripts exercise real
terminals. AI links are constructed locally, with no automatic transmission.
Clipboard copying requires an explicit user action.

The native binding inspector uses the documented R_GetBindingType API, so
R >= 4.6.0 is declared. Native routines are registered and symbol-based.

## Locally completed environment and checks

- R 4.6.2 on Debian GNU/Linux 13, x86-64.
- R CMD check --as-cran --no-manual: 0 errors, 0 warnings, 1 NOTE.
- NOTE: New submission.
- 2,154 passing assertions in 234 test blocks; no failures, warnings or skips.
- Ten real POSIX terminal suites passed, including theme selection and saved
  preferences across a fresh R process, atomic data-frame column leaves,
  theme-aware cursor tint and cursor cleanup on exit. Selectable option
  menus, command staging, cancellation and saved choices also pass.
- Native decoder AddressSanitizer/UndefinedBehaviorSanitizer checks passed.
- Eleven offline release-manifest checks passed.
- A Linux x86-64 R 4.6 binary was built in an isolated temporary library.
- Windows ConPTY and six native platform builds are configured in CI;
  Windows/macOS execution remains pending.

External system-clock verification was disabled. Package tests, code checks
and incoming checks remained enabled. PDF manual generation was omitted;
Rd documentation, signatures, cross-references and examples were checked.
LeakSanitizer was disabled because the container restricts ptrace;
AddressSanitizer and UBSan remained enabled for the native decoder.

## Before submission

Add the actual repository and issue URLs to DESCRIPTION, run the configured
release/R-devel checks, record Windows/macOS results, and complete the manual
terminal checklist. CRAN submission has not been performed.
