# Validation record: prismrepl 0.5.1

Date: 24 November 2026. This record describes the delivered source candidate.
The package has not been submitted to CRAN or published to GitHub.

## Result and scope

The final source archive passed `R CMD check --as-cran --no-manual` on
R 4.6.2, Debian GNU/Linux 13, x86-64: **0 errors, 0 warnings, 1 NOTE**.
The NOTE is **New submission**. The complete suite passed **2,154 assertions
in 234 test blocks**, with no failures, errors, warnings or skips. The check
also ran the installed-package tests and reported 2,154 passes.

External system-clock verification was disabled with
`_R_CHECK_SYSTEM_CLOCK_=FALSE`. Package tests, code checks and CRAN incoming
checks remained enabled. PDF manual generation was omitted; Rd syntax,
metadata, signatures, cross-references, examples and installed documentation
were checked. Final roxygen generation completed without warnings.

Windows/macOS execution and the six native release targets are configured in
the repository. They have not run in this Linux session. Native Windows
clipboard behavior has a configured ConPTY probe; this record does not claim
an observed Windows Terminal or macOS result. The Windows ConPTY probe now
asserts white-on-black and black-on-white cursor transitions, AI-provider
selection with Enter, and an OSC 112 reset when the REPL closes. Local PTY tests verify the emitted sequences,
not the visual behavior of a Windows host.

## Feature verification

| Area                  | Implemented behavior and evidence                                                                                                                          |
| --------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Theme picker          | Current theme initially focused; arrows/mouse focus; Enter applies and saves; Escape/q cancels; search and resize preserve record mapping                  |
| Command options       | Current focus, search, mouse, remapped acceptance, cancellation, saved choices, incomplete-command staging, settings submenus and plain numeric fallback   |
| Theme persistence     | Live PTY verifies cancellation leaves preferences unchanged, acceptance repaints the screen, and the choice survives a fresh R process                     |
| Initial appearance    | Injected Windows/GNOME/fallback tests; live Linux startup resolves and saves a light theme; saved and explicit choices retain precedence                   |
| Preference lifecycle  | Configuration/load make no writes; disabled persistence is honored; settings reset restores all configured defaults                                        |
| AI providers          | Eleven built-ins and custom HTTPS templates; provider validation, encoding, limits, URL safety, persistence and explicit copy                              |
| AI destinations       | Actual error links switch among Mistral, Claude, Gemini and ChatGPT; landing-only providers show the copy/paste hint                                       |
| Rainbow delimiters    | Independent persistent toggle, nested pairs, quote boundaries, tolerant lexing, custom roles and disabled-color behavior                                   |
| Automatic pairing     | Openers, generated-closer skipping, paired Backspace, Unicode, literal paste/history/completion and string/comment/raw-string contexts                     |
| Unicode editing       | Multibyte lexer locations, viewport-bounded token extraction and lexical equivalence; actual input, mouse boundaries and resize                            |
| Session hints         | WORKSPACE/PACKAGES/FILES retain distinct colored hint bars; bounded layout, truncation and command hit targets verified                                    |
| Palette and history   | Search names/descriptions/aliases/groups; sort toggle; keyboard/mouse stages input; all 37 command aliases are unique; colon history retained              |
| Cursor lifecycle      | Contrast derives from the painted editor background, including overrides; main/function editor, color-off, visibility-only changes and cleanup are covered |
| Structural explorer   | Nested objects/attributes, atomic-vector preview leaves, list-column drill-down, precise Unicode columns and blocked active bindings/promises              |
| Conditions            | One labeled block per multiline condition, CRLF normalization, distinct badges and bounded user-call traces                                                |
| Packages and paths    | Literal shortcut calls, installed/CRAN completion, unquoted spaced paths, Windows backslashes and optional quotes/backticks                                |
| Files and docs        | Directory navigation, previews, staged source/cd, styled installed Rd, Unicode and all-occurrence search with next/previous navigation                     |
| Function editor       | Explicit valid save, rejected invalid save, scope/attribute preservation and consistent editing preferences                                                |
| Mouse and lifecycle   | Text/rectangle selection, copy, editable paste, buffered updates, shared alternate screen and exact terminal restoration                                   |

Mistral, ChatGPT, Claude and Perplexity query links are best-effort browser
conventions, without a stable API guarantee. Other built-ins open their
landing page and use the explicit `:explain copy` flow. Tests construct links
locally; they do not send prompts to AI providers.

## Real terminal integration

All ten scripts passed against the installed 0.5.1 code:

- `tools/pty-fullscreen.py`: colors, condition badges, OSC 8 links, scrolling,
  pending input, Unicode mouse placement, Ctrl+J, resize and modal reuse.
  Also covers picker cancellation/search, keyboard and mouse focus, Enter
  acceptance, narrow reflow, full repaint, initial theme saving, typed pairs,
  AI destinations and a fresh process reading saved theme, F4 mapping, AI
  provider and editing preferences. Black/white cursor contrast, color caching,
  hidden-viewer return and configured profile-color restoration also pass.
- `tools/pty-explorer.py`: paired help records, narrow reflow, nested objects,
  data frames, attributes, size toggle, filters, mouse navigation, finite
  atomic-vector leaves, blocked bindings and unchanged inspected data. Numeric
  data-frame columns open directly as previews; repeated Enter/Right keeps their
  breadcrumb unchanged. Nested list columns retain structural navigation.
- `tools/pty-commands.py`: description search, sorting, narrow palette,
  keyboard/mouse staging, explicit function saves, invalid-save preservation,
  :h/:pwd approval, unquoted :so paths and colon history. Assertions wait for
  actual staged input after the modal-to-editor transition.
- `tools/pty-options.py`: AI/provider selection, cancellation without writes,
  search acceptance, staged custom URLs and bindings, saved pipe/mouse/editing
  choices, shortcut disabling, nested settings menus and status multiselect.
  Description-to-record mapping survives an 8-row by 34-column resize.
- `tools/pty-docs.py`: installed documentation styles, repeated and wrapped
  search matches, next/previous focus, copy/paste, F1, resize, Unicode leaves
  and exact table-column offsets.
- `tools/pty-files.py`: crowded sidebar, staged file/folder clicks, navigation,
  blank-space hits, previews, pasted filters, spaced paths, cancelled/approved
  cd/source and rectangular copy/paste.
- `tools/pty-smoke.py`: inline evaluation, Unicode, multiline paste,
  conditions, cancellation, graphemes, pipe insertion, resize and pager exit.
- `tools/pty-basic.py`: basic line input, multiline expressions and Ctrl+D.
- `tools/native/pty-io.py`: buffered UTF-8 output, R-sink bypass, synchronized
  framing, mouse events/modifiers, Ctrl+J, bracketed paste and mouse opt-out.
  Also verifies cached cursor tint, ownership across input close, reset on
  suspension, reapplication, untouched guards and error-unwind cleanup.
- `tools/native/pty-exit.py`: an actual `q("no")` restores the main screen,
  restores the configured cursor color, disables mouse reporting and restores
  every termios field.

The fullscreen suites verify zero rolling redraws and exact terminal-mode
restoration. The file suite uses disposable fake Wayland helpers for the
clipboard transport; the docs suite uses the headless session clipboard.
Neither claims access to a live desktop clipboard. Windows CI enables the
real Unicode clipboard probe with `PRISM_TEST_CLIPBOARD=1`.

The native decoder passed AddressSanitizer and UndefinedBehaviorSanitizer
checks covering key/mouse protocols, all Unicode scalar values and 100,000
malformed input cases. LeakSanitizer was disabled because of the container's
ptrace restrictions. This instruments the pure decoder, not all of R.

## Responsiveness

The fullscreen PTY observed 12 input edits with a 5 ms polling interval:
**median 5.12 ms; maximum 10.27 ms** from injected key to cursor update. This
includes the PTY and observer, cannot reliably resolve sub-5-ms latency, and
does not measure graphical terminal painting.

The installed-package benchmark uses a 132-column by 40-row viewport, with
rainbow colors and automatic pairing enabled. For 4,000 transcript entries:

| Operation                  | Repetitions | Mean elapsed time |
| -------------------------- | ----------- | ----------------- |
| First composed frame       | 5           | 10.40 ms          |
| Cursor-only composition    | 100         | 0.13 ms           |
| Changed editor row         | 50          | 1.18 ms           |
| One-line transcript scroll | 50          | 2.94 ms           |

The writer is instrumented, so these are computation and update-assembly
timings, excluding terminal I/O, emulator paint and editor preparation. Each
emitted frame uses one native writer invocation, with no R `cat()` calls or
R-console flushes. Partial OS writes may require multiple transport writes.
Payload byte counters exclude native cursor-color controls and synchronization
wrappers; cursor tint is cached across unchanged frames.

Separate editor measurements include replacement/cursor movement, geometry,
highlighting, clipping and hit maps. A 2,000-line ASCII fixture averaged
0.21 ms for cursor movement and 47.60 ms for insertion. A 2,000-line Unicode
fixture with Greek names, combining marks, nested calls, quotes and double
brackets averaged 0.25 ms and 73.30 ms respectively. The Unicode lexer uses
stringi match locations and extracts text only for visible tokens and immediate
lookahead. Deterministic tests guard bounded extraction and lexical spans.

Large-buffer insertion still validates input and recomputes token positions.
Timings depend on the host, fixture, cache warmth and scheduling. Current
methodology, raw data and source hashes are in `tools/validation/performance.md`,
`performance.csv` and `performance-provenance.json`.

## Documentation and visual review

README.md and the installed guide describe the current features, defaults,
commands, shortcuts, persistence, AI links and terminal limits. The command
registry and guide agree on all 37 commands. Dependency credits, MIT licensing
and author/maintainer metadata are included. Historical release sections and
NEWS.md are absent.

Eleven figure sets were regenerated from actual R compositors. The black main screen and atomic-column
preview were visually reviewed for contrast, breadcrumbs, values, footer hints,
alignment and clipping. All exports assert that every
row fits its terminal width. The docs figure renders installed base::mean text
with all 14 occurrences of mean highlighted. Figure provenance and the R Core
Team's documentation attribution are in `tools/figures/README.md`.
These rasterized ANSI/cell-width exports are developer renders, not screenshots
of Windows Terminal.

## CRAN and GitHub packaging

The source archive contains R code, registered C routines, Rd manuals, the
installed guide and offline tests. Repository-only tools, workflows, figures
and logs stay in the GitHub bundle. Source archive members are checked against
the final R, C, manual, test and installed documentation files. Compiled objects,
temporary test fixtures and Python bytecode are excluded from source bundles.

The release script builds in an isolated temporary library. A Linux x86-64
binary and manifest were produced for R 4.6; that binary is tied to the build
distribution's ABI. Source installation uses the corresponding platform's
compiler and R dependencies.

GitHub checks cover Linux, Windows and macOS on x86-64 and ARM64, plus R-devel
and the minimum R 4.6 series on Linux. Windows jobs run ConPTY; POSIX jobs run
the terminal suites. Windows ARM64 uses a
[community-supported R distribution](https://contributor.r-project.org/windows-arm64/).
Runner labels and action major versions were checked against the official
[runner images](https://github.com/actions/runner-images) and action releases.
The release workflow requires all native jobs before creating a draft release.

Eleven offline release-manifest tests passed. The gate verifies all six targets,
package/version, R series, architecture, commit/tag consistency, source metadata
and SHA-256 hashes. It rejects stale files, missing targets, mixed builds and
unsafe archive paths. The six-platform workflow has not run in this session.

Before actual submission, add the real repository and issue URLs to DESCRIPTION,
run the hosted platform jobs and complete the manual terminal checklist. The
local result supports a source submission candidate; CRAN acceptance and public
release remain separate actions.

## Reproduction

From the repository after installing dependencies:

```sh
R CMD INSTALL .
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --as-cran --no-manual prismrepl_0.5.1.tar.gz
python3 tools/pty-fullscreen.py
python3 tools/pty-explorer.py
python3 tools/pty-commands.py
python3 tools/pty-options.py
python3 tools/pty-docs.py
python3 tools/pty-files.py
python3 tools/pty-smoke.py
python3 tools/pty-basic.py
python3 tools/native/pty-io.py
python3 tools/native/pty-exit.py
PRISM_NATIVE_SANITIZERS=1 sh tools/native/check.sh
python3 tools/test-release-manifest.py
Rscript tools/build-release.R
```

Finish terminal tests before building if temporary directories are configured
inside the repository. Set `PRISM_R` for a non-default R executable. On Windows,
use `pwsh -NoProfile -File tools/windows-console-smoke.ps1`. The repository
includes build, check, unit, PTY, sanitizer, renderer, benchmark and release-gate
logs in `tools/validation`.
