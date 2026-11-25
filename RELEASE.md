# Release contents and installation

prismrepl provides an optional responsive terminal console for R. Its features
include Unicode editing, syntax and matching-delimiter colors, automatic
pairing, a searchable staged command palette, structural object exploration,
local documentation, a file browser, a small function editor, and labeled
multiline conditions. F1 opens function help at the cursor; F2 shows shortcuts.
Cursor pair highlighting is on by default and can be saved with `:match`. Mouse dragging copies visible text, Ctrl-drag selects a
rectangle, and right-click pastes into the active editor or search field.

Data-frame columns and other atomic vectors open as value previews. Repeated
Enter or Right keeps the explorer on that vector; list columns and attributes
remain structurally navigable. The insertion cursor contrasts with the painted
editor background, including black and white themes, and resets to the terminal
profile's configured color when the fullscreen session ends.

Theme choices, shortcut mappings, editing preferences and AI-provider settings
persist between sessions. The first session detects light or dark desktop
appearance when available. `:theme` opens a selectable list; Enter applies and
saves the focused choice, while Escape cancels. Error-explanation links can use Mistral, ChatGPT,
Claude and other web providers, or a custom HTTPS prompt template. No browser
opens and no prompt is sent automatically.

Option commands such as `:ai`, `:pipe` and `:mouse` open searchable menus when
called without arguments. Up/Down or a mouse click moves focus; Enter accepts
and Escape cancels. `:settings` opens the relevant setting menu. Shortcut and
custom-URL choices place incomplete commands in the input for editing, while
`:status` uses Space to select fields and Enter to apply the batch.

Total resident RAM of the current R process is displayed by default as
`R RAM` in the status line and WORKSPACE module. It is read from native OS
counters once per prompt refresh without garbage collection, subprocesses or
workspace-size traversal. The optional `memory` field separately totals
workspace object sizes; `process_memory` controls the process-wide display.

`:shell COMMAND`, or `:sh COMMAND`, runs a shell command without R-string
quoting. It preserves shell quotes, pipes, redirects and literal backslashes.
Linux and macOS use `system()`; Windows uses `system2()` with `COMSPEC` or
`cmd.exe`. In fullscreen mode, the ordinary terminal is restored while the
command runs; Enter or Ctrl+C returns to the REPL after completion. The exit
status is labeled and available in `.Last.value`. Native shell syntax still
applies, and changing the child directory does not change R's directory.

`:attach` and `:detach` accept multiple package names separated by spaces,
with aliases `:a` and `:de`. Tab completes each package position. Attachment
follows the entered order and stops at the first error; earlier successful
attachments remain. Detachment retains loaded namespaces and protects core
packages.

## Select and install the package archive

For version 0.6.4, the R source-package asset is exactly
**`prismrepl_0.6.4.tar.gz`**. It is produced by `R CMD build` and is the
artifact used for source installation, final package checks and CRAN submission.
Its filename has no platform or architecture suffix. The delivered source archive
is unchanged by the local check tools. Its SHA-256 is:

```text
6433f91dc1e8aaa82990c000806a91317003756c8542696686c9ed1061e92fed
```

| Release item                            | Intended use                                                     |
| --------------------------------------- | ---------------------------------------------------------------- |
| `prismrepl_0.6.4.tar.gz`                | Install from source; check and submit this built archive to CRAN |
| Platform binary and its `.manifest`     | Install on the matching operating system, CPU and R series       |
| Repository ZIP or GitHub source archive | Obtain a source checkout for development and `R CMD build`       |
| `prismrepl-fix-check-flags.R`           | Set up recognition of the selected R build's inherited flag once |
| `prismrepl-check.R`                     | Optionally verify UTC and run a complete audited local check     |

Choose binaries for your operating system, CPU architecture and R major/minor
series. Each binary has a `.manifest` describing its build environment.
`SHA256SUMS.txt` covers the release assets. For source installation, use R 4.5.0
or newer with the matching R development tools and a C compiler, then run:

```r
install.packages(c("cli", "processx", "rlang", "stringi", "utf8"))
install.packages("prismrepl_0.6.4.tar.gz", repos = NULL, type = "source")
prismrepl::repl()
```

On Windows, start Rterm.exe in Windows Terminal. Use `:help`, `:keys`,
`:settings` and `:doctor` to inspect the console. Use `:quit` to return to R.

Windows ARM64 uses a community-supported R distribution. Linux binaries depend
on the build distribution's ABI; install from source on incompatible systems.
Enhanced terminal keys, glyph widths and hyperlinks depend on the terminal.
Linux and macOS clipboard access uses optional system helpers; fallback status is explicit.

## Ubuntu and WSL check prerequisites

A complete check needs tools for PDF generation and HTML validation. On
Ubuntu, including Ubuntu in WSL2, install the standard documentation tools:

```sh
sudo apt-get update
sudo apt-get install texlive-latex-base texlive-latex-recommended texlive-fonts-recommended texinfo tidy qpdf
command -v pdflatex texi2dvi tidy qpdf
```

These tools are needed to check the documentation, not to run the REPL.
If R requests the optional Inconsolata font but its LaTeX files are absent,
the following complete check uses the standard fonts instead:

```sh
R_RD4PDF=times,hyper /usr/bin/R CMD check --as-cran /mnt/c/Users/alek/Downloads/prismrepl_0.6.4.tar.gz
```

The one-time compiler-flag setup is described below. Change the paths to
match your downloads. In WSL, a Windows C: drive
path normally starts with `/mnt/c/`; `c/Users/...` is a relative Linux path.
The `R_RD4PDF` choice changes the manual font and retains PDF generation.
Alternatively, install the Inconsolata LaTeX collection if you want that font.
See R's [manual-building guidance](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Making-the-manuals)
and Ubuntu's [TeX package](https://packages.ubuntu.com/resolute/texlive-latex-base).

An error saying `pdflatex is not available` identifies missing local tooling.
If the log instead says `inconsolata.sty` is missing, LaTeX is running but its
optional font package is absent. Use the complete `R_RD4PDF=times,hyper` command
above, or install that font package; changing R documentation is unnecessary.
Errors naming `ptmr8t`, `ptmb8t`, `ptmri8t` or related Times font metrics need
`texlive-fonts-recommended`, even when `pdflatex` is already installed. Verify
that the active TeX installation can find them:

```sh
kpsewhich ptmr8t.tfm
kpsewhich ptmb8t.tfm
```

If these print nothing after installation, compare `command -v pdflatex
kpsewhich` with `kpsewhich -var-value=TEXMFROOT`. A separate TinyTeX installation
may be taking precedence over Ubuntu's TeX installation. Ubuntu's
[font package file list](https://packages.ubuntu.com/resolute/all/texlive-fonts-recommended/filelist)
identifies the required metrics.

The subsequent `prismrepl-manual.tex` check-directory NOTE can be a leftover
of that failed generation. A missing `tidy` NOTE means HTML validation was
skipped. Install the tools and repeat the complete check in a fresh check
directory; `--no-manual` is only a preliminary development check.

## Make ordinary Ubuntu and WSL checks recognize the inherited flag

If `R CMD check --as-cran` reports `-mno-omit-leaf-frame-pointer`, download
`prismrepl-fix-check-flags.R` and run this explicit setup once. Select the same
R installation that you use for checking. On WSL2, with the downloads on the
Windows C: drive:

```sh
/usr/bin/Rscript --no-init-file --no-site-file /mnt/c/Users/alek/Downloads/prismrepl-fix-check-flags.R --apply
/usr/bin/R CMD check --as-cran /mnt/c/Users/alek/Downloads/prismrepl_0.6.4.tar.gz
```

After successful setup, use your usual `R CMD check --as-cran` command for
subsequent checks. You do not need to run a custom check driver for this flag.
The standalone file contains its helper and requires `processx`, already a
normal prismrepl dependency. From a repository checkout, the equivalent setup
is:

```sh
Rscript --no-init-file --no-site-file tools/fix-check-flags.R --apply
```

### What the one-time setup changes

Ubuntu can supply `-mno-omit-leaf-frame-pointer` in the installed R build's
compiler defaults. The setup tool compares active and build-default `CC`,
`CFLAGS`, `CPPFLAGS` and `CPICFLAGS`, using `R CMD config` with user and site
Makevars disabled for the default values. It also checks a flag embedded in
the compiler command. Ubuntu explains the motivation in its
[frame-pointer policy](https://ubuntu.com/blog/ubuntu-performance-engineering-with-frame-pointers-by-default).

Only a flag proven to come from that R build is added to
`_R_CHECK_COMPILATION_FLAGS_KNOWN_`. This is R's documented mechanism for
recognizing inherited build flags. A flag found only in user or site Makevars
is left for ordinary diagnostics. See
[R Internals](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools).

The persistent change is made in the selected user check environment file,
normally `~/.R/check.Renviron`; the actual path is printed. Explicit
`R_CHECK_ENVIRON` selection and architecture-specific files are respected.
The tool preserves existing profile contents and recognized flags, creates a
backup where appropriate, and verifies the effective result in a fresh
matching R process. Repeated application does not append duplicate entries.
A user profile may be shared by several R installations, so rerun the tool
when switching or rebuilding R to validate its build provenance again.
R documents this check environment file in
[R Internals](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools).

The setup changes no compiler command, optimization flag, Makevars file,
installed R file, package source or source timestamp. Compilation-flag
checking remains enabled; clock and future-file settings are untouched.
The package does not run this tool during installation, loading or console
startup. The setup applies to your local checking environment; CRAN uses its
own R builds and configuration.

For a read-only inspection, or to undo the setup:

```sh
Rscript --no-init-file --no-site-file prismrepl-fix-check-flags.R --inspect
Rscript --no-init-file --no-site-file prismrepl-fix-check-flags.R --undo
```

Use `tools/fix-check-flags.R` instead when working from the checkout. Retain
the reported setup and backup information with your local check records.

## Optional audited check for unavailable remote-clock services

The `unable to verify current time` NOTE is a separate issue. When R's own
clock services are unavailable, the optional `prismrepl-check.R` verifies UTC
independently before running the complete standard `R CMD check --as-cran`.
It includes its helpers and requires the normal `processx` dependency.

On Ubuntu or WSL2:

```sh
/usr/bin/Rscript --no-init-file --no-site-file /mnt/c/Users/alek/Downloads/prismrepl-check.R /mnt/c/Users/alek/Downloads/prismrepl_0.6.4.tar.gz
```

On Windows, this example explicitly selects R 4.5.0 in PowerShell:

```powershell
& "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" --no-init-file --no-site-file "C:\Users\alek\Downloads\prismrepl-check.R" "C:\Users\alek\Downloads\prismrepl_0.6.4.tar.gz"
```

Choose the executable for the R version you intend to check. An optional
final argument selects a new output directory; an existing directory is
refused. From a checkout, `tools/check-package.R` provides the same workflow
with its sibling helpers.

The driver checks examples, tests, PDF documentation and HTML validation.
Its temporary child-process settings and source SHA-256 are recorded in
`preflight.txt` and `preflight.rds`; complete output is in `console.log`, and
R's report is `prismrepl.Rcheck/00check.log`.

**This driver does not persist its settings.** It uses a private temporary
copy of the effective check profile, verifies it in a matching R process,
and removes it on exit. Use the explicit one-time setup above to address
the inherited compiler flag in later ordinary checks. The driver also
recognizes that flag within its own run only when build provenance is verified.
A bare check after the driver exits uses its normal profile and R's own
remote-clock lookup.

### Independent UTC verification

`unable to verify current time` means R's external time lookup did not return
a usable result. It does not establish that the archive contains future
files or that the computer's clock is wrong. R 4.5.0's checker tries
WorldTimeAPI and WorldClockAPI endpoints; the reviewed R 4.6.2 checker has
additional CRAN Date-header and TimeAPI paths. The same source archive can
therefore receive different clock-verification results from the two checkers.

Before launching the check, the driver requests fresh HTTPS Date headers
from CRAN, Google and Cloudflare. At least two independently operated sources
must provide usable, agreeing times. Certificate verification is enabled,
TLS 1.2 or newer is required, redirects are refused, and each request has a
fresh anonymous cache key. Malformed responses are rejected. Stale cache
evidence, inconsistent accepted times, a significant clock jump, or a
local-clock difference reaching the one-minute limit stop the run. An
unavailable or unusable service can be tolerated only when the remaining
sources satisfy the verification requirements.

After this independent check succeeds, the private profile explicitly sets:

```text
_R_CHECK_SYSTEM_CLOCK_=FALSE
_R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE
_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_=5m
_R_CHECK_COMPILATION_FLAGS_=TRUE
```

`_R_CHECK_SYSTEM_CLOCK_=FALSE` replaces the checker's duplicate external lookup
for that child process. The standard check still compares the package's
actual file timestamps with the verified local clock. Future-file checking
keeps R's five-minute allowance, and compilation-flag checking remains active.
No source file or archive timestamp is changed. The driver records these
settings openly and removes its private profile when finished.

If UTC cannot be verified, the driver stops with a failed audit instead of
starting the adjusted check. If the check itself reports a genuine future
file, an unrecognized compiler flag, an error or a warning, the driver does
not declare success. Other NOTEs, including `New submission`, remain visible
and must still be reviewed. These local settings do not control CRAN's own
machines or guarantee acceptance. Retain the audit with the check log and
disclose the local settings in submission comments.

### Additional read-only diagnostics

For compiler configuration, profile selection and optional service
reachability information, use:

```sh
Rscript --no-init-file --no-site-file tools/check-environment.R
Rscript --no-init-file --no-site-file tools/check-environment.R --network
```

The default makes no network requests. The optional network mode reports the
older checker's services as well as newer endpoints. Its CRAN-header request
uses `verify = TRUE` and `TLS = "1.2"`; legacy HTTP endpoints are reachability
diagnostics only. These observations do not replace the independent UTC
preflight. The diagnostic changes no files, clock or settings.

## Build, check and submit the same source archive

CRAN requires a source tarball created by `R CMD build` and a final
`R CMD check --as-cran` run on that tarball. See the
[CRAN submission policy](https://cran.r-project.org/web/packages/policies.html#Submission)
and R's [package-building instructions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-package-tarballs).

**Do not submit a repository ZIP, GitHub's automatic Source code archive, a
platform binary, or a tarball made by manually repacking any of them.** Extract
or check out the repository, make the intended source changes, and run
`R CMD build`. The package builder applies `.Rbuildignore`, prepares the
sources and adds build metadata.

The supplied source archive is already built and can be checked directly
with `R CMD check --as-cran`. Apply the one-time inherited-flag setup first
when the selected R build requires it. Rebuild only when preparing an archive
from an intended source change. A new build has its own timestamp and checksum.

The commands below assume the checkout directory is named `prismrepl` and
its `DESCRIPTION` has `Version: 0.6.4`. Run them from the checkout's parent
directory, using the current R release or R-patched. Replace the directory
name if your checkout has another name:

```sh
R CMD build prismrepl
python3 prismrepl/tools/check-source-archive.py prismrepl_0.6.4.tar.gz --description prismrepl/DESCRIPTION
R CMD check --as-cran prismrepl_0.6.4.tar.gz
```

The corresponding PowerShell commands use R's executable explicitly:

```powershell
R.exe CMD build prismrepl
py -3 .\prismrepl\tools\check-source-archive.py .\prismrepl_0.6.4.tar.gz --description .\prismrepl\DESCRIPTION
R.exe CMD check --as-cran .\prismrepl_0.6.4.tar.gz
```

Continue only after each command succeeds. The archive gate verifies a
nonempty `Packaged` build timestamp in the archived `DESCRIPTION`, required
package sources, the expected package root, and exclusion of repository-only
files such as `README.md`. It prints the build timestamp and SHA-256 checksum.
The archived package name, version and title must
match the intended release. `Packaged` belongs to the generated archive;
do not add it to the checkout or insert a timestamp into a manually made tarball.

Keep `prismrepl.Rcheck/00check.log` with the archive and its SHA-256. When
using the optional audited driver, retain its `preflight.txt` as well. Review
every error, warning and note before submission. The final
check includes the manual; `--no-manual` is useful for a preliminary local
check when TeX is unavailable, but does not replace that final check.

Upload the **same unchanged `prismrepl_0.6.4.tar.gz` file** whose check log
and checksum you reviewed. If source code, tests, documentation, `DESCRIPTION`
or build rules change, repeat the build, archive gate and check, then use the
new archive and checksum. Keep the repository ZIP for GitHub and development.

## Maintainer checks before publishing this draft

- Confirm all six native build jobs and their terminal checks passed.
- Confirm the tag, source archive, binary names and manifests agree with
  `DESCRIPTION`, the R series and the checked-out commit.
- Run `tools/check-source-archive.py` on the exact source tarball and retain
  its checksum with the final check log.
- Inspect the check logs and complete the manual terminal checklist.
- Test installation of the resulting archives in clean R libraries.
- Verify that the project URL in `DESCRIPTION` still resolves to the intended
  repository. Rebuild and recheck after any package metadata change.
- Record completed platform tests in `VALIDATION.md`.
- Publish the draft after reviewing the binaries and logs.

The workflow verifies the six-target artifact set, rejects mixed or stale
artifacts, writes checksums and creates a draft GitHub Release. Manual dispatch
builds downloadable workflow artifacts without publishing a release. Select
`release`, `4.5` or `4.5.0` in the manual workflow's R-version field. The six jobs
all use that same R series, and each binary manifest records the exact build.
Tagged release builds use the current R release.
