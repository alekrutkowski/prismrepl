# Release contents and installation

prismrepl provides an optional responsive terminal console for R. Its features
include Unicode editing, syntax and matching-delimiter colors, automatic
pairing, a searchable staged command palette, structural object exploration,
local documentation, a file browser, a small function editor, and labeled
multiline conditions. Mouse dragging copies visible text, Ctrl-drag selects a
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

Choose an archive matching your operating system, CPU architecture and R
major/minor series. Each binary has a `.manifest` file describing its build
environment. `SHA256SUMS.txt` covers the release assets. The source package
works across supported platforms when built with their matching R toolchain.

Use R 4.6 or newer and install `cli`, `stringi` and `utf8` first. Start
interactive R and run `prismrepl::repl()`. On Windows, start Rterm.exe in
Windows Terminal. Use `:help`, `:keys`, `:settings` and `:doctor` to inspect
the console. Use `:quit` to return to R.

Windows ARM64 uses a community-supported R distribution. Linux binaries depend
on the build distribution's ABI; install from source on incompatible systems.
Enhanced terminal keys, glyph widths and hyperlinks depend on the terminal.
POSIX clipboard access uses optional system helpers; fallback status is explicit.

## Maintainer checks before publishing this draft

- Confirm all six native build jobs and their terminal checks passed.
- Confirm the tag, source archive, binary names and manifests agree with
  `DESCRIPTION`, the R series and the checked-out commit.
- Inspect the check logs and complete the manual terminal checklist.
- Test installation of the resulting archives in clean R libraries.
- Add the actual repository URL and issue URL to `DESCRIPTION` before CRAN submission.
- Record completed platform tests in `VALIDATION.md`.
- Publish the draft after reviewing the binaries and logs.

The workflow verifies the six-target artifact set, rejects mixed or stale
artifacts, writes checksums and creates a draft GitHub Release. Manual dispatch
builds downloadable workflow artifacts without publishing a release.
