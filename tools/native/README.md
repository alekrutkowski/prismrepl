# Native decoder regression tests

Run from the package root with a C99-compatible compiler:

```sh
cc -std=c99 -Wall -Wextra -Werror -Isrc \
  tools/native/test-keyparse.c src/keyparse.c -o /tmp/prismrepl-keyparse
/tmp/prismrepl-keyparse
```

On Linux or macOS with a compiler that supports sanitizers, add
`-fsanitize=address,undefined -fno-omit-frame-pointer` to the compile command.
The test checks all valid Unicode scalar values, representative terminal key
encodings, SGR mouse presses/releases/wheels/modifiers, overflow handling, and
100,000 deterministic malformed input cases. It distinguishes Ctrl+J from Enter.
It requires no R installation and does not change terminal settings.

The R tests in `tests/testthat/test-native.R` separately check parser status,
lazy binding inspection, native routine registration through the package, and
non-interactive terminal detection.

On POSIX systems, after installing the package, also run:

```sh
python3 tools/native/pty-exit.py
python3 tools/native/pty-io.py
```

Set `PRISM_R` to choose an R executable. This starts the fullscreen REPL in a
real pseudo-terminal, executes `q('no')`, and checks that exit finalizers restore
the main screen and the exact original terminal input settings. This covers R
process exit, which does not follow the REPL's normal return path.

`pty-io.py` verifies native buffered UTF-8 writes through a real PTY, including
intentional bypass of R output sinks, optional synchronized frame delimiters,
mouse press, drag and release coordinates and modifiers, Ctrl+J, literal newlines in bracketed
paste, mouse opt-out, and exact restoration of input modes. It reports the time
to write 250 assembled 24-row frames. That measurement covers the transport
path, not the redraw speed of a graphical terminal emulator.

The Windows ConPTY smoke test also injects Ctrl-drag input. Its optional real
clipboard check is enabled with `PRISM_TEST_CLIPBOARD=1` on disposable CI runners.
It copies a rectangular Unicode selection through `CF_UNICODETEXT`, reads it
back, and verifies that right-click paste stays editable until Enter. Existing
Unicode text is restored when available. A non-text clipboard cannot be restored
by that probe, so the clipboard check is opt-in for a developer's desktop.

The mouse input layer requests xterm button-motion mode 1002 together with SGR
coordinates and disables both at cleanup. Windows uses `MOUSE_EVENT_RECORD`
and restores its original console input mode. Clipboard strings cross the
Windows API as UTF-16; R strings and POSIX helpers use UTF-8. The shared selection
model copies complete grapheme clusters and removes terminal style codes.

Primary API references:

- [Windows clipboard operations](https://learn.microsoft.com/en-us/windows/win32/dataxchg/clipboard-operations)
- [SetClipboardData ownership and allocation](https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setclipboarddata)
- [Xterm mouse and OSC control sequences](https://invisible-island.net/xterm/ctlseqs/ctlseqs.html)

`tools/pty-basic.py` separately checks the host console fallback. Before its
single Ctrl+D event, it waits for the host editor's terminal preparation and a
brief settling interval. The fallback prints its prompt before `readLines()`
enters that editor, so the visible prompt alone is not a synthetic-input
readiness signal. No production terminal behavior is changed by this test barrier.

## Windows console integration

After installing the package, run from its repository root in PowerShell:

```powershell
pwsh -NoProfile -File tools/windows-console-smoke.ps1
```

Pass `-Rterm C:\path\to\Rterm.exe` to select a particular R installation and
`-LogPath path\to\console.log` to select the captured terminal log. The test
requires Windows 10 version 1809 or later and the Rtools corresponding to R.
The package itself has no PowerShell, C#, or .NET dependency.

The test creates a real Windows pseudoconsole with `CreatePseudoConsole`, then
starts Rterm inside it. It reads output on a separate thread to prevent pipe
deadlocks. A small test-only DLL starts the console with virtual terminal
processing disabled and code page 437. The assertions verify that:

- A session output handle enables VT processing and UTF-8 output.
- Closing a line editor or resetting input preserves the output session.
- Closing the output handle restores both output modes and the original code page.
- Buffered UTF-16 console writes preserve Unicode even if the output code page
  changes, and the renderer bypasses R output sinks.
- Injected Win32 mouse presses, releases, and wheel events produce one-based
  viewport coordinates; the original input mode returns after input closes.
- Ctrl+J arrives as a newline shortcut and VK_RETURN remains Enter. A complete
  expression followed by Ctrl+J does not execute until an explicit Enter.
- Custom Ctrl/Alt digit and punctuation shortcuts preserve their modifiers;
  Ctrl+Space and zero-character console records use the active keyboard layout.
  AltGr stays text input. Native Windows F13 and F24 events are checked too.
- Warning, error, and message labels appear in the fullscreen transcript.
- Unicode identifiers and Unicode output survive the Windows input/output path.
- Bracketed paste requires an explicit Enter, cancellation recovers, Tab completes,
  and Alt+M inserts a pipe.
- Resizing ConPTY changes the native viewport dimensions and paint coordinates
  stay within the resized viewport.
- The output stream contains truecolor SGR and alternate-screen sequences.
- Leaving the REPL restores the original output settings.

`ConptyHost.cs`, `windows-console-probe.c`, and `windows-console-session.R` are
test infrastructure. They are excluded from the built CRAN source tarball with
the rest of `tools/`. A Windows CI result is required before describing this
integration test as passed; running the POSIX tests does not test Windows.

Microsoft API references:

- <https://learn.microsoft.com/en-us/windows/console/creating-a-pseudoconsole-session>
- <https://learn.microsoft.com/en-us/windows/console/setconsolemode>
- <https://learn.microsoft.com/en-us/windows/console/console-code-pages>
- <https://learn.microsoft.com/en-us/windows/console/console-screen-buffer-info-str>
- <https://learn.microsoft.com/en-us/windows/console/writeconsole>
- <https://learn.microsoft.com/en-us/windows/console/mouse-event-record-str>
- <https://learn.microsoft.com/en-us/windows/terminal/selection>
- <https://learn.microsoft.com/en-us/windows/terminal/customize-settings/actions>
- <https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes>
- <https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-mapvirtualkeyexw>

Protocol references:

- <https://invisible-island.net/xterm/ctlseqs/ctlseqs.html>
- <https://github.com/contour-terminal/vt-extensions/blob/master/synchronized-output.md>
- <https://sw.kovidgoyal.net/kitty/keyboard-protocol/>

The renderer uses one assembled native write per frame, with absolute cursor
positions and no added line feeds. POSIX writes retry only after interruption or
a partial write. Windows uses `WriteConsoleW` after one UTF-8-to-UTF-16 conversion.
Private mode 2026 is opt-in because terminals differ in synchronized-output
support. Mouse mode requests button events and SGR coordinates while an input
handle is open; every input cleanup disables those requests. Shift preserves
ordinary text selection in Windows Terminal and terminals with the same bypass.
Alt+Enter is a Windows Terminal fullscreen shortcut by default, so Ctrl+J is the
portable default for inserting an editor newline.

The decoder recognizes F1 through F24 via Win32 console records, F13 through
F20 via the documented xterm/VT220 sequences, and F13 through F24 via the kitty
CSI-u functional codes. The application does not take over the terminal's
global shortcut configuration. Keys reserved by the terminal, and combinations
that the terminal sends as identical bytes, remain terminal dependent.
