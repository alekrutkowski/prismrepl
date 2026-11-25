# Manual terminal checks

Record R version, OS/architecture, terminal/version, locale, font and
`prismrepl::repl_diagnostics()` for each tested combination. The intended matrix
includes Windows Terminal/Rterm, Linux terminals, macOS terminals, WSL, SSH,
tmux/screen, and basic GUI-console fallback. A passing automated platform job
does not establish every terminal emulator's behavior.

1. Start Rterm.exe inside Windows Terminal and call `prismrepl::repl()`.
   Verify that the full-screen header, transcript, editor and footer are visible.
   Start and exit the console several times. Check normal echo, Enter, Ctrl+C,
   readline prompts, and terminal colors after exit.
2. Type accented names, decomposed combining marks, CJK, an emoji sequence,
   and mixed ASCII/Unicode. Move/delete at every boundary. Check narrow windows.
3. Resize during a multiline edit, during history search, and in a viewer.
   Verify the sidebar collapses in narrow windows, cards reflow, input survives,
   and no cursor position exceeds the actual viewport. Repeat with `:theme paper`.
4. Paste several lines with a trailing newline. Verify that no code runs until
   explicit Enter. Test a paste larger than the configured limit.
5. Test Tab and Shift+Tab on local names, package exports, arguments, list
   members and quoted paths. Deferred and active bindings must not run.
6. Search multiline history with Ctrl+R. Accept, edit and then submit a match.
7. Test Ctrl+Shift+M and Alt+M with both configured pipes. If the terminal
   reserves a chord or collapses it with Enter, record the limitation.
8. Trigger a message, warning and nested error. Verify the explicit [MESSAGE],
   [WARNING], and [ERROR] badges, contrasting backgrounds and readable borders.
   Check labels, trace, URL
   escaping, literal percent signs and non-Latin text. Do not send private data.
9. Open `:doc stats::lm`, `:docs base`, `:view iris`, `:objects`, and `:packages`.
   Search, pan horizontally, scroll and close. Repeat with `color = FALSE`.
10. Select multiple disposable objects with `:rm`, cancel once, then confirm.
    Do the same for non-core attached packages. Verify namespaces remain loaded.
11. Switch to `:ui inline`, run `readline('Name: ')`, a small `menu()` and a
    plotting command. Verify normal terminal input, then switch back with
    `:ui fullscreen`. Test debugger or external-console routines in inline mode.
12. Enable persistent history in a temporary directory, run two sessions, exit
    both, and verify both sets of entries survive.
13. On Windows, repeat with paste containing supplementary Unicode characters
    and verify console input/output modes after exceptions and cancellation.
14. In a GUI console or `TERM=dumb`, verify basic-mode entry, multiline input,
    helper functions and `:quit` without raw escape sequences.

15. Scroll the full-screen transcript with Page Up/Down while an expression is
    unfinished. Verify the expression stays editable and is not executed.
16. Open and close a viewer repeatedly. The main alternate screen must stay
    active until the session exits. Test ordinary `:quit`, Ctrl+D, interruption,
    and `q("no")` in a disposable session.
17. Run `pwsh -NoProfile -File tools/windows-console-smoke.ps1` after installation.
    Record its real result and retain the log. It deliberately disables VT and
    sets an OEM output page before testing exact native state restoration.
18. Fill a transcript with several thousand lines, move the cursor, edit a long
    expression, and scroll `:help` continuously. Watch for visible rolling lines,
    repeated full-window clears or input lag. Commands and descriptions must stay
    paired in both wide and narrow windows.
19. Open `:theme` with conditions and source text already in the transcript.
    Verify the current theme is marked and initially focused. Move with Up/Down
    and click a different theme; neither action should apply it yet. Press Enter
    to apply and save it, and verify every surface is repainted. Reopen the picker,
    move focus and cancel with Escape or `q`; the active and saved themes must
    remain unchanged. Search with `/`; Enter ends search and a second Enter
    applies the focused match. Test this after resizing to a narrow window.
    Also apply `:theme white` and `:theme black` directly. Restart using the same
    preferences file and verify the last accepted theme returns. In the plain
    interactive fallback, select a numbered theme once and cancel with `0` once.
    Noninteractive use must print the list and direct-command hint without waiting.
20. Insert a newline with Ctrl+J. Run `:bind newline F4`, verify F4, restart and
    verify the remap remains. Check the footer, history-search hints, and `:keys`.
    Restore the defaults with `:bind reset`. Alt+Enter may keep its terminal action.
21. Click before and after wide and combining characters, after a tab, and on
    wrapped editor lines. Use the wheel over the transcript and help. Explore
    nested list/data-frame/environment children, switch tabs, inspect attributes
    and slots, toggle child sizes with `s`, and return through breadcrumbs.
    Test `:mouse off`, `:mouse on`, terminal selection and hyperlink activation.


## Integrated workflow checks

- Open :h, search a description, toggle sort with Tab, and choose with Enter.
  Confirm only editable text is staged; execute only after another Enter.
- Open :ai, :pipe, :mouse, :rainbow, :match, :autopair, :ui and :sidebar without arguments.
  Verify current focus, search, mouse focus, Enter acceptance and Escape cancellation.
  In :ai, Custom without a saved URL must stage :ai custom without changing provider.
  Choose a binding in :bind or :keys and finish its staged command explicitly.
  In :settings, open a nested menu and cancel it; inspect the saving/file rows.
  In :status, toggle fields with Space, apply with Enter, and apply no fields once.
  Repeat choice acceptance and cancellation in the plain numeric menus.
- Click a palette command; drag its description instead and confirm no command
  is activated. Test Ctrl-drag rectangular selection and right-click paste.
- Confirm :h and other colon commands appear in Up/Ctrl+R/:history recall.
- Open numeric, character, factor and Date columns of a data frame. Values must
  appear directly in Preview; repeated Enter/Right must leave the path fixed.
  Page through a long vector with [/] and return with Left. Drill through a
  nested list column, verifying each atomic value is a leaf. Inspect Attributes.
- Inspect columns with ASCII, combining marks and CJK names; headings align.
- In a data-frame Preview, press `/` and filter values in rows beyond the
  initial batch. Confirm that hidden columns can match, original row labels
  remain visible, and `[` / `]` page through filtered rows. Enter keeps the
  filter; Escape restores its prior query and position. Test no matches,
  Ctrl+U, Unicode Backspace, paste, resize, and unchanged source data.
- In Windows Terminal, start from a profile with a dark cursor and select black.
  The insertion cursor must become visible and light. Switch to white and check
  that it becomes dark, including at an empty prompt and in :edit. Open/close
  :help and :theme; check cursor visibility on return. Switch to inline, suspend
  for an external command, and quit: the configured profile color must return.
  Repeat with a light profile cursor and with mono/color disabled. Verify the
  profile's cursor shape and blink settings remain unchanged.
- Open :doc mean, search a repeated term, check all highlights and n/p jumps.
- Test :attach completion and first-use/background :install completion.
- Confirm :cd with no arguments displays the directory; source an R script
  whose unquoted path contains spaces and Windows backslashes.
- Check the FILES module remains visible with many workspace objects. Browse
  a directory and preview a script; stage :source without executing it.
- Edit a workspace function with :edit. Save with Ctrl+S, then try an invalid
  save and verify the function is unchanged; cancel remaining edits.
- Exercise package installation in a disposable library; installer output and
  prompts use the ordinary terminal and the full-screen console resumes.
- On Windows CI, PRISM_TEST_CLIPBOARD=1 exercises the real Unicode clipboard.
  Use this flag only in a disposable test desktop: non-text clipboard formats
  cannot be restored by the text-only test probe.
- With a fresh temporary preferences path for each run, start once under a light
  OS appearance and once under a dark appearance. Check the detected theme. Set a theme explicitly,
  change the OS appearance, restart, and check that the saved choice still wins.
  Use `:theme auto` to redetect and save the current desktop appearance.
- Toggle `:rainbow off` and `:rainbow on` with nested parentheses, brackets,
  braces, quoted strings, escaped quotes, comments, and raw strings. Check that
  matching delimiters use the same color and complete transcript recoloring occurs.
- Type with `:autopair on`, skip generated closers, delete an empty generated
  pair, and compare with `:autopair off`. Paste and recalled entries stay literal;
  Enter submits complete visible code and Ctrl+J inserts a newline in either mode.
- Save `:ai chatgpt` or `:ai claude`, trigger an error, restart and check the
  selected destination. Test `:explain copy minimal` with public sample text.
  Home-page providers require pasting; no prompt opens or sends automatically.
- Check colored hint rows under all three sidebar modules, including long lists
  and white/black themes. Their command labels and descriptions use footer styling.

- With a multiline Unicode draft, move onto `stats::lm` and press F1. Verify
  local help opens without executing the draft. Close it and check exact text
  and cursor restoration. Repeat in inline mode and the `:edit` function editor.
  Verify F2 opens shortcuts and both actions can be remapped persistently.
- With `:rainbow off` and `:match on`, move onto both ends of each bracket and
  quote pair, including `r"--[raw text]--"` and multiline raw strings. Both
  delimiter endpoints must be distinct in black and white themes. Move away,
  toggle `:match off`, change themes and resize; stale highlights must disappear.
- Repeat package installation and binding-safe object/completion checks on
  exact R 4.5.0 as well as current R release. A saved default F1 mapping from an
  earlier installation should become F1 context help and F2 shortcuts; explicit
  custom assignments must remain intact.
