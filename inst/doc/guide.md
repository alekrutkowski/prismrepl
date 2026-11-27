# Prism REPL guide

Version 0.6.5.

## Capabilities at a glance

- `:help`, or `:h`, is a searchable command palette. Search names, aliases,
  descriptions and groups. Choosing a command puts it in the input field;
  **you review it, supply arguments, and press Enter to execute it**.
- Every command has an unambiguous one-letter or two-letter alias. Submitted
  colon commands and ordinary R expressions share the same searchable history.
- R syntax highlighting includes rainbow delimiters, a distinct highlight for
  the matching pair at the cursor, and automatic insertion of closing brackets
  and quotes. All three options start on and can be changed independently.
- F1 opens local documentation for the function at the cursor, including
  namespace-qualified names. Closing help restores the draft and cursor.
  F2 shows the effective shortcut cheatsheet.
- The object explorer stops at atomic vectors, including data-frame columns. Column headings and rows
  share the same cell positions, including wide Unicode names.
- Messages, warnings and errors keep all their lines in one labeled card.
- `:attach`, `:install`, `:installgh`, `:source`, `:files` and `:edit` cover
  common package, file and function-editing tasks.
- `:shell`, or `:sh`, runs a system shell command directly, without wrapping
  it in an R string. Shell pipes, redirects and ordinary argument quotes work.
- The sidebar has WORKSPACE, PACKAGES and FILES modules. Each module receives
  visible space even when another list is long, with a distinct command hint bar.
- Documentation has bold headings, italics, styled links and highlighted R code.
  Search highlights every occurrence and can jump between individual matches.
- Drag to copy visible text anywhere in full-screen views. Ctrl+drag selects a
  rectangle. Right-click pastes into the active editor, palette or search field.
- Colored keyboard hints use the same spacing and appearance across editors,
  the palette, documentation, object browsing and file browsing.
- Initial theme selection follows the desktop's light or dark preference when
  it can be detected. Theme choices, editing options, AI provider and keyboard
  remappings are remembered. Option menus mark and focus the current choice;
  Enter applies it. Theme changes repaint the complete interface.
- Error explanations can use Mistral, ChatGPT, Claude and other web providers,
  or a custom URL template. Prompt generation is local and explicitly optional.

The renderer assembles buffered frames, updates changed rows and caches visible
transcript content. Terminal resizing keeps the editor, transcript and optional
session pane within the available space.

## Install and start

Use R 4.5.0 or newer. Install the five external runtime dependencies, then the
source archive:

```r
install.packages(c("cli", "processx", "rlang", "stringi", "utf8"))
install.packages("prismrepl_0.6.5.tar.gz", repos = NULL, type = "source")
prismrepl::repl()
```

Building the source package requires a C compiler and the R development tools
for your platform. Windows users need the matching
[Rtools](https://cran.r-project.org/bin/windows/Rtools/). Linux users need the
R development components; macOS users need the command-line build tools.
The release workflow builds compiled packages for six platform/architecture
combinations. Dependencies must still be installed separately.

On Windows, start **Rterm.exe inside Windows Terminal**, then call
`prismrepl::repl()`. For example, if Rterm is on your PATH, run
`Rterm.exe --vanilla` from PowerShell. The Windows R GUI is a different host
and uses the basic fallback. On Linux or macOS, start interactive R in a terminal.

From an unpacked repository you can also install with `R CMD INSTALL .`.
Loading or attaching the package does not start the console or change your
`.Rprofile`, terminal settings, global condition handlers or history files.
`:quit`, or `:q`, returns to the ordinary R prompt without closing R.

### A first session

Type these in Prism's input area, one at a time:

```text
x <- datasets::iris
:objects
:doc mean
:h package
:theme white
:quit
```

The first line is R code. A line beginning with `:` is a Prism command.
In `:objects`, choose `x` and press Enter to inspect it, then press Escape to
close the explorer. In documentation, `/` opens search. In the command palette,
type words such as `install package`, choose a result, then finish the staged
command at the prompt. No selected command runs until that final Enter.

## The screen and session pane

The main pane contains the transcript and a multiline input field. The header
shows session state, while the footer shows useful shortcuts. The display
follows the actual terminal size; a narrow window hides the sidebar and wraps
input and conditions to the available width.

The sidebar has three modules:

| Module    | What it shows                                                             | What a click stages                             |
| --------- | ------------------------------------------------------------------------- | ----------------------------------------------- |
| WORKSPACE | R process RAM, object names, types/classes, dimensions and optional sizes | `:view NAME`, or `:objects` for more items      |
| PACKAGES  | Attached packages                                                         | `:docs PACKAGE`, or the full package list       |
| FILES     | Parent folder, subfolders and files in the current directory              | `:cd PATH` for folders, `:files PATH` for files |

A click stages a command for review in the input field. A drag selects text.
The sidebar refreshes after each submitted expression or command, so it follows
assignments, package attachment and changes to the working directory.

Each module ends with a compact command hint bar: `:objects inspect  :rm select`,
`:packages list  :detach select`, or `:files browse`. The command labels use an
accent color and the explanations use the footer's text style, with the same
two-space separation as hints elsewhere. These bars remain visually separate
from object, package and file entries. A short sidebar click stages a command;
it still needs Enter in the main input field before execution.

Use `:sidebar` to choose whether to show the pane, or apply `:sidebar off` or
`:sidebar on` directly. `:status` opens a field selector: Space toggles a field,
`a` selects or clears all fields, and Enter applies the selection, including
an empty selection. You can also enter `:status process_memory directory git objects packages memory`
or `:status none` directly. These display choices last for the current session.

**Total R memory is shown by default** as `R RAM: ... MiB` or `GiB`, both in
the status line and at the top of WORKSPACE. The `process_memory` field reports
the operating system's current resident-memory estimate for this R process,
including R itself, loaded libraries and native allocations. It includes shared
resident pages, excludes swapped-out memory and other processes, and is not a
lifetime peak. It refreshes when the prompt returns after an expression or
command, using one snapshot for both locations. Reading the counter neither
forces garbage collection nor starts a subprocess. The native counters use the
[Windows working set](https://learn.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-process_memory_counters),
[Linux resident pages](https://www.kernel.org/doc/html/latest/filesystems/proc.html),
or the equivalent macOS task information.

The separate, optional `memory` field shows the approximate sum of workspace
`object.size()` results and per-object sizes. Shared storage can be counted
more than once, and some native allocations are absent from that sum. Object
size calculation stays off by default. Use `:status process_memory directory git`
for the default status fields, or add `memory` to display both measurements.
If a process-memory counter is unavailable, Prism says so explicitly.

Git inspection is read-only and uses an optional Git
executable. It reports branch and tracked-file changes without refreshing on
every keystroke.

## Editing R expressions

Type R code normally. Strings, comments, raw strings, numbers, keywords,
operators and function calls have distinct syntax colors. Enter submits a
complete expression. If it is syntactically incomplete, Enter continues on a
new line. **Ctrl+J** always inserts a newline without submitting.

Multiline expressions remain one editable history entry. Up and Down move
through visually wrapped rows; at the edge of the buffer they recall history.
Left/Right and Backspace move or delete whole graphemes, including combining
marks. Wide characters count by terminal cells when placing the cursor.
Click in the input field to place the cursor precisely. Bracketed paste inserts
text without executing it; review pasted code before pressing Enter.

| Default shortcut       | Action                                                    |
| ---------------------- | --------------------------------------------------------- |
| Enter                  | Submit a complete expression or colon command             |
| Ctrl+J                 | Insert a newline                                          |
| Shift+Enter            | Additional newline shortcut when reported by the terminal |
| Tab / Shift+Tab        | Complete names / cycle candidates in the other direction  |
| Ctrl+R                 | Search whole history entries; repeat to cycle matches     |
| Ctrl+P / Ctrl+N        | Recall previous / next history entry                      |
| Ctrl+Left / Ctrl+Right | Move by word                                              |
| Home / End             | Move to the start / end of the logical line               |
| Ctrl+A / Ctrl+E        | Move to the beginning / end of the expression             |
| Ctrl+K / Ctrl+U        | Cut to line end / line start                              |
| Ctrl+W / Ctrl+Y        | Cut previous word / reinsert the last cut                 |
| Ctrl+C                 | Cancel pending input; during evaluation, interrupt R      |
| Ctrl+D                 | Exit on empty input; otherwise delete forward             |
| Ctrl+Shift+M or Alt+M  | Insert the selected pipe operator                         |
| PageUp / PageDown      | Scroll the transcript                                     |
| Ctrl+L                 | Request a full redraw                                     |
| F1                     | Open documentation for the function at the cursor         |
| F2                     | Open the effective shortcut cheatsheet                    |

Press **F2** in the main console to open the full shortcuts view immediately.
It lists the effective bindings, including your saved remappings. Scroll with
the arrow and paging keys, or press `/` to search shortcut names and descriptions.
Escape or `q` closes the view and restores your unfinished expression and
cursor position. Opening the cheatsheet does not submit the expression.

`:keys` shows the current bindings; choosing an action with Enter stages its
`:bind ACTION ` command for editing. The `max_paste` and `max_lines` settings
bound the entire editable expression, including typing, completion, recall and
paste. A rejected insertion preserves the previous buffer and explains why.

### Function help at the cursor

Move the cursor onto a function name and press **F1**. For example, type
`stats::lm(mpg ~ wt, data = mtcars)`, move onto `lm`, and press F1 to read its
installed R documentation. Plain names, namespace-qualified names, backtick
names and documented operators are supported, even in incomplete or multiline
input. The right edge of a name also counts as being on that name. Text inside
comments and strings is ignored. Resolving a topic never runs the draft or
forces a workspace binding.

The rich documentation viewer keeps its search, highlighted matches and
scrolling controls. Press Escape or `q` to close it and resume exactly where
you were typing. The same shortcut works inside `:edit`. When no local topic is
available, a small editor hint explains the result and the draft remains intact.
Basic mode uses the host editor, so `:doc NAME` remains available there.

Both shortcuts can be saved with `:bind function_help F1` and `:bind help F2`.
Existing saved default F1-cheatsheet mappings migrate to this arrangement;
custom assignments are retained. `:keys` is always available to inspect or
change the effective mappings.

### Matching pairs and rainbow colors

**Cursor pair highlighting, rainbow colors and automatic pairing all start
on.** They are independent. Cursor pair highlighting marks both ends of the
pair at the cursor with a contrasting background, bold text and an underline.
It works with rainbow colors switched off and follows the current theme,
including the plain white and black themes.

Move onto either end, or immediately after it, to highlight `()`, `[]`, `{}`,
single or double quotes, and backtick names. For a raw string such as
`r"--(literal text)--"`, the whole opening `r"--(` and closing `)--"` are
highlighted together. The bracket forms `r"[text]"` and `r"{text}"` work too.
Comments, escaped quotes and literal brackets inside strings do not create
false pairs. An unfinished delimiter has no matching highlight. For a pair
spanning several rows, each endpoint is highlighted when it is visible.
The editor caches pair positions while the text is unchanged, so moving the
cursor does not rescan the whole expression.

Rainbow colors identify nesting levels of `()`, `[]` and `{}`, including the
brackets in `[[...]]`. Matching single quotes, double quotes, backticks and raw
string wrappers share a color. String contents and comments retain their own
syntax styling; punctuation inside them does not become R structure. Colors
come from the current theme and disappear when color output is disabled.

When you type `(` in ordinary R code, Prism inserts `)` and leaves the cursor
between them. Brackets, braces and quotes work the same way. Typing an
automatically inserted closer steps over it, and Backspace between an empty
generated pair removes both characters. Pairing respects escapes, comments and
literal string contents. It avoids inserting a pair in the middle of a name
or directly before an existing word. Colon commands and their paths remain
literal, as do pasted text, completion candidates and recalled history.

```text
:match off
:ma on
:rainbow off
:ra on
:autopair off
:au toggle
```

Use `:match`, `:rainbow` or `:autopair` without arguments to choose `on`, `off` or
`toggle`. The current setting is marked and focused; Enter applies and saves
the choice for future sessions. Escape or `q` cancels without changing it.
Changes repaint the editor immediately; rainbow changes also recolor visible
R source in the transcript. These features apply to the
native inline and full-screen editors, including `:edit`; basic mode uses the
host R console's editor.

**Enter still submits syntactically complete visible code**, even when the
cursor is between a generated pair. Use Ctrl+J to add a newline while building
a multiline call or function. In the separate function editor, Enter always
inserts a newline and Ctrl+S validates and saves.

### Completion

Tab completes visible R names, function arguments, safe `$` members and file
paths inside quoted R strings. Namespace completion uses already-loaded
namespaces and does not load packages just to inspect them. Promises and active
bindings are not evaluated for completion.

Colon commands have their own completion: command names and aliases, installed
package names for `:attach` and `:docs`, CRAN names for `:install`, theme names,
AI provider names for `:ai`, settings values such as `on`, `off` and `toggle`,
and unquoted paths for `:source`, `:cd` and `:files`.
Tab inserts a shared prefix or a candidate; further Tab presses cycle candidates.

### History, including colon commands

Every nonempty submitted expression and colon command enters history, including
commands that report an error. Choosing a palette item alone does not enter it
in history; it becomes an entry when you submit it. Repeating an identical entry
moves it to the newest position, within the configured history limit.

`:history` opens the numbered list; `:history theme` filters it. `:recall 12`
places entry 12 back in the editor. It does not run it. Ctrl+R searches complete
entries, including multiline R expressions and commands such as `:source`.

History is kept in memory by default. Opt into a file if you want it across R
sessions:

```r
prismrepl::repl(config = prismrepl::repl_config(
  history_file = file.path(tools::R_user_dir("prismrepl", "data"), "history.rds"),
  history_limit = 2000L
))
```

The saved file preserves complete UTF-8 expressions and merges sessions under
a short write lock. Remember that submitted code and commands can contain
sensitive values; choose a history path accordingly.

## Command palette and short aliases

Enter `:help` or `:h`. Start typing immediately: the search checks command
names, aliases, groups and descriptions. With multiple words, all words must
match somewhere in that combined text. For example, `workspace function`
finds function editing without requiring you to remember `:edit`.

Use arrows or PageUp/PageDown to choose a result. Press Enter or click it to
close the palette and put its command in the main input field. Commands needing
arguments include a trailing space, ready for typing. Supply or adjust the
arguments and press Enter to approve execution. Escape closes the palette
without staging anything. Ctrl+U clears the query. **Tab switches between
logical groups and alphabetical order**, while retaining the selected command
when possible. You can start with a filter, for example `:h package`.

The table below is the complete reference for all 40 built-in commands. Aliases mean
exactly the same thing as their full command names; collisions use two letters.

| Command                       | Alias | Group              | Purpose                                                                                                          |
| ----------------------------- | ----- | ------------------ | ---------------------------------------------------------------------------------------------------------------- |
| `:help [TEXT]`                | `:h`  | Discover           | Search commands and descriptions; choose one to place it in the input for approval.                              |
| `:keys`                       | `:k`  | Discover           | Browse shortcuts; Enter places the selected :bind command in the input.                                          |
| `:commands [TEXT]`            | `:co` | Discover           | Browse your saved custom R-function commands; choose one to place it in the input.                               |
| `:doc TOPIC [PACKAGE]`        | `:d`  | Discover           | Read styled function documentation; also ?mean or ?stats::lm.                                                    |
| `:docs PACKAGE`               | `:ds` | Discover           | Browse an installed package's documentation index.                                                               |
| `:objects [TEXT]`             | `:o`  | Workspace          | Explore workspace bindings, optionally filtering their names.                                                    |
| `:view NAME`                  | `:v`  | Workspace          | Inspect an object's structure, values, attributes and slots.                                                     |
| `:edit NAME`                  | `:ed` | Workspace          | Edit a function in a small editor; validate and save explicitly with Ctrl+S.                                     |
| `:rm [NAME ...]`              | `:r`  | Workspace          | Remove named objects, or select a batch interactively.                                                           |
| `:packages`                   | `:p`  | Packages           | List attached packages and loaded namespaces.                                                                    |
| `:attach PACKAGE ...`         | `:a`  | Packages           | Attach installed packages in order with library(); stop at the first error. Tab completes each package name.     |
| `:detach [PACKAGE ...]`       | `:de` | Packages           | Detach named packages, or select a batch; retain namespaces.                                                     |
| `:install PACKAGE ...`        | `:i`  | Packages           | Install packages from the configured repositories; Tab uses a cached CRAN index. --refresh refreshes that index. |
| `:installgh USER/REPO`        | `:ig` | Packages           | Install a GitHub package with remotes::install_github(); no quotes needed.                                       |
| `:files [PATH]`               | `:f`  | Files              | Browse directories and preview text files; stage a source command for an R script.                               |
| `:source PATH`                | `:so` | Files              | Run an R script in this workspace. Unquoted path spaces and single backslashes are preserved.                    |
| `:cd [PATH]`                  | `:c`  | Files              | Change working directory; with no path, show the current directory like :pwd.                                    |
| `:pwd`                        | `:pw` | Files              | Show the current working directory.                                                                              |
| `:git`                        | `:g`  | Files              | Show tracked-file Git status for the current directory.                                                          |
| `:shell COMMAND`              | `:sh` | Files              | Run a system shell command without R-string quoting; preserve shell quotes, pipes and redirects.                 |
| `:history [TEXT]`             | `:hi` | History and errors | Browse submitted R expressions and colon commands; optionally filter literal text.                               |
| `:recall NUMBER`              | `:re` | History and errors | Place a numbered history entry in the input for editing and approval.                                            |
| `:trace`                      | `:tr` | History and errors | Show the most recent error's captured call trace.                                                                |
| `:explain [copy] [minimal]`   | `:e`  | History and errors | Show an AI explanation link, or copy its prompt; minimal omits source and call trace.                            |
| `:theme [NAME\|auto]`         | `:t`  | Appearance         | Choose a theme or desktop appearance; Enter applies, saves, and repaints.                                        |
| `:ui [fullscreen\|inline]`    | `:ui` | Appearance         | Choose the full-screen interface or inline editing; Enter applies for this session.                              |
| `:sidebar [on\|off]`          | `:si` | Appearance         | Choose whether the WORKSPACE, PACKAGES and FILES pane is shown in this session.                                  |
| `:status [FIELD ...]`         | `:s`  | Appearance         | Select status fields with Space, then Enter; explicit fields or none are also accepted.                          |
| `:clear`                      | `:cl` | Appearance         | Clear the retained transcript or terminal display.                                                               |
| `:pipe [\|> \| %>%]`          | `:pi` | Preferences        | Choose with Enter and remember the operator inserted by the pipe shortcut.                                       |
| `:bind [ACTION [KEY ...]]`    | `:b`  | Preferences        | Choose an action to stage its remapping command; explicit keys save it, and reset restores defaults.             |
| `:unbind [ACTION]`            | `:u`  | Preferences        | Choose an action with Enter to disable its shortcuts and save the preference.                                    |
| `:mouse [on\|off]`            | `:m`  | Preferences        | Choose and remember mouse clicks, dragging, copying, pasting and scrolling.                                      |
| `:match [on\|off\|toggle]`    | `:ma` | Preferences        | Highlight the matching bracket or quote pair at the cursor; save the choice.                                     |
| `:rainbow [on\|off\|toggle]`  | `:ra` | Preferences        | Choose and remember matching-parenthesis, bracket, brace and quote colors.                                       |
| `:autopair [on\|off\|toggle]` | `:au` | Preferences        | Choose and remember automatic matching delimiters and generated-closer skipping.                                 |
| `:ai [PROVIDER\|custom URL]`  | `:ai` | Preferences        | Choose and save the error-explanation provider with Enter; Custom can stage a URL for editing.                   |
| `:settings [reset]`           | `:se` | Preferences        | Choose a preference to edit; URL setup and reset commands are placed in the input for approval.                  |
| `:doctor`                     | `:dr` | Session            | Show terminal, color, clipboard and platform diagnostics.                                                        |
| `:quit`                       | `:q`  | Session            | Return to the ordinary R prompt without quitting R.                                                              |

Use `prismrepl::repl_commands(details = TRUE)` for the same registry from R.
For commands taking several names, separate names with spaces; quote names
that contain spaces. File commands treat the entire remainder as one path.

### Selecting command options

Run an option command without its value, such as `:ai` or `:pipe`, to open
its menu. Single-setting menus mark the current choice with `(current)` and
focus it initially. Up/Down or a mouse click moves the focus; **Enter accepts
the focused entry**. Moving or clicking alone leaves the setting unchanged.
The footer shows effective navigation keys when you have remapped them.

Press `/` to search names and descriptions. While the search field is active,
Enter finishes the search; press Enter again to accept the focused entry.
Escape first cancels an active search. Outside search, Escape or `q` closes
the menu without applying a choice.

| Menu command                      | What accepting an entry does                                                                             | Persistence                        |
| --------------------------------- | -------------------------------------------------------------------------------------------------------- | ---------------------------------- |
| `:theme`                          | Applies the theme and repaints; the last entry, `auto`, reads desktop appearance again.                  | Saved when persistence is enabled  |
| `:ai`                             | Applies a provider; Custom uses its saved URL or stages `:ai custom ` for a new template.                | Saved after a complete choice      |
| `:pipe`, `:mouse`                 | Applies the pipe operator or mouse on/off setting.                                                       | Saved when persistence is enabled  |
| `:match`, `:rainbow`, `:autopair` | Applies on, off or toggle to the selected editing preference.                                            | Saved when persistence is enabled  |
| `:ui`, `:sidebar`                 | Applies the console layout or shows/hides the session pane.                                              | Current session only               |
| `:status`                         | Space toggles one field, `a` selects/clears all, and Enter applies the batch; selecting none is allowed. | Current session only               |
| `:bind`, `:bind ACTION`, `:keys`  | Stages `:bind ACTION ` so you can type a key and submit it; the reset entry stages `:bind reset`.        | Saved after submitting the command |
| `:unbind`                         | Disables the selected action's shortcuts immediately.                                                    | Saved when persistence is enabled  |
| `:settings`                       | Opens a setting's menu, stages custom-URL/reset commands, or displays saving/file information.           | Follows the selected setting       |

For a binding, type the shortcut after the staged command, for example
`:bind newline F4`, then press Enter to apply it. The `:bind ACTION` form
opens just that action's entry. In `:settings`, Custom AI URL stages the saved
template for editing, or an empty `:ai custom ` prefix; Reset preferences
stages `:settings reset` for explicit submission. Save changes and Preferences
file are informational rows.

In a plain interactive console, single-choice menus accept a number and
`0` cancels. The `:status` menu lets you toggle fields by number and choose
**Apply selection** when finished; `0` cancels the whole batch. Noninteractive
calls print the options without waiting for input or changing settings.

## Packages and files

### Attach, install and detach packages

```text
:attach data.table ggplot2 tidyr
:a data.table ggplot2 tidyr
:install data.table cli utf8
:installgh r-lib/cli
:packages
:detach data.table ggplot2 tidyr
:de data.table ggplot2 tidyr
```

Both `:attach` and `:detach` accept several package names separated by spaces.
Their short aliases are `:a` and `:de`. Tab completes the current package name
at every position in the list.

`:attach xxx yyy` calls `library(..., character.only = TRUE)` for each unique
package in the order entered. A single name still works. Attachment stops at
the first error, with earlier successful attachments retained, just like
sequential `library()` calls. It does not install missing packages. Completion
uses installed names without reading the full installed-package metadata table.
`:install xx yy zz` calls `install.packages(c("xx", "yy", "zz"))`,
with your configured repositories. An unset CRAN mirror uses
`https://cloud.r-project.org`, avoiding an unexpected mirror menu.

On the first Tab after `:install`, Prism starts a background R process to obtain
the CRAN package names using R's `available.packages()`. The editor remains
responsive and explains that the index is being prepared. Press Tab again
shortly. The plain-text name cache is reused across sessions and refreshed after
24 hours; cached names remain usable while it refreshes. `:install --refresh`
retries or explicitly refreshes the index. No download happens merely from
loading the package or constructing a configuration. Completion lists CRAN names;
R's installer still decides compatibility, dependencies and binary availability.

The background index worker is managed by `processx`. Its temporary files are
removed after it finishes or is cancelled, and an outstanding worker is stopped
when the REPL closes or its namespace unloads. Changing repositories cancels the
old job before starting another; an old result cannot replace the current cache.
Cancellation and ordinary refresh errors leave the previous valid cache available.

`:installgh user/repo` calls `remotes::install_github("user/repo")`; quotes are
optional. The usual `user/repo/subdir` and `user/repo@ref` forms also work.
Install the optional `remotes` package first with `:install remotes` if needed.
Installer prompts retain their usual behavior. During package installation the
full-screen display is temporarily suspended, so compiler output and interactive
questions use the ordinary terminal. Prism resumes when the installer returns;
the detailed installation output remains in terminal scrollback.

`:packages` distinguishes attached packages from loaded namespaces. Detaching
removes the named packages from the search path while retaining their
namespaces. Packages already detached are reported without failing the batch.
Use `:detach` without names for a batch selector. Core packages are protected.
Space selects rows, `a` selects all/none, Enter accepts, and a confirmation is
requested unless you explicitly disabled confirmations. `:rm` uses the same
pattern for deleting workspace bindings.

### Your own persistent colon-commands

You can turn an R function into a named console command with
`repl_add_command()`. The definition is saved by default and is available in
later Prism sessions. Run the registration as ordinary R code, either inside
Prism or before opening it:

```r
prismrepl::repl_add_command(
  "where", function() getwd(),
  description = "Show my current working directory"
)

prismrepl::repl_add_command(
  "greet", function(name) paste("Hello,", name),
  description = "Greet a person by name", alias = "gr"
)

prismrepl::repl_add_command(
  "compare", function(left, right) paste(left, "versus", right),
  description = "Put two labels beside each other"
)

prismrepl::repl_add_command(
  "join", function(...) paste(c(...), collapse = " | "),
  description = "Join any number of labels"
)
```

You can then submit:

```text
:where
:greet Ada Lovelace
:gr Ada  Lovelace
:compare "North region" "South region"
:join first "second item" third
```

Every supplied argument reaches the function as a **single character string**.
For example, `123`, `TRUE`, `NA`, `x=y` and `$HOME` remain those literal strings.
The command parser does not turn them into numbers, logical values, missing
values, named arguments or environment-variable substitutions. Convert an
argument inside your function when you need another type.

| Function signature        | Argument handling                                                        |
| ------------------------- | ------------------------------------------------------------------------ |
| `function()`              | No arguments; submit the command name alone.                             |
| `function(text)`          | The entire remaining text is one string; internal spaces are preserved.  |
| `function(left, right)`   | Whitespace separates arguments; quote an argument containing spaces.     |
| `function(...)`           | Zero or more whitespace-separated or quoted strings.                     |
| `function(first, ...)`    | The first string fills `first`; additional strings enter `...`.          |

The whole-remainder rule applies only to a function with exactly one formal
argument that is not `...`. Thus `:greet Ada  Lovelace` retains the two spaces
between the names. You can also surround that entire argument with a matching
pair of quotes. Only that complete outer wrapper is removed; quotes within
otherwise unquoted text remain literal. For commands with several arguments,
single quotes, double quotes or backticks group a string containing spaces.
Place the opening quote at the start of an argument, and separate the next
argument with whitespace after the closing quote. Apostrophes inside bare
words, such as `don't`, remain literal.

Backslashes remain literal, so Windows paths do not need R-string escaping,
including a quoted directory path ending in a backslash. There are no
backslash escape sequences. To include a quotation mark inside a grouped
argument, choose another quote style, for example `'She said "hello"'`.
Unclosed quotes are reported before the command runs.

An explicit `""` or `''` supplies an empty string. With no argument text, no
argument is supplied, allowing an R default such as `function(name = "world")`
to take effect. Normal R argument matching handles required and excess
arguments. Arguments are positional; formal parameters after `...` should
have defaults if they are needed by the function.

Custom names and optional aliases appear in Tab completion and the searchable
`:help` palette. Descriptions are searchable too. Use `:commands` or `:co` to
browse only your custom commands. Choosing an entry stages it in the input;
you can supply arguments and press Enter to run it. Custom commands enter
history and use the same object previews, labeled messages, warning blocks,
errors and tracebacks as ordinary R expressions.

#### Review, replace or remove definitions

```r
prismrepl::repl_custom_commands()

prismrepl::repl_add_command(
  "greet", function(name) paste("Welcome,", name),
  description = "Welcome a person", alias = "gr", replace = TRUE
)

prismrepl::repl_remove_command("greet")
```

Replacement is explicit. A custom name or alias cannot take over a built-in
command, a built-in alias, or another custom command's name or alias. Names
start with a letter and can contain letters, digits, underscores and hyphens;
matching is case-insensitive.

#### Persistence and function environments

Definitions are saved in a separate `commands.rds` registry. Check its path
with `repl_commands_file()`. It normally sits beside the selected preferences
file. Registration and removal write the registry; merely opening the console,
listing commands or completing a name does not execute the command body.

Ordinary R closures retain their captured local values. For example, this
command retains its prefix when you start another R session:

```r
greeting <- local({
  prefix <- "Welcome to the project,"
  function(name) paste(prefix, name)
})
prismrepl::repl_add_command("project-greet", greeting)
```

References to objects in `.GlobalEnv` continue to mean objects in the current
R session. The registry does not save your entire workspace. Recreate those
objects after restarting R, or capture the constants you need in a small local
environment as above. Prefer explicit package calls, such as
`utils::head()` or `data.table::fread()`, inside reusable commands. Open
connections and other operating-system resources need to be reopened in the
new session. The registry is limited to 16 MiB, including captured data; keep
large datasets outside command definitions. Changes to a captured environment
while a command runs are retained for this R process, and are saved again only
if you re-register the function.

For a separate registry, pass the same path when defining commands and when
configuring the console:

```r
path <- file.path(tempdir(), "project-commands.rds")
prismrepl::repl_add_command("where", function() getwd(), file = path)
cfg <- prismrepl::repl_config(commands_file = path)
# In an interactive R session:
# prismrepl::repl(config = cfg)
```

Use a persistent path instead of `tempdir()` when you want to retain that
registry. A relative `commands_file` path is resolved when the configuration
is created, so `:cd` keeps using that registry. `PRISMREPL_COMMANDS_FILE` can
select the default path. Using
`file = NULL` for registration and `commands_file = NULL` in the configuration
selects an in-memory registry for the current R session.

Set `external = TRUE` when registering a function that needs ordinary terminal
input or output, for example an interactive external program. Prism temporarily
leaves its full-screen interface for that command. Press Enter or Ctrl+C at the
return prompt to restore the interface afterward.

### Directory navigation, previews and sourcing

```text
:pwd
:cd
:cd C:\Users\Alek\My R project
:files
:source my/path/folder with spaces/my file.R
:source C:\Users\Alek\My R project\analysis.R
```

`:cd` without an argument is equivalent to `:pwd`. With an argument it changes
the working directory. For `:cd`, `:files` and `:source`, the whole remainder
of the line is a single path. **Internal spaces and single backslashes are
preserved**; they are not parsed as R string escapes. Matching surrounding
single quotes, double quotes or backticks are also accepted. Both `/` and `\`
work as path separators on Windows; the path must exist on the current OS.

`:source PATH` runs the script in the REPL's evaluation environment, matching
ordinary `source()` in the global workspace when using the default `repl()`.
Syntactically valid assignments made before a runtime error remain, as they do
with R's own `source()`. Selecting a script in a browser never runs it by itself.

`:files [PATH]` opens a small file explorer. Enter or double-click opens a folder
or a read-only text preview. Backspace returns to the parent folder, `/` filters
names, and Escape returns from a preview or closes the browser. `c` stages
`:cd` for the displayed directory. `s` stages `:source` for the selected R
script. Return to the prompt, review that command and press Enter to run it.
Large text previews and directory listings are bounded; binary files are
identified without trying to display them as text. R script previews use
syntax highlighting. Browsing does not modify files.

Text recognition uses the contents, including their encoding, rather than a
filename-extension whitelist. UTF-8 text, UTF-16 and UTF-32 with byte-order
markers, and strongly recognizable UTF-16 text without a marker are supported.
This includes Windows `.ini` and `.log` files containing Unicode text. CRLF
line endings are displayed normally. Ambiguous binary contents remain blocked,
and malformed text receives a decoding notice. A byte limit preserves complete
characters at the cutoff; the original file is never converted or rewritten.

### System shell commands

Use `:shell COMMAND`, or its short alias `:sh`, to run a command directly:

```text
:shell git status
:sh git status --short > changes.txt
```

There is no outer R string to quote or escape. Everything after the command
name is passed to the shell as command text, preserving internal spaces,
quotes, backslashes, variable references, pipes and redirection. Individual
arguments still use the shell's usual quoting rules. For example, Windows
paths containing spaces use ordinary double quotes and single backslashes:

```text
:sh type "C:\Users\Alek\My R project\notes.txt"
```

The same Prism command works across operating systems, while the commands
available inside the shell follow that system's conventions:

| Task                        | Windows default shell | Linux or WSL2 shell |
| --------------------------- | --------------------- | ------------------- |
| List names in this folder   | `:sh dir /b`          | `:sh ls`            |
| Show a detailed folder list | `:sh dir`             | `:sh ls -lah`       |

On Linux and other Unix-like systems, Prism uses R's `system()` to run the
command through the system shell, normally `/bin/sh`. On Windows it uses
R's `system2()` to invoke the Windows command processor selected by `COMSPEC`,
falling back to `cmd.exe`. This uses Windows command-processor syntax
independently of `R_SHELL` or the Windows Terminal profile used to start R.
To use PowerShell explicitly, for example, enter
`:sh powershell -NoProfile -Command Get-Date`. R describes the platform
differences in its
[`system()` documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/system.html).

The child process inherits R's current directory and environment. A `cd` or
environment assignment inside that child does not persist in R; use Prism's
`:cd` to change the REPL's directory. Prism waits for the command to finish,
displays a nonzero exit status clearly, and retains the status in `.Last.value`
when the evaluation workspace permits that assignment.

In full-screen mode, Prism temporarily restores the ordinary terminal so the
command's output and interactive input work normally. After completion, press
Enter to return to Prism; Ctrl+C also returns. The full output remains in
terminal scrollback. Inline and basic modes return directly without that pause.

Shell commands share the ordinary command history and can be recalled and
edited. The searchable palette stages `:shell ` for your approval, and Tab
completes the Prism command name. Shell command names and arguments are handled
by the shell when you execute them.

## Structural object explorer

`:objects` starts at the workspace. `:objects model` filters its names, while
`:view model` starts at one exact ordinary binding. The breadcrumb shows your
current location. Each row has a name, type/class, shape or length, optional
size, and short value preview. In smaller windows, each object's details stay
together on wrapped rows rather than separating names from descriptions.

Enter, Right, or a double-click opens a child. Left or Backspace returns to its
parent. Tab cycles through **Children**, **Preview**, and **Attributes**. `/`
filters names in Children and Attributes, and values in Preview. `[` and `]`
change batches, and `s` toggles approximate sizes in structural views.
F2 explains the explorer keys. Large containers are inspected in bounded
batches; size measurements are cached for visible records.

### Filtering preview values

Open a data frame with `:view iris`, press Tab to select **Preview**, then press
`/`. The **Filter values:** field appears. Type `virginica` to keep rows with
that text in any column. Matching is literal and case-insensitive, including
Unicode text. The filter searches beyond the current preview batch, and it can
match a column that is not currently visible in a narrow terminal. Original row
numbers and row names are retained so you can locate results in the source.

Press **Enter** to keep the filter. Press **Escape** while editing to restore
the previous filter and scroll position. **Ctrl+U** clears the query, and
Backspace removes one Unicode character. Pasting into Preview starts or edits
the value filter; it never changes the object. A clear no-match message appears
when nothing qualifies. `[` and `]` page through matching rows, and resizing
keeps the filter active.

Atomic-vector and matrix previews support value filtering too. The explorer
caches the latest matching indices so ordinary redraws do not rescan the
object. Extending a query can narrow the previous matches, and formatting uses
bounded chunks. The first search of a very large object still needs to inspect
its relevant values and can take longer than a redraw.

Filters match safe value text before terminal-width clipping: full character
values, factor labels, Date values and formatted numbers. Higher-dimensional
atomic arrays use linear element indices. List-column cells use their short
preview summaries; open a nested child to inspect its values in detail.
Function, environment and S4 Preview filters search their bounded explanatory
text. They do not evaluate bindings or call custom printing methods.

### Navigating object structure

Lists, data frames, environments, S4 slots, function formals, bodies and
closure environments can be inspected structurally. **Atomic vectors are
leaves**, including numeric, character and logical columns, factors and Dates.
Opening a data-frame column shows its values directly; Enter or Right does
not create an element child or another `[[1]]` breadcrumb. Long atomic-vector
previews use bounded `[` / `]` batches, with Home/End and continued Up/Down
navigation. Matrices and arrays also remain value previews. Attributes stay
available through their tab. List columns retain structural navigation into
their list elements; any atomic value reached there is again a leaf.

The explorer does not call arbitrary custom print, format, subset or str
methods. It labels unforced promises and active bindings and does not execute
them. This avoids running user code merely because a pane becomes visible.
Object sizes are approximate. They can cost time to compute for large objects,
so leave them off when you do not need them.

## Editing functions with :edit

Create or select an ordinary function in the workspace, then open it:

```text
f <- function(x) mean(x, na.rm = TRUE)
:edit f
```

The editor shows the function's source with R highlighting, wrapped Unicode
cursor movement and a compact shortcut bar. Enter inserts a newline here.
It does not execute the source.

| Shortcut         | Function editor action                                   |
| ---------------- | -------------------------------------------------------- |
| Ctrl+S           | Validate and save the edited function in the workspace   |
| Ctrl+X or Escape | Close; discard edits made since the last successful save |
| Ctrl+Z / Ctrl+Y  | Undo / redo                                              |
| Ctrl+W           | Find literal text, ignoring case                         |
| Ctrl+N           | Jump to the next match                                   |
| Tab              | Complete R names                                         |
| Click / wheel    | Place the cursor / move through the source               |

Keep the editor contents as one `function(...)` expression. Do not add
`f <-` or a second expression. A successful save replaces `f` and keeps its
original lexical environment and custom attributes. Its body and default
arguments are not executed during validation. A parse error is shown in the
editor and leaves the old function intact. Ctrl+S does not write an `.R` file;
it changes the function in the current R workspace.

An inherited function is copied into your workspace when saved; package and
namespace bindings are not modified. Locked, active or deferred targets are
rejected with an explanation. If an attached function is still a lazy binding,
evaluate its name explicitly once before using `:edit`; the editor does not
force it for you. The save also checks that the target has not changed since
it was opened.

## Documentation, search and conditions

`:doc mean`, `:doc stats::lm`, `?mean` and `?stats::lm` open function documentation.
`:docs stats` opens a package index. Documentation comes from the installed Rd
files and R's HTML renderer, converted to safe terminal text. Headings, bold
text, italics, lists, links and R code receive suitable terminal formatting.
It uses no browser or network. Fonts and emulators determine how italics look;
plain text remains readable when styling is disabled.

Press `/` to search. **Every occurrence is highlighted**, including repeated
matches on one line. Enter accepts the search. `n` jumps to the next occurrence;
`p` jumps to the previous one and wraps at the ends. The active occurrence has
a distinct highlight. Search is literal and case-insensitive; regular-expression
punctuation has no special meaning. A pasted query opens the search field.
Arrows scroll, PageUp/PageDown page, and F2 shows the viewer's shortcuts.
Special characters are preserved without terminal overstrike backspaces.

Normal values appear as ordinary output. Each message, warning or error has
one clearly labeled **[MESSAGE]**, **[WARNING]** or **[ERROR]** block. Multiline
condition text stays inside that single block. Errors include captured user
calls in a readable order, and `:trace` brings the latest trace back for review.
Warnings are recorded and displayed immediately; Prism does not add them to
base R's deferred `warnings()` buffer. R's `options(warn = ...)` behavior is
respected for suppression and conversion to errors.

### AI explanation links and persistent provider choice

An error includes a link to your selected AI provider where the terminal
supports clickable OSC 8 links. **Mistral is the default.** To choose a different
provider, use `:ai`; the choice is saved with your other preferences and applies
to subsequent error links and requests for the most recent error.

```text
:ai
:ai chatgpt
:ai claude
:explain
:explain minimal
:explain copy
:explain copy minimal
```

`:ai` alone opens the provider menu with the active choice marked and focused.
Use Up/Down or a mouse click to focus a provider, then Enter to apply it.
Escape or `q` cancels without saving a change. After `:ai `, Tab completes
provider identifiers. `:explain` prints a URL for the latest error
using the currently selected provider. Add `minimal` to omit source code and
the call trace. Add `copy` to copy the explanation prompt to the clipboard and
print the destination URL, ready to open and paste. `:e` is the short form of
`:explain`; `:ai` is already a short, unambiguous command.

| Provider identifier | Name       | Browser destination or prompt template        | How the prompt reaches it |
| ------------------- | ---------- | --------------------------------------------- | ------------------------- |
| `mistral`           | Mistral AI | `https://chat.mistral.ai/chat?q={prompt}`     | Prompt in URL             |
| `chatgpt`           | ChatGPT    | `https://chatgpt.com/?q={prompt}`             | Prompt in URL             |
| `claude`            | Claude     | `https://claude.ai/new?q={prompt}`            | Prompt in URL             |
| `perplexity`        | Perplexity | `https://www.perplexity.ai/search?q={prompt}` | Prompt in URL             |
| `gemini`            | Gemini     | `https://gemini.google.com/app`               | Copy and paste            |
| `copilot`           | Copilot    | `https://copilot.microsoft.com/`              | Copy and paste            |
| `grok`              | Grok       | `https://grok.com/`                           | Copy and paste            |
| `deepseek`          | DeepSeek   | `https://chat.deepseek.com/`                  | Copy and paste            |
| `qwen`              | Qwen       | `https://chat.qwen.ai/`                       | Copy and paste            |
| `duckai`            | Duck.ai    | `https://duck.ai/`                            | Copy and paste            |
| `poe`               | Poe        | `https://poe.com/`                            | Copy and paste            |

The four prompt templates are best-effort browser conventions. Provider sign-in,
redirects and web updates can affect whether a prompt is filled or submitted.
Some services may submit immediately when you open the URL. If a page does not
fill the prompt, `:explain copy` provides the same local explanation request for
manual pasting. The other seven destinations deliberately contain no error text
in their URL; their error link is labeled **Open**, with a copy-and-paste hint.
No API key or API subscription is required by Prism.

For another service, save an HTTPS URL containing exactly one `{prompt}` marker:

```text
:ai custom https://chat.example.org/new?message={prompt}
```

Replace the example domain and path with the destination your service supports.
Prism inserts percent-encoded UTF-8 prompt text at that marker. The template
must have an ordinary HTTPS host, with no embedded credentials, whitespace or
tracking parameters. Switching to a built-in provider keeps the saved custom
template, so `:ai custom` or the menu's Custom entry can select it again.
If no custom template is saved, choosing Custom places `:ai custom ` in the
input field. Complete the HTTPS URL and submit it before the provider changes.
The Custom AI URL row in `:settings` also stages the template for editing.
You can inspect the registry or generate a prompt or link directly from R:

```r
prismrepl::repl_ai_providers()
prismrepl::ai_prompt("object 'x' not found", "mean(x)")
prismrepl::ai_link("object 'x' not found", "mean(x)", provider = "claude")
prismrepl::repl_settings(ai_provider = "chatgpt")
```

Prompt construction is entirely local. Prism does not open the browser, send a
request to a provider or write the clipboard merely because an error occurs.
For AI explanations, only `:explain copy` changes the clipboard. Opening a prompt URL
shares its contents with that provider and may retain them in browser history.
The generated prompt uses only the error, supplied source and captured calls;
it does not inspect workspace values or session history. Prompts are bounded
to 1800 characters by default and links to 8000 bytes. Truncation is marked.
To include only the error message by default:

```r
prismrepl::repl(config = prismrepl::repl_config(link_context = FALSE))
```

The URL conventions are cross-checked against the primary documentation for
the R package `searcher`: [Mistral](https://r-pkg.thecoatlessprofessor.com/searcher/reference/ask_mistral.html),
[Claude](https://r-pkg.thecoatlessprofessor.com/searcher/reference/ask_claude.html)
and [Perplexity](https://r-pkg.thecoatlessprofessor.com/searcher/reference/ask_perplexity.html).
The ChatGPT destination uses [OpenAI's current web address](https://help.openai.com/en/articles/9125172-the-chatgpt-home-page).
These links describe browser behavior; Prism has no service API integration.
Provider availability, account requirements and data handling remain under each
provider's control.

## Mouse selection and clipboard

With mouse handling enabled, drag over any visible transcript, editor, sidebar,
palette, object table, documentation page or file view to select its rendered
text. Releasing the button copies the selection. **Ctrl+drag** selects a
rectangle, which is useful for a table column. Copying strips terminal styling
and preserves complete Unicode graphemes. An ordinary short click retains its
contextual action, such as moving the editor cursor or staging a palette command.

Right-click pastes clipboard text into an editor, the palette filter, or a
viewer's search/filter field. Pasting never submits an expression or approves
a command. Read-only preview content is not modified. Selection is based on
currently visible cells; scroll first to reach other retained content.

Native Windows uses the Unicode system clipboard. Linux uses `wl-copy` /
`wl-paste` on Wayland, or `xclip` / `xsel` on X11 when installed. WSL can use
PowerShell's clipboard commands; macOS uses `pbcopy` / `pbpaste`. Helpers have
a short timeout, and large pasted text is checked against the input limit.

If there is no system clipboard helper, Prism retains a session copy and can
send an OSC 52 copy request to a supporting terminal. A terminal can reject
that request, so the status does not claim system-clipboard success unless it
is confirmed. Clipboard reading then uses the last Prism selection, or you can
use the terminal's own paste shortcut, commonly Ctrl+Shift+V.

`:mouse off` restores normal host mouse behavior. Windows Terminal also supports
[Shift+drag to use host selection while an application handles the mouse](https://learn.microsoft.com/en-us/windows/terminal/selection).
The host's own rectangular selection normally uses Alt, whereas Prism's
application selection uses Ctrl. Host-reserved gestures and a configured
right-click context menu can intercept events before R sees them.

## Themes and persistent preferences

On a first session with no saved or explicit theme, Prism reads the desktop's
appearance and chooses **paper for light** or **midnight for dark**. Unknown
appearance also uses midnight. Windows detection reads the current user's
`AppsUseLightTheme` preference. Ubuntu/GNOME detection reads `color-scheme` and
uses the GTK theme as a fallback. A best-effort macOS appearance reader and
conventional GTK or terminal background hints provide other local fallbacks.

Detection is read-only, cached within the R process, and uses no network or
terminal-input queries. Optional desktop commands have a one-second timeout.
Loading the package or constructing `repl_config()` never writes preferences.
At the first actual REPL startup, the resolved theme is saved when persistence
is enabled. Later sessions use that saved choice, even if the desktop changes.
An explicit theme always takes precedence over the saved one.

Enter `:theme`, or `:t`, to open the theme picker. The current theme is marked
and focused initially. Use Up/Down or a mouse click to focus another theme.
**Enter applies and saves the focused choice, then redraws the whole screen.**
Moving the focus alone leaves the active theme unchanged. Outside the search
field, Escape or `q` closes the picker without applying a change.

Press `/` to search the theme names and details. The shared viewer highlights
matches and supports `n`/`p` navigation. While the search field is active, Enter
finishes the search; press Enter again to apply the focused theme. Escape from
the search field cancels the search before returning to normal picker controls.

You can also apply a known choice directly with `:theme NAME`, for example
`:theme white`, or use `:theme auto` to read the desktop appearance again and
remember the detected theme. The picker's last entry, `auto (desktop appearance)`,
performs the same detection when accepted with Enter. In a plain interactive
console, the picker uses a numbered menu; enter a number to choose or `0` to
cancel. When invoked by
noninteractive tooling, it prints the theme list and a `:theme NAME` hint
without waiting for input.

| Theme           | Appearance                         |
| --------------- | ---------------------------------- |
| midnight        | Dark neutral background            |
| black           | Exact black main/editor background |
| ocean           | Dark blue palette                  |
| forest          | Dark green palette                 |
| solarized-dark  | Dark Solarized-inspired palette    |
| paper           | Soft light background              |
| white           | Exact white main/editor background |
| solarized-light | Light Solarized-inspired palette   |
| mono            | Monochrome                         |

Theme changes invalidate transcript, source, condition, link and frame caches,
then repaint the whole screen. Existing output adopts the new theme too. Semantic
roles can be customized through `repl_theme()`; built-in theme selections are
persisted, while custom role overrides belong in your explicit R configuration.

The full-screen prompt and function editor choose a white or black insertion
cursor to contrast with their painted background. This includes the exact-black
and exact-white themes and custom background overrides. The cursor color is
updated when the theme changes, with no repeated color writes while typing.
Read-only viewers hide the insertion cursor and show their selected row instead.
Inline and monochrome modes retain the terminal's own cursor color. Leaving a
colored full-screen session restores the terminal profile's configured cursor
color, while preserving its shape and blink settings. Cursor tinting uses the
standard OSC 12/112 controls. Use a current Windows Terminal version for both
color changes and restoration; Microsoft documents
[cursor-color reset support in the 1.23 series](https://github.com/microsoft/terminal/discussions/18843).
Terminal hosts that ignore these controls may need a contrasting cursor
configured in their appearance settings.

Theme, pipe, mouse, rainbow colors, cursor pair highlighting, automatic pairing, AI provider, custom AI
URL and shortcut changes are saved by default in
`file.path(tools::R_user_dir("prismrepl", "config"), "preferences.dcf")`.
`:settings` opens a preferences menu: select a setting to open its choices,
or select Preferences file to view the path. `:bind` and `:keys` show effective
shortcuts and stage the selected action's remapping command. Console layout
(`:ui`), sidebar visibility and status fields remain session-only. The file
contains data, not executable R code.
`PRISMREPL_CONFIG_FILE` overrides the path.

```text
:theme black
:theme auto
:pipe %>%
:ai claude
:rainbow on
:match on
:autopair on
:bind newline F4
:bind pipe Alt+P
:unbind history_search
:bind reset
:settings reset
```

`:settings reset` resets all persistent preferences together: a fresh desktop
theme choice, native pipe, mouse on, rainbow on, cursor pair highlighting on,
automatic pairing on, Mistral
AI, no custom URL, and the default keyboard shortcuts. `:bind reset` resets only
the shortcuts. Both commands honor the persistence settings for the session.

A chord assigned to a new action is removed from its previous action. You may
bind several chords, for example `:bind newline Ctrl+J F4`. `:keys` and the
footers show effective bindings. Shared viewer navigation follows those
bindings; mode-specific operations such as palette sorting or function-editor
saving have their own documented keys.

**Alt+Enter has no default Prism binding** because Windows Terminal uses it
for fullscreen. Ctrl+Shift+M may also be reserved by Windows Terminal's mark
mode; use Alt+M or your own mapping. A shortcut captured by the terminal cannot
reach R until its host binding is changed. See the
[Windows Terminal action reference](https://learn.microsoft.com/en-us/windows/terminal/customize-settings/actions).

Start with `prismrepl::repl()` to use saved choices. Explicit arguments to
`repl_config()` override them. `persist = FALSE` reads preferences but does not
save command changes or the initial detected theme; `settings_file = NULL`
disables both reading and writing.

```r
cfg <- prismrepl::repl_config(
  theme = "white",
  pipe = "%>%",
  status = c("directory", "git", "objects", "memory"),
  mouse = TRUE,
  rainbow = TRUE,
  match_pairs = TRUE,
  autopair = TRUE,
  ai_provider = "claude",
  rows = 12L,
  keybindings = list(newline = c("Ctrl+J", "F4"))
)
prismrepl::repl(config = cfg)
```

For a custom AI destination, use `ai_provider = "custom"` together with
`ai_url = "https://chat.example.org/new?message={prompt}"`. For defaults that
should persist without starting the REPL, call `prismrepl::repl_settings()`
with the corresponding arguments, for example
`prismrepl::repl_settings(rainbow = FALSE, match_pairs = TRUE, autopair = TRUE)`.

The appearance reader follows the platform's application preferences described
in [Windows app settings](https://learn.microsoft.com/en-us/windows/apps/develop/settings/settings-common)
and the [GNOME interface schema](https://mail.gnome.org/archives/commits-list/2021-September/msg06894.html).
An explicit [GTK_THEME override](https://docs.gtk.org/gtk4/running.html) is also
recognized on Linux. These are appearance hints; a terminal profile can choose
a different background, which you can match with `:theme`.

## Compatibility and practical limits

R 4.5.0 and newer use the same editor, rendering, completion and inspection
features. The native R 4.6 binding inspector is retained on R 4.6 and newer.
On R 4.5, a compatibility path uses exported metadata helpers and supported R
interfaces to distinguish delayed, active, missing and ordinary bindings. An
already-evaluated promise whose value is the missing-argument sentinel remains
a value. Inspection does not run active bindings or force delayed expressions.
The appropriate implementation is selected at compilation time.
Use a binary built for your R major/minor series, or compile the source archive
with that R version. The release workflow produces binaries for the current
R release; older R users can install the same source package.

Full-screen mode needs an interactive terminal with supported input and ANSI
output. `ui = "auto"` chooses it when available. `ui = "fullscreen"` requires
it; `ui = "inline"` keeps terminal scrollback and the native editor. Switch live
with `:ui inline` or `:ui fullscreen`. Basic mode uses the host's line editor:

```r
prismrepl::repl(config = prismrepl::repl_config(ui = "inline"), mode = "basic")
```

The terminal's size determines layout. Truecolor, 256-color, 16-color and plain
text paths are available. Automatic color detection respects `NO_COLOR`;
explicit `color = TRUE` overrides it. `unicode = FALSE` uses simple decoration
characters while retaining Unicode input. Use `:doctor` for diagnostics.

R evaluation is synchronous on its main thread. Ctrl+C can interrupt R code,
but a native extension that does not check interrupts can remain busy. Ordinary
R output and conditions are captured for the transcript. External programs,
direct writes to OS stderr, interactive menus, debuggers or some custom console
integrations can need inline mode. Package installation commands deliberately
use the ordinary terminal while their subprocesses run.

Terminal font width, emoji rendering, IME behavior and reserved shortcuts vary.
Prism measures graphemes and display cells, but an emulator's rendering can
differ for unusual glyph sequences. Clipboard access also depends on the
host and installed helpers. Tests in this repository distinguish simulated
screen cells, real POSIX PTYs and native Windows ConPTY coverage.

The GitHub workflows cover Linux, Windows and macOS, each on x86-64 and ARM64.
A configured job is not a claim that it has run locally. Review the current
validation record included in the source repository before publishing a release.

