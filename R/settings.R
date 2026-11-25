#' Locate the Per-User Console Preferences
#'
#' Preferences are plain DCF data, never executable R code. The default location
#' is `preferences.dcf` under `tools::R_user_dir("prismrepl", "config")`.
#' Set `PRISMREPL_CONFIG_FILE` to use another file. Merely locating or reading
#' preferences does not create directories or files.
#'
#' @return One absolute or user-supplied path.
#' @export
#' @examples
#' repl_settings_file()
repl_settings_file <- function() {
  out <- Sys.getenv("PRISMREPL_CONFIG_FILE", unset = "")
  if (!nzchar(out)) out <- file.path(tools::R_user_dir("prismrepl", "config"), "preferences.dcf")
  out <- path.expand(out)
  stopifnot("settings path must be nonempty" = .is_text(out))
  out
}

#' Configure Persistent Console Preferences
#'
#' With no changes, read the saved preferences. Supplying a theme, key bindings,
#' pipe, mouse, editing or AI-provider preference validates and saves the change.
#' Console commands `:theme`, `:bind`, `:unbind`, `:pipe`, `:mouse`, `:rainbow`,
#' `:autopair`, `:match` and `:ai` use the same file.
#' Explicit arguments to [repl_config()] override saved values for that session.
#' Use `repl_config(settings_file = NULL)` for a fully temporary configuration,
#' or `persist = FALSE` to read preferences without saving console changes.
#'
#' Bindings are named lists mapping action identifiers to one or more chords.
#' Reassigning a chord removes it from its previous action. An empty character
#' vector disables an action. The `:bind` command lists action identifiers;
#' `:bind newline Ctrl+J` assigns a chord and `:bind reset` restores defaults.
#' Terminal or operating-system shortcuts must reach R before they can work.
#'
#' @param theme Optional built-in theme name.
#' @param keybindings Optional named binding overrides; see [repl_keybindings()].
#' @param pipe Optional `"|>"` or `"%>%"`.
#' @param mouse Optional logical mouse preference.
#' @param file Preferences file. It is created only when saving a change.
#' @param reset Logical; reset saved preferences before applying any changes.
#' @param rainbow Optional logical preference for matching delimiter colors.
#' @param autopair Optional logical preference for automatic delimiter insertion.
#' @param match_pairs Optional logical preference for highlighting the matching
#'   bracket or quote pair at the cursor, including compound raw-string quotes.
#' @param ai_provider Optional provider identifier from [repl_ai_providers()],
#'   or `"custom"` with an `ai_url` template.
#' @param ai_url Optional HTTPS URL template containing exactly one `{prompt}`.
#'   It is retained when switching to a built-in provider for convenient reuse.
#' @return A list of saved preferences. Unset preferences are absent.
#' @export
#' @examples
#' path <- tempfile(fileext = ".dcf")
#' invisible(repl_settings(theme = "white", keybindings = list(newline = "Ctrl+J"), file = path))
#' repl_config(settings_file = path)$theme$name
#' unlink(path)
repl_settings <- function(theme = NULL, keybindings = NULL, pipe = NULL,
                         mouse = NULL, file = repl_settings_file(), reset = FALSE,
                         rainbow = NULL, autopair = NULL, ai_provider = NULL,
                         ai_url = NULL, match_pairs = NULL) {
  stopifnot("file must be a nonempty path" = .is_text(file),
            "reset must be logical" = .is_flag(reset),
            "theme must be NULL or a built-in theme" = is.null(theme) ||
              (.is_text(theme) && theme %in% repl_themes()),
            "pipe must be NULL, |> or %>%" = is.null(pipe) ||
              (.is_text(pipe) && pipe %in% c("|>", "%>%")),
            "mouse must be NULL or logical" = is.null(mouse) || .is_flag(mouse),
            "rainbow must be NULL or logical" = is.null(rainbow) || .is_flag(rainbow),
            "autopair must be NULL or logical" = is.null(autopair) || .is_flag(autopair),
            "match_pairs must be NULL or logical" = is.null(match_pairs) || .is_flag(match_pairs))
  bindings <- if (is.null(keybindings)) NULL else repl_keybindings(keybindings)
  out <- if (reset) list() else .read_preferences(file, warn = FALSE, strict = TRUE)
  if (!is.null(theme)) out$theme <- theme
  if (!is.null(pipe)) out$pipe <- pipe
  if (!is.null(mouse)) out$mouse <- mouse
  if (!is.null(rainbow)) out$rainbow <- rainbow
  if (!is.null(autopair)) out$autopair <- autopair
  if (!is.null(match_pairs)) out$match_pairs <- match_pairs
  if (!is.null(ai_provider)) out$ai_provider <- ai_provider
  if (!is.null(ai_url)) out$ai_url <- ai_url
  .ai_target(if (is.null(out$ai_provider)) "mistral" else out$ai_provider, out$ai_url)
  if (!is.null(bindings)) out$keybindings <- .merge_keybindings(
    if (is.null(out$keybindings)) .default_keybindings() else out$keybindings, keybindings)
  if (reset || !all(vapply(list(theme, keybindings, pipe, mouse, rainbow, autopair,
                               ai_provider, ai_url, match_pairs), is.null, logical(1))))
    .write_preferences(out, file)
  stopifnot("preferences must be a named list or an empty list" = is.list(out) &&
              (!length(out) || !is.null(names(out))))
  out
}

#' Build the Effective Keyboard Bindings
#'
#' The default explicit newline shortcut is Ctrl+J, with Shift+Enter as an alias
#' on terminals that distinguish it. Alt+Enter is reserved by several terminals
#' and is not assigned. Enter submits complete code and continues incomplete
#' expressions. Ctrl+Shift+M and Alt+M insert the selected pipe.
#'
#' @param overrides Optional named list mapping action identifiers to character
#'   vectors of chords. For example, `list(newline = "F4")`. A character vector
#'   with action names is also accepted. Chord reassignment removes the previous
#'   assignment. Conflicting chords within the same set of overrides are errors.
#' @return A complete named list of validated keyboard chords.
#' @export
#' @examples
#' repl_keybindings()[c("newline", "pipe", "history_search")]
#' repl_keybindings(list(newline = "F4"))$newline
repl_keybindings <- function(overrides = NULL) {
  out <- .merge_keybindings(.default_keybindings(), overrides)
  stopifnot("bindings must have unique action names" = is.list(out) &&
              !is.null(names(out)) && !anyDuplicated(names(out)))
  out
}

#' Default Keyboard Actions
#' @return A named list of chords.
#' @keywords internal
#' @noRd
.default_keybindings <- function() list(
  submit = "Enter", newline = c("Ctrl+J", "Shift+Enter"),
  complete = "Tab", complete_previous = "Shift+Tab",
  pipe = c("Ctrl+Shift+M", "Alt+M"), cancel = "Ctrl+C", delete_or_exit = "Ctrl+D",
  backspace = "Backspace", delete = "Delete", left = "Left", right = "Right",
  word_left = c("Ctrl+Left", "Alt+Left"), word_right = c("Ctrl+Right", "Alt+Right"),
  line_start = "Home", line_end = "End", buffer_start = c("Ctrl+A", "Ctrl+Home"),
  buffer_end = c("Ctrl+E", "Ctrl+End"), history_previous = "Ctrl+P", history_next = "Ctrl+N",
  up = "Up", down = "Down", kill_start = "Ctrl+U", kill_end = "Ctrl+K",
  kill_word = "Ctrl+W", yank = "Ctrl+Y", history_search = "Ctrl+R", function_help = "F1", help = "F2",
  escape = "Escape", redraw = "Ctrl+L", scroll_up = "PageUp", scroll_down = "PageDown",
  scroll_line_up = "Alt+Up", scroll_line_down = "Alt+Down")

#' Normalize a User-Entered Keyboard Chord
#' @param chord One chord such as ctrl+j.
#' @return A canonical chord.
#' @keywords internal
#' @noRd
.normalize_chord <- function(chord) {
  stopifnot("each shortcut must be a nonempty chord" = .is_text(chord))
  stopifnot("shortcut cannot end in a separator; spell the plus key as Plus" =
              !endsWith(trimws(chord), "+"))
  parts <- strsplit(tolower(gsub("[[:space:]]+", "", chord)), "+", fixed = TRUE)[[1L]]
  stopifnot("shortcut must include a key" = length(parts) > 0L && all(nzchar(parts)))
  key <- tail(parts, 1L)
  mods <- head(parts, -1L)
  mods[mods == "control"] <- "ctrl"
  stopifnot("shortcut modifiers must be Ctrl, Alt or Shift" =
              all(mods %in% c("ctrl", "alt", "shift")) && !anyDuplicated(mods))
  aliases <- c(return = "enter", esc = "escape", pgup = "pageup", pgdn = "pagedown",
               del = "delete", ins = "insert", minus = "-", plus = "+", space = " ")
  if (key %in% names(aliases)) key <- aliases[[key]]
  allowed <- c(letters, as.character(0:9), paste0("f", 1:24), "enter", "tab", "escape",
    "backspace", "delete", "insert", "left", "right", "up", "down", "home", "end",
    "pageup", "pagedown", "-", "+", "=", " ", ".", ",", ";", "'", "/", "[", "]", "\\")
  stopifnot("shortcut key is unsupported" = key %in% allowed,
            "printable shortcuts need Ctrl or Alt" =
              !(key %in% c(letters, as.character(0:9), "-", "+", "=", " ", ".", ",", ";", "'", "/", "[", "]", "\\")) ||
                any(mods %in% c("ctrl", "alt")))
  out <- .chord_from_parts(key, mods)
  stopifnot("normalized chord must be text" = .is_text(out))
  out
}

#' Create a Canonical Keyboard Chord
#' @param key Native key identifier.
#' @param modifiers Native modifier identifiers.
#' @return One canonical chord, or an empty string for no key.
#' @keywords internal
#' @noRd
.chord_from_parts <- function(key, modifiers = character()) {
  if (!length(key) || !nzchar(key)) return("")
  special <- c(enter = "Enter", tab = "Tab", escape = "Escape", backspace = "Backspace",
    delete = "Delete", insert = "Insert", left = "Left", right = "Right", up = "Up", down = "Down",
    home = "Home", end = "End", pageup = "PageUp", pagedown = "PageDown", " " = "Space", "+" = "Plus")
  key <- if (key %in% names(special)) special[[key]] else toupper(key)
  prefixes <- c(ctrl = "Ctrl", alt = "Alt", shift = "Shift")
  paste(c(unname(prefixes[names(prefixes) %in% modifiers]), key), collapse = "+")
}

#' Merge Binding Overrides and Reassign Colliding Chords
#' @param base Complete bindings.
#' @param overrides Optional named overrides.
#' @return Complete normalized bindings.
#' @keywords internal
#' @noRd
.merge_keybindings <- function(base, overrides) {
  stopifnot("base bindings must be named" = is.list(base) && !is.null(names(base)))
  if (is.null(overrides) || !length(overrides)) return(base)
  stopifnot("bindings must be a named list or character vector" =
              (is.list(overrides) || is.character(overrides)) && !is.null(names(overrides)) &&
              !anyNA(names(overrides)) && !anyDuplicated(names(overrides)),
            "unknown shortcut action; use :bind to list actions" =
              all(names(overrides) %in% names(.default_keybindings())))
  overrides <- lapply(as.list(overrides), function(chords) {
    stopifnot("each action must contain character chords" = is.character(chords) && !anyNA(chords))
    unique(vapply(chords, .normalize_chord, character(1), USE.NAMES = FALSE))
  })
  assigned <- unlist(overrides, use.names = FALSE)
  stopifnot("a shortcut cannot be assigned to multiple actions" = !anyDuplicated(assigned))
  out <- lapply(base, setdiff, y = assigned)
  out[names(overrides)] <- overrides
  stopifnot("merged chords must be unique" = !anyDuplicated(unlist(out, use.names = FALSE)))
  out
}

#' Build a Fast Chord Lookup
#' @param bindings Complete bindings.
#' @return A named character vector of action identifiers.
#' @keywords internal
#' @noRd
.key_lookup <- function(bindings) {
  out <- stats::setNames(rep(names(bindings), lengths(bindings)), unlist(bindings, use.names = FALSE))
  stopifnot("lookup chords must be unique" = !anyDuplicated(names(out)))
  out
}

#' Resolve One Native Event to a Configured Action
#' @param event Native event record.
#' @param config Console configuration.
#' @return An action identifier or an empty string.
#' @keywords internal
#' @noRd
.key_action <- function(event, config) {
  if (!event$type %in% c("key", "text") || !length(event$key) || !nzchar(event$key)) return("")
  chord <- .chord_from_parts(event$key, event$modifiers)
  lookup <- config$key_lookup
  if (is.null(lookup)) lookup <- .key_lookup(if (is.null(config$keybindings))
    .default_keybindings() else config$keybindings)
  found <- match(chord, names(lookup), nomatch = 0L)
  if (found) unname(lookup[[found]]) else ""
}

#' Tabulate Effective Shortcuts
#' @param config Console configuration.
#' @return Identifier, key, and description columns.
#' @keywords internal
#' @noRd
.keybinding_table <- function(config) {
  descriptions <- c(
    submit = "Run complete code; continue an incomplete expression.",
    newline = "Insert a newline without running code.", complete = "Complete a name or cycle forward.",
    complete_previous = "Cycle completions backward.", pipe = "Insert the selected |> or %>% pipe.",
    cancel = "Cancel the current input; Ctrl+C also interrupts evaluation.",
    delete_or_exit = "Delete the next character; exit when input is empty.",
    backspace = "Delete the previous grapheme.", delete = "Delete the next grapheme.",
    left = "Move left by one grapheme.", right = "Move right by one grapheme.",
    word_left = "Move to the previous word.", word_right = "Move to the next word.",
    line_start = "Move to the start of this line.", line_end = "Move to the end of this line.",
    buffer_start = "Move to the start of the expression.", buffer_end = "Move to the end of the expression.",
    history_previous = "Recall the previous whole expression.", history_next = "Recall the next whole expression.",
    up = "Move up, or recall history at the first line.", down = "Move down, or recall history at the last line.",
    kill_start = "Cut to the start of this line.", kill_end = "Cut to the end of this line.",
    kill_word = "Cut the previous word.", yank = "Paste the last cut text.",
    history_search = "Search previous expressions; repeat to cycle matches.",
    function_help = "Open documentation for the function name under the cursor.",
    help = "Open the shortcut cheatsheet.",
    escape = "Dismiss completion, search or help.", redraw = "Redraw the screen.",
    scroll_up = "Scroll the transcript or viewer up by one page.",
    scroll_down = "Scroll the transcript or viewer down by one page.",
    scroll_line_up = "Scroll the transcript up by one line.", scroll_line_down = "Scroll it down by one line.")
  bindings <- if (is.null(config$keybindings)) .default_keybindings() else config$keybindings
  out <- data.frame(id = names(bindings), key = vapply(bindings, function(x)
    if (length(x)) paste(x, collapse = " / ") else "(unbound)", character(1)),
    action = unname(descriptions[names(bindings)]), row.names = NULL, stringsAsFactors = FALSE)
  stopifnot("each shortcut must have a description" = !anyNA(out$action) && nrow(out) == length(bindings))
  out
}

#' Read Bounded Data-Only Preferences
#' @param file Path or NULL to disable preferences.
#' @param warn Warn about an invalid preferences file.
#' @param strict Raise an error instead of falling back.
#' @return Validated preferences or an empty list.
#' @keywords internal
#' @noRd
.read_preferences <- function(file, warn = FALSE, strict = FALSE) {
  if (is.null(file) || !file.exists(file)) return(list())
  out <- tryCatch({
    size <- file.info(file)$size
    stopifnot("preferences file exceeds 64 KiB" = is.finite(size) && size <= 65536L)
    data <- read.dcf(file, all = TRUE)
    stopifnot("preferences must contain one version-1 record" = nrow(data) == 1L &&
                "Version" %in% names(data) && identical(data$Version[[1L]], "1"))
    fields <- as.list(data[1L, , drop = FALSE])
    result <- list()
    if (!is.null(fields$Theme)) {
      stopifnot("unknown saved theme" = .is_text(fields$Theme) && fields$Theme %in% repl_themes())
      result$theme <- fields$Theme
    }
    if (!is.null(fields$Pipe)) {
      stopifnot("invalid saved pipe" = .is_text(fields$Pipe) && fields$Pipe %in% c("|>", "%>%"))
      result$pipe <- fields$Pipe
    }
    flags <- c(Mouse = "mouse", Rainbow = "rainbow", Autopair = "autopair", "Match-Pairs" = "match_pairs")
    supplied <- intersect(names(flags), names(fields))
    values <- lapply(fields[supplied], function(value) {
      stopifnot("invalid saved logical preference" = .is_text(value) && value %in% c("true", "false"))
      identical(value, "true")
    })
    result[unname(flags[supplied])] <- values
    if (!is.null(fields[["AI-Provider"]])) result$ai_provider <- fields[["AI-Provider"]]
    if (!is.null(fields[["AI-URL"]])) result$ai_url <- fields[["AI-URL"]]
    .ai_target(if (is.null(result$ai_provider)) "mistral" else result$ai_provider, result$ai_url)
    key_fields <- grep("^Key-", names(fields), value = TRUE)
    if (length(key_fields)) {
      overrides <- lapply(fields[key_fields], function(x)
        if (identical(x, "none")) character() else trimws(strsplit(x, " | ", fixed = TRUE)[[1L]]))
      names(overrides) <- sub("^Key-", "", key_fields)
      # Older releases saved the whole keymap, including the former F1 default.
      # Upgrade that default while preserving any explicitly reassigned F2.
      if (!"function_help" %in% names(overrides) && identical(overrides$help, "F1")) {
        occupied <- unlist(overrides[setdiff(names(overrides), "help")], use.names = FALSE)
        occupied <- vapply(occupied, .normalize_chord, character(1L), USE.NAMES = FALSE)
        overrides$help <- if ("F2" %in% occupied) character() else "F2"
        overrides$function_help <- "F1"
      }
      result$keybindings <- repl_keybindings(overrides)
    }
    result
  }, error = function(e) {
    message <- paste0("Preferences could not be read from ", file, ": ", conditionMessage(e))
    if (strict) stop(message, call. = FALSE)
    if (warn) warning(paste(message, "Using defaults."), call. = FALSE)
    list()
  })
  stopifnot("read preferences must be a list" = is.list(out))
  out
}

#' Save Preferences Using a Staged File
#' @param preferences Validated preference list.
#' @param file Destination.
#' @return Destination invisibly.
#' @keywords internal
#' @noRd
.write_preferences <- function(preferences, file) {
  stopifnot("preferences must be a list" = is.list(preferences), "file must be a path" = .is_text(file))
  file <- path.expand(file)
  directory <- dirname(file)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, mode = "0700"))
    stop("Could not create the preferences directory.", call. = FALSE)
  fields <- c(Version = "1", if (!is.null(preferences$theme)) c(Theme = preferences$theme),
    if (!is.null(preferences$pipe)) c(Pipe = preferences$pipe),
    if (!is.null(preferences$mouse)) c(Mouse = if (preferences$mouse) "true" else "false"),
    if (!is.null(preferences$rainbow)) c(Rainbow = if (preferences$rainbow) "true" else "false"),
    if (!is.null(preferences$autopair)) c(Autopair = if (preferences$autopair) "true" else "false"),
    if (!is.null(preferences$match_pairs)) c("Match-Pairs" = if (preferences$match_pairs) "true" else "false"),
    if (!is.null(preferences$ai_provider)) c("AI-Provider" = preferences$ai_provider),
    if (!is.null(preferences$ai_url)) c("AI-URL" = preferences$ai_url))
  if (!is.null(preferences$keybindings)) fields <- c(fields,
    stats::setNames(vapply(preferences$keybindings, function(x)
      if (length(x)) paste(x, collapse = " | ") else "none", character(1)),
      paste0("Key-", names(preferences$keybindings))))
  staged <- tempfile(".prism-preferences-", tmpdir = directory)
  backup <- tempfile(".prism-preferences-backup-", tmpdir = directory)
  keep_backup <- FALSE
  on.exit(unlink(c(staged, if (!keep_backup) backup)), add = TRUE)
  write.dcf(as.data.frame(as.list(fields), stringsAsFactors = FALSE, check.names = FALSE), staged, width = 100000L)
  Sys.chmod(staged, mode = "0600")
  moved_old <- FALSE
  if (file.exists(file)) {
    if (!file.copy(file, backup, overwrite = FALSE, copy.mode = TRUE))
      stop("Could not stage the existing preferences for recovery.", call. = FALSE)
  }
  saved <- suppressWarnings(file.rename(staged, file))
  if (!saved && file.exists(file) && file.exists(backup)) {
    moved_old <- file.remove(file)
    saved <- isTRUE(moved_old) && suppressWarnings(file.rename(staged, file))
  }
  if (!saved) {
    if (moved_old && file.exists(backup))
      keep_backup <- !isTRUE(file.copy(backup, file, overwrite = TRUE))
    stop(if (keep_backup) paste("Could not restore preferences; recovery copy:", backup) else
      "Could not save preferences; the previous file was retained.", call. = FALSE)
  }
  stopifnot("saved preferences must exist" = file.exists(file))
  invisible(file)
}

#' Save Selected Session Preferences Without Interrupting the Session
#' @param config Console configuration.
#' @param fields Names of preference fields to save.
#' @param reset Discard the old saved record before applying these fields.
#' @return Whether the preference was saved, invisibly.
#' @keywords internal
#' @noRd
.persist_preferences <- function(config, fields, reset = FALSE) {
  stopifnot("configuration must be a list" = is.list(config),
            "unknown preference field" = all(fields %in% c("theme", "pipe", "mouse", "keybindings",
                                                          "rainbow", "autopair", "match_pairs", "ai_provider", "ai_url")))
  if (!isTRUE(config$persist) || is.null(config$settings_file)) return(invisible(FALSE))
  values <- config[fields]
  if ("theme" %in% fields) values$theme <- config$theme$name
  out <- tryCatch({ do.call(repl_settings, c(values, list(file = config$settings_file, reset = reset))); TRUE },
    error = function(e) {
      .emit(paste("Setting changed for this session; saving failed:", conditionMessage(e)), "warning", config)
      FALSE
    })
  invisible(out)
}

#' Remember the First Session's Detected Theme Without Replacing a Later Choice
#' @param config Console configuration, resolved before actual REPL startup.
#' @return Whether the initial theme was saved, invisibly.
#' @noRd
.remember_initial_theme <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  out <- FALSE
  if (isTRUE(config$theme_initial) && isTRUE(config$persist) && !is.null(config$settings_file) &&
      is.null(.read_preferences(config$settings_file)$theme))
    out <- .persist_preferences(config, "theme")
  stopifnot("initial-theme save result must be logical" = .is_flag(out))
  invisible(out)
}
