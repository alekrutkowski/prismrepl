#' Configure the Interactive Console
#'
#' Saved appearance, editing, AI-provider and keyboard preferences supply defaults. Explicit
#' arguments take precedence. This function only reads preferences; it never
#' writes files or changes terminal modes, options, or environment variables.
#'
#' @param theme A built-in theme name, a theme returned by [repl_theme()], or
#'   `"auto"` to detect the operating system's light/dark appearance. With no
#'   saved theme, the first console session chooses `paper` for light appearance
#'   or `midnight` otherwise and remembers it when persistence is enabled.
#' @param pipe Pipe inserted by the pipe shortcut: `"|>"` or `"%>%"`.
#' @param color Logical; use color. `NULL` detects support and respects `NO_COLOR`.
#' @param unicode Logical; allow Unicode decorations. Input remains Unicode-aware.
#' @param history_file Optional history file. `NULL` keeps history in memory only.
#' @param history_limit Maximum number of history entries retained.
#' @param status Status fields: `process_memory`, `directory`, `git`, `objects`,
#'   `packages`, `memory`. `process_memory` is enabled by default and reports the
#'   current R process's resident RAM from the operating system, including native
#'   allocations and loaded libraries. `memory` separately sums approximate
#'   workspace object sizes. Neither measurement forces garbage collection.
#' @param preview Use bounded previews for ordinary objects (`"auto"`) or ordinary
#'   print methods (`"print"`). Custom S3/S4 classes retain their print methods.
#' @param rows Maximum rows shown in an automatic preview.
#' @param width Width for previews and documentation; `NULL` follows the terminal.
#' @param hyperlinks Logical; render OSC 8 links when supported.
#' @param confirm Logical; confirm batch removal and detachment in the console.
#' @param max_paste Maximum UTF-8 bytes in the editable expression, including
#'   typed text, paste, completion, pipe insertion, and recalled history.
#' @param max_lines Maximum logical lines in the editable expression. Soft
#'   wrapping to the terminal width does not count as additional logical lines.
#' @param trace_limit Maximum stack frames displayed for an error.
#' @param link_context Include source text and trace in generated AI prompts.
#'   Set to `FALSE` to include only the error message.
#' @param ui Layout: `"auto"` uses the full-screen interface in a supported
#'   terminal, `"fullscreen"` requires it, and `"inline"` keeps terminal scrollback.
#' @param sidebar Show the workspace sidebar when the window has enough space.
#' @param editor_height Maximum visible rows reserved for multiline editing.
#' @param transcript_limit Maximum retained transcript lines in full-screen mode.
#' @param color_depth Optional color depth: 0, 16, 256, or 16777216 (truecolor).
#'   `NULL` detects the terminal, including Windows Terminal. `color = FALSE`
#'   always disables colors; automatic color detection respects `NO_COLOR`.
#' @param settings_file Preferences path, normally [repl_settings_file()].
#'   `NULL` disables both reading and writing preferences.
#' @param persist Save appearance, editing, AI-provider and shortcut changes made by console
#'   commands. `FALSE` still reads existing preferences at startup.
#' @param keybindings Named shortcut overrides; see [repl_keybindings()].
#' @param mouse Enable clicks, drag selection, Ctrl-drag rectangles, copying,
#'   right-click pasting and wheel events in full-screen editors and viewers.
#'   Clipboard availability is reported by [repl_diagnostics()]. Turn this off
#'   for the terminal's usual mouse behavior.
#' @param synchronize Wrap native frame writes in synchronized-update sequences.
#'   Disabled by default; enable only when the terminal supports DEC mode 2026.
#' @param rainbow Color matching parentheses, brackets, braces and quote
#'   delimiters by nesting level. Requires color support. Defaults to `TRUE`.
#' @param autopair Automatically insert matching delimiters while typing R code.
#'   Paste, completion, recalled history and colon commands remain literal.
#'   Defaults to `TRUE`.
#' @param match_pairs Highlight both endpoints when the cursor is on or just
#'   after a matching bracket or quote, including compound raw-string delimiters.
#'   Defaults to `TRUE`, independently of `rainbow` and `autopair`.
#' @param ai_provider Browser destination for error explanations; see
#'   [repl_ai_providers()]. Defaults to `"mistral"`. No API key is needed.
#' @param ai_url Optional HTTPS template containing exactly one `{prompt}` for
#'   `ai_provider = "custom"`. The placeholder receives URL-encoded UTF-8 text.
#' @return A validated `prismrepl_config` list.
#' @export
#' @examples
#' cfg <- repl_config(theme = "midnight", pipe = "|>", status = "directory")
#' cfg$history_file
repl_config <- function(theme = "auto", pipe = "|>", color = NULL,
                        unicode = TRUE, history_file = NULL, history_limit = 1000L,
                        status = c("process_memory", "directory", "git"), preview = "auto", rows = 12L,
                        width = NULL, hyperlinks = TRUE, confirm = TRUE,
                        max_paste = 100000L, max_lines = 2000L, trace_limit = 12L,
                        link_context = TRUE, ui = "auto", sidebar = TRUE,
                        editor_height = 8L, transcript_limit = 4000L,
                        color_depth = NULL, settings_file = repl_settings_file(),
                        persist = TRUE, keybindings = NULL, mouse = TRUE,
                        synchronize = FALSE, rainbow = TRUE, autopair = TRUE,
                        ai_provider = "mistral", ai_url = NULL, match_pairs = TRUE) {
  stopifnot("settings_file must be NULL or a nonempty path" = is.null(settings_file) || .is_text(settings_file),
            "persist must be logical" = .is_flag(persist),
            "mouse must be logical" = .is_flag(mouse),
            "synchronize must be logical" = .is_flag(synchronize))
  preferences <- .read_preferences(settings_file, warn = interactive())
  theme_initial <- missing(theme) && is.null(preferences$theme)
  if (missing(theme) && !is.null(preferences$theme)) theme <- preferences$theme
  if (identical(theme, "auto")) theme <- .initial_theme(refresh = !missing(theme))
  if (missing(pipe) && !is.null(preferences$pipe)) pipe <- preferences$pipe
  if (missing(mouse) && !is.null(preferences$mouse)) mouse <- preferences$mouse
  if (missing(rainbow) && !is.null(preferences$rainbow)) rainbow <- preferences$rainbow
  if (missing(autopair) && !is.null(preferences$autopair)) autopair <- preferences$autopair
  if (missing(match_pairs) && !is.null(preferences$match_pairs)) match_pairs <- preferences$match_pairs
  if (missing(ai_provider) && !is.null(preferences$ai_provider)) ai_provider <- preferences$ai_provider
  if (missing(ai_url) && !is.null(preferences$ai_url)) ai_url <- preferences$ai_url
  .ai_target(ai_provider, ai_url)
  keybindings <- .merge_keybindings(if (is.null(preferences$keybindings))
    .default_keybindings() else preferences$keybindings, keybindings)
  stopifnot("pipe must be '|>' or '%>%'" = is.character(pipe) && length(pipe) == 1L &&
              !is.na(pipe) && pipe %in% c("|>", "%>%"),
            "color must be NULL or a logical scalar" = is.null(color) || .is_flag(color),
            "unicode must be a logical scalar" = .is_flag(unicode),
            "history_file must be NULL or a nonempty path" = is.null(history_file) ||
              .is_text(history_file),
            "history_limit must be a positive integer" = .is_count(history_limit, 1L),
            "status contains an unknown field" = is.character(status) && !anyNA(status) &&
              all(status %in% c("process_memory", "directory", "git", "objects", "packages", "memory")),
            "preview must be 'auto' or 'print'" = length(preview) == 1L &&
              preview %in% c("auto", "print"),
            "rows must be a positive integer" = .is_count(rows, 1L),
            "width must be NULL or an integer of at least 20" = is.null(width) ||
              .is_count(width, 20L),
            "hyperlinks must be a logical scalar" = .is_flag(hyperlinks),
            "confirm must be a logical scalar" = .is_flag(confirm),
            "max_paste must be a positive integer" = .is_count(max_paste, 1L),
            "max_lines must be a positive integer" = .is_count(max_lines, 1L),
            "trace_limit must be a nonnegative integer" = .is_count(trace_limit),
            "link_context must be a logical scalar" = .is_flag(link_context),
            "rainbow must be a logical scalar" = .is_flag(rainbow),
            "autopair must be a logical scalar" = .is_flag(autopair),
            "match_pairs must be a logical scalar" = .is_flag(match_pairs),
            "ui must be auto, fullscreen, or inline" = .is_text(ui) &&
              ui %in% c("auto", "fullscreen", "inline"),
            "sidebar must be a logical scalar" = .is_flag(sidebar),
            "editor_height must be an integer of at least two" = .is_count(editor_height, 2L),
            "transcript_limit must be an integer of at least twenty" = .is_count(transcript_limit, 20L),
            "color_depth must be NULL, 0, 16, 256, or 16777216" = is.null(color_depth) ||
              (.is_count(color_depth) && color_depth %in% c(0L, 16L, 256L, 16777216L)))
  if (is.character(theme)) theme <- repl_theme(theme)
  stopifnot("theme must be a valid theme" = is.list(theme) && .is_text(theme$name) &&
              is.character(theme$colors) && !anyNA(theme$colors) &&
              all(setdiff(names(repl_theme()$colors), "pair_match") %in% names(theme$colors)) &&
              all(grepl("^[0-9;]*$", theme$colors)))
  color_auto <- is.null(color)
  color_depth_auto <- is.null(color_depth)
  color_depth_requested <- color_depth
  color_depth <- .console_color_depth(color = color, requested = color_depth)
  color <- color_depth > 0L
  theme <- .theme_at_depth(theme, color_depth)
  out <- structure(list(theme = theme, pipe = pipe, color = color, unicode = unicode,
    history_file = history_file, history_limit = as.integer(history_limit),
    status = unique(status), preview = preview, rows = as.integer(rows),
    width = width, hyperlinks = hyperlinks, confirm = confirm,
    max_paste = as.integer(max_paste), max_lines = as.integer(max_lines),
    trace_limit = as.integer(trace_limit), link_context = link_context,
    ui = ui, sidebar = sidebar, editor_height = as.integer(editor_height),
    transcript_limit = as.integer(transcript_limit), color_depth = color_depth,
    color_auto = color_auto, color_depth_auto = color_depth_auto,
    color_depth_requested = color_depth_requested,
    settings_file = if (is.null(settings_file)) NULL else path.expand(settings_file),
    persist = persist, keybindings = keybindings, key_lookup = .key_lookup(keybindings),
    mouse = mouse, synchronize = synchronize, rainbow = rainbow, autopair = autopair,
    match_pairs = match_pairs,
    ai_provider = ai_provider, ai_url = ai_url, theme_initial = theme_initial),
    class = c("prismrepl_config", "list"))
  stopifnot("configuration output must be a named list" = is.list(out) &&
              !is.null(names(out)) && inherits(out, "prismrepl_config"))
  out
}

#' Resolve Console Colors from Explicit Settings and Terminal Evidence
#' @param caps Native capability list.
#' @param variables Named environment-variable values, injectable in tests.
#' @param detected Color count detected by cli.
#' @param color Explicit logical color choice or NULL for automatic detection.
#' @param requested Explicit color depth or NULL.
#' @return Integer color depth, one of 0, 16, 256, or 16777216.
#' @keywords internal
#' @noRd
.console_color_depth <- function(caps = term_capabilities(),
    variables = Sys.getenv(c("NO_COLOR", "WT_SESSION", "WT_PROFILE_ID", "COLORTERM", "TERM")),
    detected = cli::num_ansi_colors(), color = NULL, requested = NULL) {
  stopifnot("capabilities must be a list" = is.list(caps),
            "variables must be named character values" = is.character(variables) &&
              !is.null(names(variables)) && !anyNA(variables),
            "detected colors must be a nonnegative finite number" =
              is.numeric(detected) && length(detected) == 1L && is.finite(detected) && detected >= 0,
            "color must be NULL or logical" = is.null(color) || .is_flag(color),
            "requested depth is invalid" = is.null(requested) ||
              (.is_count(requested) && requested %in% c(0L, 16L, 256L, 16777216L)))
  value <- function(name) if (name %in% names(variables)) variables[[name]] else ""
  modern <- nzchar(value("WT_SESSION")) || nzchar(value("WT_PROFILE_ID")) ||
    tolower(value("COLORTERM")) %in% c("truecolor", "24bit") || isTRUE(caps$truecolor)
  out <- if (identical(color, FALSE) || (is.null(color) && nzchar(value("NO_COLOR")))) 0L
  else if (!is.null(requested)) as.integer(requested)
  else if (isTRUE(caps$supported) && modern) 16777216L
  else if (detected >= 16777216) 16777216L
  else if (detected >= 256 || (isTRUE(caps$supported) && grepl("256color", value("TERM")))) 256L
  else if (detected > 1 || identical(color, TRUE) ||
           (isTRUE(caps$supported) && !identical(value("TERM"), "dumb"))) 16L
  else 0L
  stopifnot("resolved color depth must be supported" =
              is.integer(out) && length(out) == 1L && out %in% c(0L, 16L, 256L, 16777216L))
  out
}

#' Refresh Automatic Color Settings at Session Startup
#' @param config Console configuration.
#' @param caps Native capabilities.
#' @return A configuration with a matching palette and color depth.
#' @keywords internal
#' @noRd
.resolve_console_config <- function(config, caps = term_capabilities()) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"),
            "capabilities must be a list" = is.list(caps))
  config$color_depth <- .console_color_depth(caps,
    color = if (isTRUE(config$color_auto)) NULL else config$color,
    requested = if (isTRUE(config$color_depth_auto)) NULL else config$color_depth_requested)
  config$color <- config$color_depth > 0L
  config$theme <- .theme_at_depth(config$theme, config$color_depth)
  stopifnot("resolved configuration must retain its class" = inherits(config, "prismrepl_config"))
  config
}

#' Test a Logical Scalar
#' @param x An object.
#' @return A logical scalar.
#' @keywords internal
#' @noRd
.is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

#' Test a Nonempty Character Scalar
#' @param x An object.
#' @return A logical scalar.
#' @keywords internal
#' @noRd
.is_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

#' Test a Bounded Integer Scalar
#' @param x An object.
#' @param lower Minimum value.
#' @return A logical scalar.
#' @keywords internal
#' @noRd
.is_count <- function(x, lower = 0L) is.numeric(x) && length(x) == 1L &&
  is.finite(x) && x >= lower && x <= .Machine$integer.max && x == trunc(x)

#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  .register_cran_cleanup()
}

#' @keywords internal
#' @noRd
.onUnload <- function(libpath) {
  try(.cleanup_cran_workers(wait = TRUE), silent = TRUE)
  try(term_reset(output = TRUE), silent = TRUE)
  library.dynam.unload("prismrepl", libpath)
}
