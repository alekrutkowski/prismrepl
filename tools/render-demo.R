#!/usr/bin/env Rscript
# Reproduce the real compositor frames used for visual review.
# From the repository root: Rscript tools/render-demo.R
# Then: python tools/ansi-to-png.py tools/figures/*.json
# Requires an installed prismrepl native backend and jsonlite. The current R/
# sources are loaded into a private environment, without recompiling anything.

args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
package_version <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[[1L]]
destination <- if (length(args)) args[[1L]] else file.path(root, "tools", "figures")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
stopifnot("jsonlite must be installed for the developer exporter" =
            requireNamespace("jsonlite", quietly = TRUE),
          "prismrepl must be installed for its native backend" =
            requireNamespace("prismrepl", quietly = TRUE))
ns <- new.env(parent = asNamespace("prismrepl"))
invisible(lapply(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
                 sys.source, envir = ns))
# Explicit color = TRUE must work even when a noninteractive runner reports no
# color support. This also reproduces the color-stripping regression check.
options(cli.num_colors = 1L)
Sys.setenv(NO_COLOR = "1")

#' Build a Reproducible Evaluated Session for Visual Review
#' @param theme Built-in theme name.
#' @return Console configuration containing a real captured session.
#' @noRd
demo_session <- function(theme = "midnight") {
  stopifnot("theme must be a built-in name" = theme %in% ns$repl_themes())
  cfg <- ns$repl_config(theme = theme, color = TRUE, color_depth = 16777216L,
    unicode = TRUE, width = NULL, ui = "fullscreen", hyperlinks = TRUE,
    rows = 3L, status = c("process_memory", "objects", "memory", "packages"), trace_limit = 4L,
    settings_file = NULL)
  cfg$screen <- ns$screen_state()
  cfg$screen$active <- TRUE
  cfg$highlight_cache <- new.env(parent = emptyenv())
  env <- new.env(parent = baseenv())
  setup <- c(
    "flowers <- datasets::iris",
    "flowers$Sepal.Width[c(2L, 7L)] <- NA_real_",
    'label <- "Flower measurements: \u03bc, \u03c3, caf\u00e9"',
    'check_response <- function(x) {',
    '  if (is.null(x)) stop("Response column \'Class\' was not found. Use \'Species\'.", call. = FALSE)',
    '  x',
    '}',
    'fit_flowers <- function(x) check_response(x$Class)',
    'inspect_flowers <- function(x) {',
    '  print(utils::head(x, 3L))',
    '  message("150 rows loaded; UTF-8 labels ready: \u03bc, \u03c3, caf\u00e9.")',
    '  if (anyNA(x)) warning("2 Sepal.Width values are missing; results omit incomplete rows.", call. = FALSE)',
    '  fit_flowers(x)',
    '}'
  )
  setup_result <- ns$evaluate_r(setup, env, cfg, display = FALSE)
  stopifnot("demo setup must evaluate without errors" = is.null(setup_result$error))
  code <- "inspect_flowers(flowers)"
  source <- paste0(ns$style_text("r> ", "prompt", cfg$theme, TRUE),
                   ns$highlight_r(code, cfg$theme, TRUE))
  ns$screen_append(cfg$screen, source, "input", ansi = TRUE)
  result <- ns$.with_screen_output(function(local) ns$evaluate_r(code, env, local),
                                   cfg, paint = FALSE)
  cfg$screen$mode <- "ERROR"
  inventory <- ns$workspace_objects(env, sizes = "memory" %in% cfg$status)
  process_memory <- ns$.process_memory()
  cfg$screen$sidebar <- ns$.session_sidebar(env, cfg, inventory, process_memory)
  cfg$screen$status <- ns$.session_status(env, cfg, inventory, process_memory)
  stopifnot("demo must capture all three condition types" =
              length(result$messages) == 1L && length(result$warnings) == 1L &&
              inherits(result$error, "error"),
            "demo must preserve the nested user calls" = length(result$trace) >= 3L)
  cfg
}

#' Export a Frame and Its Actual Grapheme Widths
#' @param config Configuration containing the demo screen.
#' @param width,height Terminal columns and rows.
#' @param name File basename.
#' @param text Optional source buffer; NULL uses the general console example.
#' @param cursor Optional zero-based grapheme cursor position in the buffer.
#' @return A frame with exact terminal dimensions.
#' @noRd
export_frame <- function(config, width, height, name, text = NULL, cursor = NULL) {
  stopifnot("width and height must be positive integers" =
              ns$.is_count(width, 1L) && ns$.is_count(height, 1L),
            "name must be scalar text" = ns$.is_text(name),
            "text must be NULL or one string" = is.null(text) || ns$.is_text(text))
  if (is.null(text)) text <- paste(c("flowers |>",
    "  subset(!is.na(Sepal.Width)) |>",
    "  transform(ratio = Sepal.Length / Sepal.Width) |>",
    "  head(6L)  # Tab completes names; F1 opens function help"), collapse = "\n")
  state <- ns$.editor_state(text)
  if (!is.null(cursor)) {
    stopifnot("cursor must identify a grapheme boundary" = ns$.is_count(cursor) &&
                cursor <= length(state$chars))
    state$cursor <- cursor
  }
  size <- c(width = as.integer(width), height = as.integer(height))
  editor <- ns$.editor_display_frame(state, size, "r> ", "+  ", character(), config)
  frame <- ns$.screen_compose(config$screen, editor, config, width, height)
  cells <- lapply(frame$lines, ns$.screen_tokens)
  stopifnot("every composed row must fit in the terminal" =
              length(frame$lines) == height &&
              all(vapply(frame$lines, ns$.screen_width, numeric(1)) <= width - 1L),
            "explicit truecolor must survive disabled cli color detection" =
              any(grepl("38;2;", frame$lines, fixed = TRUE)))
  metadata <- list(generator = "prismrepl actual .screen_compose output",
    package_version = package_version, r_version = as.character(getRversion()),
    theme = config$theme$name, width = width, height = height,
    cursor_row = frame$cursor_row, cursor_column = frame$cursor_column,
    default_fg = config$theme$palette$foreground,
    default_bg = config$theme$palette$background,
    editor_source = text, editor_cursor = state$cursor,
    match_pairs = config$match_pairs, rainbow = config$rainbow,
    sidebar = frame$layout$sidebar, cells = cells)
  jsonlite::write_json(metadata, file.path(destination, paste0(name, ".json")),
                       auto_unbox = TRUE, pretty = FALSE)
  writeLines(frame$lines, file.path(destination, paste0(name, ".ansi")), useBytes = TRUE)
  plain <- gsub(ns$.screen_ansi_pattern(), "", frame$lines, perl = TRUE)
  writeLines(plain, file.path(destination, paste0(name, ".txt")), useBytes = TRUE)
  cat(name, ": ", width, " x ", height, "; sidebar ", frame$layout$sidebar,
      "; ", length(config$screen$entries), " actual transcript entries\n", sep = "")
  frame
}

dark <- demo_session("midnight")
invisible(export_frame(dark, 132L, 40L, "fullscreen-midnight-wide"))
invisible(export_frame(dark, 68L, 30L, "fullscreen-midnight-narrow"))
light <- demo_session("paper")
invisible(export_frame(light, 132L, 40L, "fullscreen-paper-wide"))
invisible(export_frame(demo_session("white"), 132L, 40L, "fullscreen-white-wide"))
invisible(export_frame(demo_session("black"), 132L, 40L, "fullscreen-black-wide"))

#' Export an Actual Help or Explorer Frame
#' @param lines Exact styled rows from a package compositor.
#' @param config Console configuration.
#' @param size Named width and height.
#' @param name Output basename.
#' @param cursor Optional zero-based row and column for an editable view.
#' @return NULL invisibly.
#' @noRd
export_panel <- function(lines, config, size, name, cursor = NULL) {
  stopifnot("panel must fit the requested terminal" = length(lines) == size[["height"]] &&
              all(vapply(lines, ns$.screen_width, numeric(1L)) <= size[["width"]] - 1L))
  metadata <- list(generator = "prismrepl actual viewer compositor output", theme = config$theme$name,
    package_version = package_version, r_version = as.character(getRversion()),
    width = size[["width"]], height = size[["height"]],
    cursor_row = if (is.null(cursor)) 0L else cursor$row,
    cursor_column = if (is.null(cursor)) 0L else cursor$column,
    show_cursor = !is.null(cursor),
    default_fg = config$theme$palette$foreground, default_bg = config$theme$palette$background,
    sidebar = FALSE, cells = lapply(lines, ns$.screen_tokens))
  jsonlite::write_json(metadata, file.path(destination, paste0(name, ".json")), auto_unbox = TRUE)
  writeLines(lines, file.path(destination, paste0(name, ".ansi")), useBytes = TRUE)
  writeLines(gsub(ns$.screen_ansi_pattern(), "", lines, perl = TRUE),
    file.path(destination, paste0(name, ".txt")), useBytes = TRUE)
  cat(name, ": ", size[["width"]], " x ", size[["height"]], "\n", sep = "")
  invisible(NULL)
}

panel_cfg <- ns$repl_config(theme = "white", color = TRUE, color_depth = 16777216L, settings_file = NULL)
panel_size <- c(width = 132L, height = 36L)
palette <- ns$.palette_state()
palette$cursor <- match("attach", ns$.palette_data(ns$.command_catalog())$id)
help_frame <- ns$.palette_frame(palette, panel_cfg, panel_size)
export_panel(help_frame, panel_cfg, panel_size, "palette-white")

doc_size <- c(width = 122L, height = 51L)
doc_lines <- ns$.help_rich_text("base::mean", NULL, doc_size[["width"]] - 3L, panel_cfg)
doc_hits <- ns$.panel_matches(doc_lines, "mean")
doc_frame <- ns$.panel_frame(doc_lines, "Help: base::mean", panel_cfg, doc_size,
  caption = ns$.footer_hints(c("Up/Down" = "line", "PageUp/PageDown" = "page", "/" = "search",
    "n/p" = "match", "F2" = "keys", q = "close"), panel_cfg, doc_size[["width"]] - 2L),
  footer = paste0("Search: mean  Match 1/", ns$.panel_match_count(doc_hits),
    "  All occurrences highlighted; n/p moves between them"),
  styled = TRUE, matches = doc_hits, current_match = 1L, active_rows = integer())
export_panel(doc_frame, panel_cfg, doc_size, "docs-white")

project <- list(data = datasets::iris,
  fit = stats::lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = datasets::iris),
  matrix = matrix(seq_len(24L), 6L, 4L), groups = split(datasets::iris, datasets::iris$Species),
  options = list(seed = 42L, confidence = .95, theme = "white"),
  labels = c("caf\u00e9", "\u03bc", "\u03c3"),
  created = as.Date("2026-11-23"), notes = "Click a child; Enter or Right opens it.")
attr(project, "source") <- "Evaluated R data for a reproducible explorer example"
panel_cfg$theme <- ns$repl_theme("midnight")
explorer <- ns$.explorer_state(project, label = "analysis", limit = 200L, sizes = TRUE)
export_panel(ns$.explorer_frame(explorer, panel_cfg, c(width = 132L, height = 25L)),
  panel_cfg, c(width = 132L, height = 25L), "explorer-midnight")
explorer$stack[[1L]]$cursor <- 2L
explorer <- ns$.explorer_apply(explorer,
  list(type = "key", key = "enter", modifiers = character()), panel_cfg)
export_panel(ns$.explorer_frame(explorer, panel_cfg, c(width = 132L, height = 28L)),
  panel_cfg, c(width = 132L, height = 28L), "explorer-drilldown-midnight")

column_preview <- ns$.explorer_state(datasets::iris, label = "flowers", limit = 200L)
column_preview$stack[[1L]]$cursor <- 4L
column_preview <- ns$.explorer_apply(column_preview,
  list(type = "key", key = "enter", modifiers = character()), panel_cfg)
stopifnot("data-frame column must open directly as a value preview" =
            identical(tail(column_preview$stack, 1L)[[1L]]$view, "preview"))
export_panel(ns$.explorer_frame(column_preview, panel_cfg, c(width = 132L, height = 25L)),
  panel_cfg, c(width = 132L, height = 25L), "explorer-column-midnight")

table_preview <- ns$.explorer_state(datasets::iris, label = "flowers", limit = 200L)
table_preview <- ns$.explorer_apply(table_preview,
  list(type = "key", key = "tab", modifiers = character()), panel_cfg)
table_preview <- ns$.explorer_apply(table_preview,
  list(type = "text", key = "", text = "/", modifiers = character()), panel_cfg)
table_preview <- ns$.explorer_apply(table_preview,
  list(type = "text", key = "", text = "VIRGINICA", modifiers = character()), panel_cfg)
stopifnot("Preview filter must retain original matching row positions" =
  identical(ns$.explorer_preview_load(table_preview$stack[[1L]])$indices, 101:150))
export_panel(ns$.explorer_frame(table_preview, panel_cfg, c(width = 132L, height = 25L)),
  panel_cfg, c(width = 132L, height = 25L), "explorer-filter-midnight")

original_function <- function(x, na.rm = TRUE) {
  stopifnot("x must be numeric" = is.numeric(x),
            "na.rm must be a logical scalar" = is.logical(na.rm) && length(na.rm) == 1L)
  values <- if (na.rm) x[!is.na(x)] else x
  out <- list(n = length(values), mean = mean(values), sd = stats::sd(values),
    quantiles = stats::quantile(values, probs = c(0.25, 0.5, 0.75)))
  stopifnot("summary must contain four components" = is.list(out) && length(out) == 4L)
  out
}
saved_source <- ns$.function_edit_source(original_function)
edited_source <- sub("0.25, 0.5, 0.75", "0.05, 0.5, 0.95", saved_source, fixed = TRUE)
stopifnot("demo editing must change the quantile probabilities" = !identical(saved_source, edited_source) &&
            grepl("0.95", edited_source, fixed = TRUE))
function_editor <- ns$.function_editor_state(edited_source)
function_editor$saved_source <- saved_source
function_editor$editor$cursor <- as.integer(regexpr("0.95", edited_source, fixed = TRUE)[[1L]] + 3L)
editor_size <- c(width = 122L, height = 24L)
editor_frame <- ns$.function_editor_frame(function_editor, "describe_numbers", panel_cfg, editor_size)
export_panel(editor_frame$lines, panel_cfg, editor_size, "editor-midnight",
  cursor = list(row = editor_frame$cursor_row, column = editor_frame$cursor_column))

#' Export Raw-String Pair Focus Independently of Rainbow Coloring
#' @param theme The white or black built-in theme.
#' @return A real 100-column, 24-row compositor frame.
#' @noRd
export_pair_focus <- function(theme) {
  stopifnot("theme must select a neutral light or dark palette" = theme %in% c("white", "black"))
  cfg <- ns$repl_config(theme = theme, color = TRUE, color_depth = 16777216L,
    unicode = TRUE, ui = "fullscreen", sidebar = FALSE, editor_height = 6L,
    settings_file = NULL, persist = FALSE, rainbow = FALSE, match_pairs = TRUE,
    status = c("process_memory", "objects", "memory"))
  cfg$screen <- ns$screen_state()
  cfg$screen$active <- TRUE
  cfg$highlight_cache <- new.env(parent = emptyenv())
  env <- new.env(parent = baseenv())
  env$flowers <- datasets::iris
  commands <- c("names(flowers)",
    'message("Cursor pair focus: on. Rainbow delimiter colors: off.")')
  invisible(lapply(commands, function(code) {
    ns$screen_append(cfg$screen, paste0(ns$style_text("r> ", "prompt", cfg$theme, TRUE),
      ns$highlight_r(code, cfg$theme, TRUE, rainbow = FALSE)), "input", ansi = TRUE)
    result <- ns$.with_screen_output(function(local) ns$evaluate_r(code, env, local),
      cfg, paint = FALSE)
    stopifnot("pair-focus transcript must evaluate successfully" = is.null(result$error))
    NULL
  }))
  inventory <- ns$workspace_objects(env, sizes = "memory" %in% cfg$status)
  process_memory <- ns$.process_memory()
  cfg$screen$sidebar <- ns$.session_sidebar(env, cfg, inventory, process_memory)
  cfg$screen$status <- ns$.session_status(env, cfg, inventory, process_memory)
  text <- paste(c('notes <- r"---{',
    '  Brackets stay literal: (x[1]) and {y}',
    '  Quotes stay literal: "text" and \'text\'',
    '}---"'), collapse = "\n")
  # Put the cursor on the first dash inside the compound opening delimiter.
  cursor <- as.integer(regexpr('r"---{', text, fixed = TRUE)[[1L]] + 1L)
  state <- ns$.editor_state(text)
  state$cursor <- cursor
  focus <- ns$.editor_pair_focus(state, text, cfg)
  stopifnot("raw delimiters must be recognized as complete endpoint spans" = nrow(focus) == 2L &&
              all(focus[, 2L] - focus[, 1L] + 1L == c(6L, 5L)),
            "rainbow must stay off while cursor pair focus is on" = !cfg$rainbow && cfg$match_pairs)
  out <- export_frame(cfg, 100L, 24L, paste0("pair-focus-", theme), text, cursor)
  stopifnot("the focused role must survive full-screen composition" = any(grepl(
    paste0("\033[", cfg$theme$colors[["pair_match"]], "m"), out$lines, fixed = TRUE)))
  out
}

invisible(lapply(c("white", "black"), export_pair_focus))
