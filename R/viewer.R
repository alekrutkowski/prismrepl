#' Explore an R Object Without Running Its Methods
#'
#' Opens a structural explorer with breadcrumbs, type and class metadata,
#' dimensions, approximate size, and bounded child pages. Enter or Right opens
#' a child; Left or Backspace returns to its parent. Tab switches children,
#' preview, and attributes. `/` filters child names; `[` and `]` change batches.
#' Arrow and Page keys move the selection. Mouse clicks and the wheel are
#' supported when enabled. F1 shows explorer shortcuts; `q` closes the viewer.
#' Press `s` to toggle cached child sizes. Sizes are initially shown when
#' `"memory"` is enabled in the status configuration; new measurements happen
#' for visible records after navigation or accepting a name filter.
#'
#' Environment promises and active bindings are displayed without evaluating
#' them and cannot be opened. Previews use ordinary underlying values and never
#' call custom print, format, subset, or str methods. Inspecting an explicitly
#' supplied argument still evaluates that argument normally. No objects are
#' modified. Plain consoles receive a bounded structural summary.
#'
#' @param x Object to inspect.
#' @param config Console configuration.
#' @param rows Maximum child records or preview rows per batch.
#' @param label Name shown at the root of the breadcrumb.
#' @param filter Initial literal child-name filter; an integer selects an index.
#' @return Initial prepared structural text invisibly.
#' @export
#' @examples
#' if (interactive()) view_object(iris, label = "iris")
view_object <- function(x, config = repl_config(), rows = 200L,
                        label = "object", filter = "") {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"),
            "rows must be a positive integer" = .is_count(rows, 1L),
            "label must be a text scalar" = .is_text(label),
            "filter must be one nonmissing string" = is.character(filter) &&
              length(filter) == 1L && !is.na(filter))
  out <- .object_explorer(x, config, min(1000L, rows), label, filter)
  stopifnot("explorer output must be text" = is.character(out) && !anyNA(out))
  invisible(out)
}

#' Browse R Documentation in the Terminal
#'
#' Documentation is rendered from installed Rd files with section headings,
#' emphasis, and R syntax colors. `/` searches literal text, highlights every
#' occurrence, and `n`/`p` moves between occurrences, including several on one
#' line. This avoids terminal overstrike sequences and does not open a browser
#' or use the network. Plain text remains available from [help_text()].
#'
#' @param topic A help topic, including a qualified topic such as `"stats::lm"`.
#'   Use `NULL` for a package index.
#' @param package Optional package name.
#' @param config Console configuration.
#' @return Prepared documentation text invisibly.
#' @export
#' @examples
#' if (interactive()) view_help("mean", package = "base")
view_help <- function(topic = NULL, package = NULL, config = repl_config()) {
  stopifnot("topic must be NULL or a text scalar" = is.null(topic) || .is_text(topic),
            "package must be NULL or a text scalar" = is.null(package) || .is_text(package),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  out <- help_text(topic, package, width = .viewer_width(config))
  .tui_panel(character(), paste("Help:", if (is.null(topic)) package else topic), config,
    formatter = function(width) list(lines = .help_rich_text(topic, package,
      max(20L, width), config), records = NULL), styled = TRUE)
  stopifnot("documentation output must be character" = is.character(out))
  invisible(out)
}

#' Format a Data Frame as Paired, Width-Aware Records
#' @param data A data frame with at least one column.
#' @param width Maximum physical row width.
#' @return A list of plain lines and their one-based record identifiers.
#' @noRd
.table_format <- function(data, width) {
  stopifnot("data must be a data frame with columns" = is.data.frame(data) && ncol(data) >= 1L,
            "width must be a positive integer" = .is_count(width, 1L))
  columns <- lapply(unclass(data), function(x) .safe_text(ifelse(is.na(x), "NA", as.character(x))))
  labels <- columns[[1L]]
  body <- if (length(columns) == 2L) columns[[2L]] else if (length(columns) == 1L)
    rep("", length(labels)) else vapply(seq_len(nrow(data)), function(i)
      paste(vapply(seq.int(2L, length(columns)), function(j)
        paste0(names(data)[[j]], ": ", columns[[j]][[i]]), character(1L)), collapse = "  |  "), character(1L))
  label_width <- if (length(labels)) min(30L, max(12L, max(.panel_cells(labels)))) else 12L
  paired <- width >= label_width + 26L
  records <- lapply(seq_along(labels), function(i) {
    if (paired) {
      left <- .condition_wrap(labels[[i]], label_width, continuation = "")
      right <- .condition_wrap(body[[i]], max(4L, width - label_width - 2L), continuation = "")
      n <- max(length(left), length(right))
      left <- c(left, rep("", n - length(left)))
      right <- c(right, rep("", n - length(right)))
      paste0(.panel_fit(left, label_width), "  ", right)
    } else {
      c(.condition_wrap(labels[[i]], max(4L, width), continuation = ""),
        if (nzchar(body[[i]])) paste0(if (width >= 6L) "  " else "",
          .condition_wrap(body[[i]], max(4L, width - if (width >= 6L) 2L else 0L), continuation = "")))
    }
  })
  # Separate stacked records, keeping command and description together.
  if (!paired && length(records) > 1L) records[-length(records)] <-
    lapply(records[-length(records)], c, "")
  out <- list(lines = unlist(records, use.names = FALSE),
              records = rep(seq_along(records), lengths(records)), count = nrow(data))
  if (is.null(out$lines)) out$lines <- character()
  if (width < 4L && length(out$lines)) out$lines <- .panel_fit(out$lines, width)
  stopifnot("table rows must retain their record identifiers" =
              is.character(out$lines) && !anyNA(out$lines) && length(out$lines) == length(out$records),
            "formatted records must fit the requested width" = all(.panel_cells(out$lines) <= width))
  out
}

#' Open a Structured Table Viewer
#' @param data A data frame.
#' @param title Panel title.
#' @param config Configuration.
#' @return NULL invisibly.
#' @noRd
.tui_table <- function(data, title, config) {
  stopifnot("data must be a data frame" = is.data.frame(data),
            "title must be scalar text" = .is_text(title),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  .tui_panel(character(), title, config,
             formatter = function(width) .table_format(data, width))
  invisible(NULL)
}

#' Map Common Viewer Keys Through the Session Key Bindings
#' @param event A native terminal event.
#' @param config Configuration.
#' @return A canonical viewer action or an empty string.
#' @noRd
.viewer_action <- function(event, config) {
  stopifnot("event must be a list" = is.list(event), "config must be a list" = is.list(config))
  if (identical(event$type, "mouse")) return("")
  action <- if (exists(".key_action", mode = "function")) .key_action(event, config) else NULL
  mapped <- c(up = "up", down = "down", left = "left", right = "right",
    scroll_up = "pageup", scroll_down = "pagedown", line_start = "home",
    line_end = "end", complete = "tab", complete_previous = "backtab",
    buffer_start = "home", buffer_end = "end", backspace = "backspace",
    submit = "enter", cancel = "escape", escape = "escape", help = "help")
  if (is.character(action) && length(action) == 1L && action %in% names(mapped))
    return(unname(mapped[[action]]))
  out <- if (!is.null(action)) "" else if (is.character(event$key) && length(event$key)) event$key else ""
  if (identical(out, "tab") && "shift" %in% event$modifiers) out <- "backtab"
  if (identical(out, "f1")) out <- "help"
  stopifnot("viewer action must be scalar text" = is.character(out) && length(out) == 1L)
  out
}

#' Label Effective Common Viewer Shortcuts
#' @param config Configuration.
#' @param actions Action identifiers.
#' @return A slash-separated label, or unbound.
#' @noRd
.viewer_keys <- function(config, actions) {
  stopifnot("config must be a list" = is.list(config),
            "actions must be nonmissing text" = is.character(actions) && !anyNA(actions))
  bindings <- if (is.null(config$keybindings)) .default_keybindings() else config$keybindings
  keys <- unique(unlist(lapply(actions, function(action) utils::head(bindings[[action]], 1L)), use.names = FALSE))
  out <- if (length(keys)) paste(keys, collapse = "/") else "unbound"
  stopifnot("shortcut label must be scalar text" = .is_text(out))
  out
}

#' Keep Essential Viewer Hints Visible in Narrow Windows
#' @param parts Ordered hint strings; closing help belongs last.
#' @param width Available cells.
#' @return One compact footer string.
#' @noRd
.viewer_caption <- function(parts, width) {
  stopifnot("parts must be nonmissing text" = is.character(parts) && length(parts) >= 1L && !anyNA(parts),
            "width must be positive" = .is_count(width, 1L))
  while (length(parts) > 1L && .panel_cells(paste(parts, collapse = "  ")) > width)
    parts <- parts[-max(1L, length(parts) - 2L)]
  out <- paste(parts, collapse = "  ")
  stopifnot("caption must be scalar text" = .is_text(out))
  out
}

#' Prepare the Pager Shortcut Page Using Effective Bindings
#' @param config Configuration.
#' @param width Content width.
#' @param selectable Whether the panel accepts a batch selection.
#' @param records Whether the panel moves between structured records.
#' @param choose Whether Enter accepts the focused record.
#' @return Plain shortcut rows.
#' @noRd
.panel_help <- function(config, width, selectable, records, choose = FALSE) {
  stopifnot("width must be positive" = .is_count(width, 1L),
            "selection and record flags must be logical" = .is_flag(selectable) && .is_flag(records) && .is_flag(choose))
  keys <- c(.viewer_keys(config, c("up", "down")),
    .viewer_keys(config, c("scroll_up", "scroll_down")),
    .viewer_keys(config, c("line_start", "line_end")),
    .viewer_keys(config, c("left", "right")), "/", "n / p",
    if (selectable) c("Space / a", .viewer_keys(config, "submit")),
    if (choose) .viewer_keys(config, "submit"),
    .viewer_keys(config, "help"), paste0("q / ", .viewer_keys(config, "escape")))
  descriptions <- c(if (records) "Move to the previous or next record." else "Move one physical line.",
    "Move one visible page.", "Jump to the first or last line.", "Pan horizontally through long lines.",
    "Search literal text and highlight every occurrence; Enter accepts and Escape cancels.",
    "Move to the next or previous occurrence, including repeated matches on one line.",
    if (selectable) c("Toggle one selection, or all selections.", "Accept selected records."),
    if (choose) "Apply the focused record. Navigation and mouse clicks only move focus.",
    "Show or hide this shortcut page.", "Close the viewer or cancel the selection.")
  descriptions[[5L]] <- paste0("Search literal text and highlight every occurrence; ", .viewer_keys(config, "submit"),
    " accepts and ", .viewer_keys(config, "escape"), " cancels.")
  keys <- c(keys, "Drag / Ctrl+drag", "Right-click")
  descriptions <- c(descriptions, "Select and copy visible text; Ctrl selects a rectangle.",
    "Paste clipboard text into the search field.")
  out <- .table_format(data.frame(key = keys, description = descriptions, stringsAsFactors = FALSE), width)$lines
  stopifnot("pager help must be nonmissing text" = is.character(out) && !anyNA(out))
  out
}

#' Acquire a Viewer Terminal Without Reentering an Existing Alternate Screen
#' @param config Configuration.
#' @return A handle, output guard, and screen ownership flag.
#' @noRd
.viewer_start <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  shared <- is.environment(config$screen) && isTRUE(config$screen$active)
  guard <- if (interactive() && !isTRUE(term_output_state()$active)) term_output_open() else NULL
  complete <- FALSE
  handle <- NULL
  on.exit({
    if (!complete) {
      if (!is.null(handle)) term_close(handle)
      if (!is.null(guard)) term_output_close(guard)
    }
  }, add = TRUE)
  handle <- if (interactive()) term_open(mouse = isTRUE(config$mouse)) else NULL
  if (is.null(handle)) {
    if (!is.null(guard)) term_output_close(guard)
    guard <- NULL
  } else {
    if (!shared) { term_write("\033[?1049h"); term_output_screen(TRUE) }
    term_write("\033[?25l")
  }
  out <- list(handle = handle, guard = guard, shared = shared)
  stopifnot("viewer session must contain ownership information" =
              is.list(out) && .is_flag(out$shared))
  complete <- TRUE
  out
}

#' Release a Viewer and Repaint Its Parent Screen
#' @param session Result of .viewer_start.
#' @param config Configuration.
#' @return NULL invisibly.
#' @noRd
.viewer_finish <- function(session, config) {
  stopifnot("session and config must be lists" = is.list(session) && is.list(config))
  on.exit({ if (!is.null(session$guard)) term_output_close(session$guard) }, add = TRUE)
  if (!is.null(session$handle)) {
    term_close(session$handle)
    if (session$shared) {
      if (exists("screen_invalidate", mode = "function")) screen_invalidate(config$screen)
      else config$screen$last_frame <- NULL
      screen_render(config$screen, config = config)
    } else { term_write("\033[0m\033[?25h\033[?1049l"); term_output_screen(FALSE) }
  }
  invisible(NULL)
}

#' Emit Only Changed Viewer Rows in One Native Write
#' @param lines Exact-height styled rows.
#' @param previous The preceding frame or NULL.
#' @param size Actual terminal dimensions.
#' @param config Configuration.
#' @return The current frame invisibly.
#' @noRd
.panel_draw <- function(lines, previous, size, config) {
  stopifnot("lines must be nonmissing text" = is.character(lines) && !anyNA(lines),
            "size must contain terminal dimensions" = all(c("width", "height") %in% names(size)))
  frame <- list(lines = lines, cursor_row = 0L, cursor_column = 0L,
                layout = list(width = size[["width"]], height = size[["height"]]))
  frame <- .cursor_frame(frame, config, visible = FALSE)
  update <- .screen_build_update(frame, previous, config, show_cursor = FALSE)
  if (nzchar(update)) term_write(update, synchronize = isTRUE(config$synchronize))
  stopifnot("drawn viewer frame must preserve row count" = length(frame$lines) == size[["height"]])
  invisible(frame)
}

#' Show a Cached, Locally Scrollable Terminal Panel
#' @param lines Character lines.
#' @param title Heading.
#' @param config Configuration.
#' @param selected NULL for a viewer or one logical value per selectable line.
#' @param formatter Optional width-to-records formatting function.
#' @param styled Whether content contains safe semantic ANSI styling.
#' @param choose Whether Enter accepts one focused record, independently of
#'   batch selection. Mouse clicks only move focus in this mode.
#' @param initial_record One-based record identifier to focus initially.
#' @return Selected row numbers, one chosen record, or NULL when cancelled or read-only.
#' @noRd
.tui_panel <- function(lines, title, config, selected = NULL, formatter = NULL, styled = FALSE,
                       choose = FALSE, initial_record = 1L) {
  stopifnot("lines must be nonmissing character text" = is.character(lines) && !anyNA(lines),
            "title must be a text scalar" = .is_text(title),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "selected must be NULL or match the lines" = is.null(selected) ||
              (is.logical(selected) && !anyNA(selected) && length(selected) == length(lines)),
            "formatter must be NULL or a function" = is.null(formatter) || is.function(formatter),
            "styled and choose must be logical" = .is_flag(styled) && .is_flag(choose),
            "single choice cannot be combined with batch selection" = !choose || is.null(selected),
            "initial_record must be a positive integer" = .is_count(initial_record, 1L))
  selectable <- !is.null(selected)
  if (selectable && !length(lines)) return(invisible(integer()))
  lines <- stringi::stri_replace_all_fixed(if (styled)
    vapply(lines, .screen_safe_ansi, character(1L), USE.NAMES = FALSE) else .safe_text(lines), "\n", "\\n")
  title <- stringi::stri_replace_all_fixed(.safe_text(title), "\n", "\\n")
  session <- .viewer_start(config)
  on.exit(.viewer_finish(session, config), add = TRUE)
  size <- term_size()
  records <- NULL
  if (!is.null(formatter)) {
    prepared <- formatter(max(1L, .panel_layout(size)$width - 2L))
    lines <- prepared$lines
    records <- prepared$records
  }
  if (choose && !is.null(records)) stopifnot("choice records must match the formatted lines" =
    is.numeric(records) && length(records) == length(lines) && !anyNA(records) &&
      all(records >= 1L & records == trunc(records)))
  if (is.null(session$handle)) {
    if (choose && interactive() && length(lines))
      return(invisible(.panel_plain_choice(lines, title, records)))
    if (selectable && interactive())
      return(invisible(.panel_plain_selection(lines, title, selected)))
    .emit(title, "heading", config)
    .emit(gsub(.screen_ansi_pattern(), "", lines, perl = TRUE), "output", config)
    if (selectable) .emit("Use explicit command arguments in a plain console.", "muted", config)
    return(invisible(NULL))
  }
  if (choose && !length(lines)) return(invisible(NULL))
  if (!length(lines)) { lines <- "(empty)"; records <- NULL }
  cursor <- if (length(records) && initial_record %in% records) which(records == initial_record)[[1L]]
    else if (is.null(records)) min(as.integer(initial_record), length(lines)) else 1L
  top <- if (choose) max(1L, cursor - .panel_layout(size)$page %/% 3L) else 1L
  query <- ""
  hits <- .panel_matches(lines, query)
  current_match <- 0L
  search_before <- NULL
  searching <- FALSE
  redraw <- TRUE
  horizontal <- 0L
  previous <- NULL
  cache <- new.env(parent = emptyenv())
  help_cache <- new.env(parent = emptyenv())
  helping <- FALSE
  help_top <- 1L
  help_width <- 0L
  help_lines <- character()
  mouse <- .mouse_state()
  mouse_status <- ""
  repeat {
    layout <- .panel_layout(size)
    page <- layout$page
    cursor <- max(1L, min(length(lines), cursor))
    if (cursor < top) top <- cursor
    if (cursor >= top + page) top <- cursor - page + 1L
    if (redraw) {
      hint_key <- list(layout$width, config$keybindings, config$theme$colors, config$color)
      if (!identical(cache$hint_key, hint_key)) {
        hints <- if (choose) stats::setNames(c("apply", "move", "search", "match"),
          c(.viewer_keys(config, "submit"), .viewer_keys(config, c("up", "down")), "/", "n/p"))
          else if (selectable) stats::setNames(c("select", "all/none", "accept", "search"),
          c("Space", "a", .viewer_keys(config, "submit"), "/")) else stats::setNames(c(
            if (is.null(records)) "line" else "record", "page", "search", "match"),
          c(.viewer_keys(config, c("up", "down")), .viewer_keys(config, c("scroll_up", "scroll_down")), "/", "n/p"))
        closing <- if (choose) stats::setNames("cancel", paste0(.viewer_keys(config, "escape"), "/q"))
          else c(q = if (selectable) "cancel" else "close")
        cache$caption <- .footer_hints(c(hints, stats::setNames("keys", .viewer_keys(config, "help")),
          closing), config, max(1L, layout$width - 1L))
        cache$hint_key <- hint_key
      }
      caption <- if (searching) paste0(style_text("Search: ", "accent", config$theme, config$color), query,
        "  ", .footer_hints(stats::setNames(c("apply", "cancel"),
          c(.viewer_keys(config, "submit"), .viewer_keys(config, "escape"))), config,
          max(1L, layout$width - .panel_cells(query) - 10L))) else cache$caption
      footer <- if (!is.null(records)) paste0("Record ", records[[cursor]], "/", max(records),
        "  line ", cursor, "/", length(lines)) else paste0("Line ", cursor, "/", length(lines),
        "  column ", horizontal + 1L)
      if (nzchar(query)) footer <- paste0(footer, "  Match ", current_match, "/", .panel_match_count(hits))
      if (nzchar(mouse_status)) footer <- mouse_status
      active <- if (is.null(records)) cursor else which(records == records[[cursor]])
      if (helping) {
        if (help_width != layout$width) {
          help_lines <- .panel_help(config, max(1L, layout$width - 2L), selectable, !is.null(records), choose)
          help_width <- layout$width
        }
        help_top <- max(1L, min(help_top, max(1L, length(help_lines) - page + 1L)))
        frame <- .panel_frame(help_lines, "Viewer shortcuts", config, size, cursor = help_top,
          top = help_top, caption = .footer_hints(stats::setNames("return", paste0(
            .viewer_keys(config, c("help", "escape")), "/q")), config, max(1L, layout$width - 1L)),
          footer = "Common shortcuts follow your session bindings.", cache = help_cache, active_rows = integer())
      } else frame <- .panel_frame(lines, title, config, size, cursor, top, horizontal,
          selected, caption, footer, cache = cache, active_rows = active,
          styled = styled, matches = hits, current_match = current_match)
      previous <- .panel_draw(.mouse_paint(frame, mouse, config), previous, size, config)
      redraw <- FALSE
    }
    event <- term_read(session$handle, 50L)
    if (identical(event$type, "timeout")) next
    dispatched <- .mouse_dispatch(event, frame, mouse, config)
    mouse <- dispatched$selection
    if (nzchar(dispatched$status)) mouse_status <- dispatched$status
    else if (!identical(event$type, "mouse")) mouse_status <- ""
    redraw <- redraw || dispatched$redraw
    event <- dispatched$event
    if (is.null(event)) next
    if (identical(event$type, "resize")) {
      size <- term_size()
      if (!is.null(formatter)) {
        record <- if (length(records)) records[[cursor]] else 1L
        prepared <- formatter(max(1L, .panel_layout(size)$width - 2L))
        lines <- prepared$lines
        records <- prepared$records
        cursor <- if (length(records)) {
          if (record %in% records) which(records == record)[[1L]] else 1L
        } else min(cursor, max(1L, length(lines)))
        top <- min(top, cursor)
      }
      hits <- .panel_matches(lines, query)
      current_match <- 0L
      previous <- NULL
      redraw <- TRUE
      next
    }
    if (identical(event$type, "eof")) return(invisible(NULL))
    key <- .viewer_action(event, config)
    text <- event$text
    ctrl <- "ctrl" %in% event$modifiers
    redraw <- TRUE
    if (helping && identical(event$type, "paste")) helping <- FALSE
    if (helping) {
      if (key %in% c("help", "escape") || identical(text, "q")) helping <- FALSE
      if (key == "down" || identical(event$key, "wheeldown")) help_top <- help_top + 1L
      if (key == "up" || identical(event$key, "wheelup")) help_top <- max(1L, help_top - 1L)
      if (key == "pagedown") help_top <- help_top + page
      if (key == "pageup") help_top <- max(1L, help_top - page)
      if (key == "home") help_top <- 1L
      if (key == "end") help_top <- max(1L, length(help_lines) - page + 1L)
      next
    }
    if (identical(event$type, "mouse")) {
      if (event$key %in% c("wheelup", "wheeldown")) cursor <- max(1L, min(length(lines),
        cursor + if (event$key == "wheelup") -3L else 3L))
      else if (identical(event$action, "press") && identical(event$button, "left") &&
               event$y > layout$header && event$y <= layout$header + page) {
        hit <- top + event$y - layout$header - 1L
        if (hit > length(lines)) next
        cursor <- hit
        if (selectable && event$x <= 6L) selected[[cursor]] <- !selected[[cursor]]
      }
      current_match <- 0L
      next
    }
    if (identical(event$type, "paste") && !searching) {
      search_before <- list(query = query, hits = hits, current_match = current_match,
        cursor = cursor, horizontal = horizontal, top = top)
      searching <- TRUE
      query <- ""
    }
    if (searching) {
      if (key == "escape" || (ctrl && key == "c")) {
        searching <- FALSE
        if (!is.null(search_before)) {
          query <- search_before$query; hits <- search_before$hits
          current_match <- search_before$current_match; cursor <- search_before$cursor
          horizontal <- search_before$horizontal; top <- search_before$top
        }
        next
      }
      if (key == "backspace") {
        parts <- stringi::stri_split_boundaries(query, type = "character")[[1L]]
        query <- paste(utils::head(parts, -1L), collapse = "")
      } else if (key == "enter") {
        searching <- FALSE
        next
      } else if (ctrl && identical(event$key, "u")) query <- ""
      else if (event$type %in% c("text", "paste"))
        query <- substr(paste0(query, gsub("[\r\n\t]", " ", .safe_text(text))), 1L, 200L)
      hits <- .panel_matches(lines, query)
      current_match <- if (nrow(hits)) 1L else 0L
      if (current_match) {
        cursor <- hits$row[[1L]]
        horizontal <- .panel_reveal_match(hits, current_match, horizontal, max(1L, layout$width - 2L))
        top <- max(1L, cursor - page %/% 3L)
      }
      next
    }
    if (key == "escape" || identical(text, "q") || (ctrl && key == "c")) return(invisible(NULL))
    if (key == "help") { helping <- TRUE; help_top <- 1L; next }
    if (identical(text, "/")) {
      search_before <- list(query = query, hits = hits, current_match = current_match,
        cursor = cursor, horizontal = horizontal, top = top)
      query <- ""; hits <- .panel_matches(lines, query); current_match <- 0L
      searching <- TRUE
      next
    }
    if (selectable && key == "enter") {
      out <- which(selected)
      stopifnot("selection output must contain valid row numbers" = all(out >= 1L & out <= length(lines)))
      return(invisible(out))
    }
    if (choose && key == "enter") {
      out <- as.integer(if (is.null(records)) cursor else records[[cursor]])
      stopifnot("chosen record must be a positive integer" = .is_count(out, 1L))
      return(invisible(out))
    }
    if (selectable && identical(text, " ")) { selected[[cursor]] <- !selected[[cursor]]; next }
    if (selectable && identical(text, "a")) { selected[] <- !all(selected); next }
    if (nrow(hits) && text %in% c("n", "p", "N")) {
      chosen <- .panel_match_step(hits, current_match, if (text == "n") 1L else -1L, cursor)
      current_match <- chosen$index
      cursor <- chosen$row
      horizontal <- .panel_reveal_match(hits, current_match, horizontal, max(1L, layout$width - 2L))
      top <- max(1L, cursor - page %/% 3L)
      next
    }
    if (key %in% c("up", "down", "left", "right", "pageup", "pagedown", "home", "end", "enter") ||
        text %in% c("j", "k", "g", "G", " ")) current_match <- 0L
    if (key %in% c("up", "down") || text %in% c("j", "k")) {
      direction <- if (key == "up" || identical(text, "k")) -1L else 1L
      if (is.null(records)) cursor <- cursor + direction else {
        starts <- which(!duplicated(records))
        index <- match(records[[cursor]], records[starts])
        cursor <- starts[[max(1L, min(length(starts), index + direction))]]
      }
    }
    if (key == "pagedown" || (!selectable && identical(text, " ")) ||
        key == "enter" || (ctrl && key == "f")) cursor <- cursor + page
    if (key == "pageup" || (ctrl && key == "b")) cursor <- cursor - page
    if (key == "home" || identical(text, "g")) cursor <- 1L
    if (key == "end" || identical(text, "G")) cursor <- length(lines)
    if (key == "left") horizontal <- max(0L, horizontal - 8L)
    if (key == "right") horizontal <- min(max(vapply(lines, .screen_width, numeric(1L))), horizontal + 8L)
  }
}

#' Choose One Record with the Host Console's Numeric Menu
#' @param lines Formatted text lines.
#' @param title Menu title.
#' @param records Optional physical-line-to-record mapping.
#' @param ask Numeric menu function, injectable for tests.
#' @return One chosen record identifier, or NULL after cancellation.
#' @noRd
.panel_plain_choice <- function(lines, title, records = NULL, ask = utils::menu) {
  stopifnot("lines must be nonmissing text" = is.character(lines) && !anyNA(lines),
            "title must be nonempty text" = .is_text(title),
            "records must be NULL or match the lines" = is.null(records) ||
              (is.numeric(records) && length(records) == length(lines) && !anyNA(records) && all(records >= 1L)),
            "ask must be a function" = is.function(ask))
  if (!length(lines)) return(NULL)
  if (is.null(records)) records <- seq_along(lines)
  ids <- unique(records)
  choices <- vapply(ids, function(id) paste(trimws(gsub(.screen_ansi_pattern(), "",
    lines[records == id], perl = TRUE)), collapse = " "), character(1L))
  selected <- ask(choices, graphics = FALSE, title = paste0(title, "; 0 cancels"))
  stopifnot("menu choice must be zero or a valid item" = .is_count(selected) && selected <= length(ids))
  out <- if (selected == 0L) NULL else as.integer(ids[[selected]])
  stopifnot("plain choice must be NULL or a record identifier" = is.null(out) || .is_count(out, 1L))
  out
}

#' Select a Batch with the Host Console's Numeric Menu
#'
#' Choosing a row toggles its mark and returns to the menu. Only the separate
#' Apply selection entry accepts the marked rows. Zero cancels every staged
#' change. The caller retains responsibility for any subsequent confirmation.
#'
#' @param lines One nonmissing character label per selectable row.
#' @param title Nonempty menu title.
#' @param selected Current logical selection, one value per label.
#' @param ask Numeric menu function, injectable for tests.
#' @return Selected row indices after applying, or NULL after cancellation.
#' @noRd
.panel_plain_selection <- function(lines, title, selected, ask = utils::menu) {
  stopifnot("lines must be nonmissing text" = is.character(lines) && !anyNA(lines),
            "title must be nonempty text" = .is_text(title),
            "selected must contain one nonmissing logical per line" =
              is.logical(selected) && !anyNA(selected) && length(selected) == length(lines),
            "ask must be a function" = is.function(ask))
  labels <- trimws(gsub(.screen_ansi_pattern(), "", lines, perl = TRUE))
  labels <- gsub("[\r\n]+", " ", .safe_text(labels))
  repeat {
    choices <- c("Apply selection", paste0(ifelse(selected, "[x] ", "[ ] "), labels))
    chosen <- ask(choices, graphics = FALSE, title = paste0(title, "; 0 cancels"))
    stopifnot("menu choice must be zero or a valid item" =
                .is_count(chosen) && chosen <= length(choices))
    if (chosen <= 1L) {
      out <- if (chosen == 0L) NULL else which(selected)
      stopifnot("plain selection must be NULL or valid row indices" = is.null(out) ||
                  (is.integer(out) && all(out >= 1L & out <= length(lines))))
      return(out)
    }
    row <- as.integer(chosen) - 1L
    selected[[row]] <- !selected[[row]]
  }
}

#' Resolve the Width of a Full-Window Viewer
#' @param config Console configuration.
#' @return A positive integer width.
#' @noRd
.viewer_width <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  out <- if (is.null(config$width)) max(20L, term_size()[["width"]] - 2L) else config$width
  out <- min(1000L, as.integer(out))
  stopifnot("viewer width must be at least twenty" = .is_count(out, 20L))
  out
}

#' Calculate Pager Geometry from the Actual Window
#' @param size Named width and height.
#' @param headers Requested header rows.
#' @return Positive dimensions and row allocations.
#' @noRd
.panel_layout <- function(size, headers = 1L) {
  stopifnot("size must contain positive terminal dimensions" = is.numeric(size) &&
              all(c("width", "height") %in% names(size)) && all(size > 0L),
            "headers must be a nonnegative integer" = .is_count(headers))
  height <- as.integer(size[["height"]])
  header <- if (height >= 3L) min(headers, max(1L, height - 3L)) else 0L
  footer <- min(2L, max(0L, height - header - 1L))
  out <- list(width = max(1L, as.integer(size[["width"]]) - 1L), height = height,
              header = header, footer = footer, page = height - header - footer)
  stopifnot("pager must reserve a content row" = out$page >= 1L)
  out
}

#' Measure Plain Text Quickly While Respecting Unicode Cells
#' @param text Character strings.
#' @return Nonnegative cell widths.
#' @noRd
.panel_cells <- function(text) {
  stopifnot("text must be nonmissing character" = is.character(text) && !anyNA(text))
  out <- nchar(text, type = "bytes")
  nonascii <- grepl("[^ -~]", text, perl = TRUE)
  if (any(nonascii)) out[nonascii] <- .condition_cells(text[nonascii])
  stopifnot("cell widths must be nonnegative" = all(out >= 0L))
  out
}

#' Fit Plain Text to a Fixed Number of Cells
#' @param text Plain character strings.
#' @param width Nonnegative width.
#' @return Padded, clipped text.
#' @noRd
.panel_fit <- function(text, width) {
  stopifnot("text must be nonmissing character" = is.character(text) && !anyNA(text),
            "width must be a nonnegative integer" = .is_count(width))
  if (!width) return(rep("", length(text)))
  cells <- .panel_cells(text)
  long <- which(cells > width)
  if (length(long)) {
    text[long] <- vapply(text[long], .panel_slice, character(1L), offset = 0L, width = width)
    cells[long] <- .panel_cells(text[long])
  }
  out <- paste0(text, strrep(" ", width - cells))
  stopifnot("fitted plain text must retain its length" = length(out) == length(text))
  out
}

#' Compose a Cached Exact-Size Viewer Frame
#' @param lines Plain content lines.
#' @param title Viewer heading.
#' @param config Configuration.
#' @param size Named actual terminal dimensions.
#' @param cursor,top Selected and first visible content row.
#' @param horizontal Horizontal offset in display cells.
#' @param selected Optional selection flags.
#' @param caption,footer Footer text.
#' @param cache Optional session-local cache environment.
#' @param header Additional header rows.
#' @param active_rows Physical rows highlighted as the selected record.
#' @param styled Whether content contains trusted ANSI styles.
#' @param matches Optional literal occurrence locations from .panel_matches.
#' @param current_match Index of the occurrence to emphasize more strongly.
#' @return Exact-height styled rows.
#' @noRd
.panel_frame <- function(lines, title, config, size, cursor = 1L, top = 1L,
                          horizontal = 0L, selected = NULL, caption = "q close", footer = "",
                          cache = NULL, header = character(), active_rows = cursor,
                          styled = FALSE, matches = NULL, current_match = 0L) {
  stopifnot("content must be nonmissing text" = is.character(lines) && !anyNA(lines),
            "title must be text" = .is_text(title),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "cursor and top must be positive integers" = .is_count(cursor, 1L) && .is_count(top, 1L),
            "horizontal offset must be nonnegative" = .is_count(horizontal),
            "selected must match the content" = is.null(selected) ||
              (is.logical(selected) && length(selected) == length(lines) && !anyNA(selected)),
            "cache must be NULL or an environment" = is.null(cache) || is.environment(cache),
            "header must be nonmissing text" = is.character(header) && !anyNA(header),
            "styled must be logical" = .is_flag(styled),
            "matches must be NULL or a data frame" = is.null(matches) || is.data.frame(matches),
            "current match must be a nonnegative integer" = .is_count(current_match))
  if (is.null(cache)) cache <- new.env(parent = emptyenv())
  layout <- .panel_layout(size, 1L + length(header))
  key <- list(lines, layout$width, horizontal, !is.null(selected), config$theme$colors,
              config$color, styled, matches, current_match)
  if (!identical(cache$key, key)) {
    cache$key <- key
    cache$normal <- cache$active <- rep(NA_character_, length(lines))
    cache$checks <- rep(NA, length(lines))
    cache$blank <- style_text(strrep(" ", layout$width), "background", config$theme, config$color)
  }
  rows <- rep(cache$blank, layout$height)
  headings <- utils::head(c(title, header), layout$header)
  if (length(headings)) rows[seq_along(headings)] <-
    style_text(.panel_fit(paste0(" ", .safe_text(headings)), layout$width),
                 "panel_header", config$theme, config$color)
  indices <- if (top <= length(lines)) seq.int(top, min(length(lines), top + layout$page - 1L)) else integer()
  if (length(indices)) {
    checks <- if (is.null(selected)) rep(FALSE, length(indices)) else selected[indices]
    missing <- indices[is.na(cache$normal[indices]) | is.na(cache$checks[indices]) |
                         cache$checks[indices] != checks]
    if (length(missing)) {
      marks <- if (is.null(selected)) rep("", length(missing)) else ifelse(selected[missing], "[x] ", "[ ] ")
      body <- vapply(missing, function(i) {
        text <- if (styled) .screen_safe_ansi(lines[[i]]) else .safe_text(lines[[i]])
        if (!is.null(matches) && nrow(matches)) text <- .panel_highlight(text,
          matches[matches$row == i, , drop = FALSE], current_match, config)
        .panel_slice_ansi(text, offset = horizontal,
          width = max(1L, layout$width - 2L - if (is.null(selected)) 0L else 4L))
      }, character(1L))
      fit <- function(text) vapply(text, .screen_fit, character(1L), width = layout$width)
      cache$normal[missing] <- style_text(fit(paste0("  ", marks, body)),
                                         "background", config$theme, config$color)
      cache$active[missing] <- style_text(fit(paste0("> ", marks, body)),
                                         "selection", config$theme, config$color)
      cache$checks[missing] <- if (is.null(selected)) FALSE else selected[missing]
    }
    rows[layout$header + seq_along(indices)] <- ifelse(indices %in% active_rows,
      cache$active[indices], cache$normal[indices])
  }
  if (layout$footer >= 2L) rows[[layout$height - 1L]] <- style_text(
    .screen_fit(paste0(" ", .screen_safe_ansi(footer)), layout$width), "status", config$theme, config$color)
  if (layout$footer >= 1L) rows[[layout$height]] <- style_text(
    .screen_fit(paste0(" ", .screen_safe_ansi(caption)), layout$width), "status", config$theme, config$color)
  stopifnot("viewer frame must preserve the terminal height" = length(rows) == layout$height && !anyNA(rows))
  rows
}

#' Slice Plain Text by Display Columns at Grapheme Boundaries
#' @param text One string.
#' @param offset Columns to skip.
#' @param width Available columns.
#' @return One string containing complete grapheme clusters.
#' @noRd
.panel_slice <- function(text, offset, width) {
  stopifnot("text must be one nonmissing string" = is.character(text) && length(text) == 1L && !is.na(text),
            "offset must be a nonnegative integer" = .is_count(offset),
            "width must be a positive integer" = .is_count(width, 1L))
  if (!grepl("[^ -~]", text, perl = TRUE)) return(substr(text, offset + 1L, offset + width))
  chars <- stringi::stri_split_boundaries(text, type = "character")[[1L]]
  widths <- utf8::utf8_width(chars, encode = FALSE, utf8 = TRUE)
  ends <- cumsum(widths)
  starts <- ends - widths
  eligible <- which(starts >= offset)
  out <- if (!length(eligible)) "" else {
    origin <- starts[eligible[[1L]]]
    paste(chars[eligible[ends[eligible] - origin <= width]], collapse = "")
  }
  stopifnot("panel slice must return one string" = is.character(out) && length(out) == 1L)
  out
}

#' Slice Styled Text by Cells Without Splitting a Unicode Grapheme
#' @param text A safe styled line.
#' @param offset Cells to skip.
#' @param width Available cells.
#' @return One styled substring with its original leading style restored.
#' @noRd
.panel_slice_ansi <- function(text, offset, width) {
  stopifnot("text must be one string" = is.character(text) && length(text) == 1L && !is.na(text),
            "offset must be nonnegative" = .is_count(offset),
            "width must be positive" = .is_count(width, 1L))
  if (!grepl("\033", text, fixed = TRUE)) return(.panel_slice(text, offset, width))
  parts <- .screen_tokens(text)
  cells <- parts$widths[parts$printable]
  ends <- cumsum(cells)
  starts <- ends - cells
  keep <- which(starts >= offset & ends <= offset + width)
  out <- if (!length(keep)) "" else paste0(strrep(" ", max(0L, starts[[keep[[1L]]]] - offset)),
    .screen_token_slice(parts, keep[[1L]], utils::tail(keep, 1L)))
  stopifnot("styled slice must fit its cells" = .screen_width(out) <= width)
  out
}

#' Locate Every Literal Search Occurrence in Visible Text
#' @param lines Plain or styled lines.
#' @param query A literal search string.
#' @return Occurrence identifiers, rows, grapheme bounds, and cell bounds.
#' @noRd
.panel_matches <- function(lines, query) {
  stopifnot("lines must be nonmissing text" = is.character(lines) && !anyNA(lines),
            "query must be a scalar string" = is.character(query) && length(query) == 1L && !is.na(query))
  empty <- data.frame(id = integer(), row = integer(), first = integer(), last = integer(),
    column = integer(), end_column = integer())
  if (!nzchar(query) || !length(lines)) return(empty)
  plain <- gsub(.screen_ansi_pattern(), "", lines, perl = TRUE)
  # A wrapping boundary represents one space. Empty rows retain paragraph
  # boundaries. Only the visual margins are trimmed, not interior whitespace.
  left <- nchar(plain, type = "chars") - nchar(sub("^ +", "", plain), type = "chars")
  text <- trimws(plain, whitespace = " ")
  lengths <- nchar(text, type = "chars")
  starts <- cumsum(c(1L, utils::head(lengths, -1L) + 1L))
  ends <- starts + lengths - 1L
  separators <- ifelse(lengths > 0L & c(utils::tail(lengths, -1L), 0L) > 0L, " ", "\n")
  joined <- paste0(paste0(text, separators), collapse = "")
  hits <- stringi::stri_locate_all_fixed(joined, query, omit_no_match = TRUE,
    opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))[[1L]]
  if (!nrow(hits)) return(empty)
  cache <- new.env(parent = emptyenv())
  geometry <- function(row) {
    key <- as.character(row)
    if (!exists(key, cache, inherits = FALSE)) {
      chars <- .editor_graphemes(plain[[row]])
      assign(key, list(chars = cumsum(nchar(chars, type = "chars")),
        cells = c(0L, cumsum(.editor_widths(chars)))), cache)
    }
    get(key, cache, inherits = FALSE)
  }
  rows <- lapply(seq_len(nrow(hits)), function(id) {
    first_row <- max(1L, findInterval(hits[id, 1L], starts))
    last_row <- min(length(lines), max(first_row, findInterval(hits[id, 2L], starts)))
    records <- seq.int(first_row, last_row)
    records <- records[lengths[records] > 0L & ends[records] >= hits[id, 1L] & starts[records] <= hits[id, 2L]]
    if (!length(records)) return(NULL)
    do.call(rbind, lapply(records, function(row) {
      g <- geometry(row)
      first_char <- max(hits[id, 1L], starts[[row]]) - starts[[row]] + 1L + left[[row]]
      last_char <- min(hits[id, 2L], ends[[row]]) - starts[[row]] + 1L + left[[row]]
      first <- findInterval(first_char - 1L, g$chars) + 1L
      last <- findInterval(last_char - 1L, g$chars) + 1L
      data.frame(id = id, row = row, first = first, last = last,
        column = g$cells[[first]], end_column = g$cells[[last + 1L]])
    }))
  })
  rows <- Filter(Negate(is.null), rows)
  out <- if (length(rows)) do.call(rbind, rows) else empty
  # Distinct code-point hits can coincide inside one grapheme. Paint that
  # visible character once so a combining sequence is never split or repeated.
  out <- out[!duplicated(out[c("row", "first", "last")]), , drop = FALSE]
  rownames(out) <- NULL
  out$id <- match(out$id, unique(out$id))
  stopifnot("search hits must stay on a displayed line" =
              all(out$row >= 1L & out$row <= length(lines)) && all(out$first <= out$last))
  out
}

#' Count Logical Matches Across Wrapped Physical Lines
#' @param matches Occurrence fragments.
#' @return A nonnegative integer count.
#' @noRd
.panel_match_count <- function(matches) {
  stopifnot("matches must be a data frame" = is.data.frame(matches))
  out <- length(unique(matches$id))
  stopifnot("match count must be nonnegative" = .is_count(out))
  out
}

#' Paint All Search Occurrences While Retaining Neighboring Syntax Styles
#' @param text A safe styled line.
#' @param matches Occurrences on this line.
#' @param current_match The active global occurrence identifier.
#' @param config Configuration.
#' @return One styled line with the same visible text.
#' @noRd
.panel_highlight <- function(text, matches, current_match, config) {
  stopifnot("text must be a scalar string" = is.character(text) && length(text) == 1L && !is.na(text),
            "matches must be a data frame" = is.data.frame(matches),
            "current match must be nonnegative" = .is_count(current_match),
            "config must be a configuration" = inherits(config, "prismrepl_config"))
  if (!nrow(matches)) return(text)
  parts <- .screen_tokens(text)
  chars <- parts$tokens[parts$printable]
  previous <- c(0L, utils::head(matches$last, -1L))
  chunks <- lapply(seq_len(nrow(matches)), function(i) c(
    .screen_token_slice(parts, previous[[i]] + 1L, matches$first[[i]] - 1L),
    style_text(paste0(chars[seq.int(matches$first[[i]], matches$last[[i]])], collapse = ""),
      if (matches$id[[i]] == current_match) "success_badge" else "warning_badge", config$theme, config$color)))
  out <- paste0(c(unlist(chunks, use.names = FALSE),
    .screen_token_slice(parts, utils::tail(matches$last, 1L) + 1L, length(chars))), collapse = "")
  stopifnot("search highlighting must preserve visible text" =
    identical(gsub(.screen_ansi_pattern(), "", out, perl = TRUE),
              gsub(.screen_ansi_pattern(), "", text, perl = TRUE)))
  out
}

#' Move to a Literal Occurrence, Including Several on the Same Line
#' @param matches Search locations.
#' @param current Current occurrence index, or zero.
#' @param direction One for next, minus one for previous.
#' @param cursor Current displayed row when no occurrence is selected.
#' @return A row, horizontal cell, and one-based occurrence index.
#' @noRd
.panel_match_step <- function(matches, current, direction = 1L, cursor = 1L) {
  stopifnot("matches must be nonempty" = is.data.frame(matches) && nrow(matches) > 0L,
            "current must identify an occurrence or zero" = .is_count(current) && current <= .panel_match_count(matches),
            "direction must be one or minus one" = direction %in% c(-1L, 1L),
            "cursor must be a positive row" = .is_count(cursor, 1L))
  matches <- matches[!duplicated(matches$id), , drop = FALSE]
  if (current > 0L) index <- (current - 1L + direction) %% nrow(matches) + 1L else {
    candidates <- which(if (direction > 0L) matches$row >= cursor else matches$row <= cursor)
    index <- if (length(candidates)) (if (direction > 0L) candidates[[1L]] else utils::tail(candidates, 1L))
      else if (direction > 0L) 1L else nrow(matches)
  }
  out <- list(index = as.integer(index), row = matches$row[[index]], column = matches$column[[index]])
  stopifnot("match navigation must preserve a valid index" = out$index >= 1L && out$index <= nrow(matches))
  out
}

#' Pan Only When a Selected Match Falls Outside the Visible Columns
#' @param matches Search fragments.
#' @param index Logical occurrence identifier.
#' @param horizontal Current zero-based column offset.
#' @param width Visible content cells.
#' @return A nonnegative offset that reveals the occurrence's beginning.
#' @noRd
.panel_reveal_match <- function(matches, index, horizontal, width) {
  stopifnot("matches must identify the requested occurrence" = is.data.frame(matches) && index %in% matches$id,
            "horizontal must be nonnegative" = .is_count(horizontal),
            "width must be positive" = .is_count(width, 1L))
  hit <- matches[match(index, matches$id), , drop = FALSE]
  out <- if (hit$column < horizontal || hit$end_column > horizontal + width)
    max(0L, hit$column - min(8L, width %/% 4L)) else horizontal
  stopifnot("match offset must be nonnegative" = .is_count(out))
  out
}
