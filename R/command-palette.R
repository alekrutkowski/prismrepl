#' Filter and Order the Console Command Catalog
#' @param catalog Command records.
#' @param query Space-separated literal search terms.
#' @param sort Logical groups or alphabetical order.
#' @return A filtered data frame in display order.
#' @noRd
.palette_data <- function(catalog, query = "", sort = "groups") {
  stopifnot("catalog must contain command metadata" = is.data.frame(catalog) &&
              all(c("id", "command", "alias", "group", "description", "insert") %in% names(catalog)),
            "query must be scalar text" = is.character(query) && length(query) == 1L && !is.na(query),
            "sort must be groups or alphabetical" = sort %in% c("groups", "alphabetical"))
  terms <- strsplit(trimws(query), "[[:space:]]+", perl = TRUE)[[1L]]
  terms <- terms[nzchar(terms)]
  haystack <- do.call(paste, c(unclass(catalog[c("id", "command", "alias", "group", "description")]), sep = " "))
  matches <- if (!length(terms)) rep(TRUE, nrow(catalog)) else Reduce(`&`, lapply(terms,
    function(term) stringi::stri_detect_fixed(haystack, term,
      opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))))
  out <- catalog[matches, , drop = FALSE]
  if (sort == "alphabetical") out <- out[order(out$id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  stopifnot("palette rows must remain valid commands" = !anyDuplicated(out$id) && !anyNA(out$insert))
  out
}

#' Create a Command Palette Navigation State
#' @param query Initial filter.
#' @return A navigation state without any execution side effects.
#' @noRd
.palette_state <- function(query = "") {
  stopifnot("query must be scalar text" = is.character(query) && length(query) == 1L && !is.na(query))
  out <- list(query = query, sort = "groups", cursor = 1L, top = 1L,
              chosen = NULL, quit = FALSE, status = "")
  stopifnot("new palette must have no chosen command" = is.null(out$chosen))
  out
}

#' Keep Each Palette Command Beside Its Description
#' @param data Filtered command catalog.
#' @param width Content width in terminal cells.
#' @param grouped Include logical section headings.
#' @return Lines and their filtered command indices; section headings use zero.
#' @noRd
.palette_lines <- function(data, width, grouped = TRUE) {
  stopifnot("data must contain command records" = is.data.frame(data) && "command" %in% names(data),
            "width must be positive" = .is_count(width, 1L), "grouped must be logical" = .is_flag(grouped))
  if (!nrow(data)) return(list(lines = "No commands match. Backspace or Ctrl+U changes the filter.", records = 0L))
  table <- .table_format(data.frame(command = paste0(data$command,
    ifelse(nzchar(data$alias), paste0("  (:", sub("^:", "", data$alias), ")"), "")),
    description = data$description, stringsAsFactors = FALSE), width)
  indices <- if (grouped) split(seq_len(nrow(data)), factor(data$group, levels = unique(data$group))) else
    list(seq_len(nrow(data)))
  chunks <- lapply(indices, function(ids) {
    selected <- which(table$records %in% ids)
    lines <- table$lines[selected]
    records <- table$records[selected]
    if (grouped) list(lines = c(paste0("[", data$group[[ids[[1L]]]], "]"), lines),
                     records = c(0L, records)) else list(lines = lines, records = records)
  })
  out <- list(lines = unlist(lapply(chunks, `[[`, "lines"), use.names = FALSE),
              records = unlist(lapply(chunks, `[[`, "records"), use.names = FALSE))
  stopifnot("palette records must correspond to display lines" = length(out$lines) == length(out$records),
            "palette records must be valid indices" = all(out$records %in% c(0L, seq_len(nrow(data)))))
  out
}

#' Compose a Searchable Command Palette Frame
#' @param state Palette navigation state.
#' @param config Console configuration.
#' @param size Actual terminal dimensions.
#' @param catalog Command metadata.
#' @param cache Optional panel cache.
#' @return Styled rows with hit-test and viewport attributes.
#' @noRd
.palette_frame <- function(state, config, size, catalog = .command_catalog(config$commands_file), cache = NULL) {
  stopifnot("state must contain a query" = is.list(state) && is.character(state$query),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  data <- .palette_data(catalog, state$query, state$sort)
  layout <- .panel_layout(size, 3L)
  content <- .palette_lines(data, max(1L, layout$width - 2L), state$sort == "groups")
  active <- which(content$records == min(state$cursor, nrow(data)))
  if (!nrow(data)) active <- integer()
  cursor <- if (length(active)) active[[1L]] else 1L
  top <- max(1L, min(state$top, max(1L, length(content$lines) - layout$page + 1L)))
  if (cursor < top) top <- cursor
  if (cursor >= top + layout$page) top <- cursor - layout$page + 1L
  caption <- .footer_hints(stats::setNames(c("stage command", "move", "sort", "clear", "cancel"),
    c(.viewer_keys(config, "submit"), .viewer_keys(config, c("up", "down")),
      .viewer_keys(config, "complete"), "Ctrl+U", .viewer_keys(config, "escape"))), config, max(1L, layout$width - 1L))
  footer <- if (nzchar(state$status)) state$status else paste0(nrow(data), "/", nrow(catalog),
    " commands. Selection fills the input; review arguments and press Enter there to run.")
  rows <- .panel_frame(content$lines, "Command palette", config, size, cursor = cursor, top = top,
    caption = caption, footer = footer, cache = cache,
    header = c(paste0("Search: ", if (nzchar(state$query)) state$query else "type a name, alias or description"),
      paste0("Sort: ", if (state$sort == "groups") "logical groups" else "alphabetical")), active_rows = active)
  hits <- rep(NA_integer_, length(rows))
  visible <- seq.int(top, min(length(content$lines), top + layout$page - 1L))
  hits[layout$header + seq_along(visible)] <- ifelse(content$records[visible] > 0L,
    content$records[visible], NA_integer_)
  attr(rows, "palette_records") <- hits
  attr(rows, "palette_top") <- top
  attr(rows, "palette_page") <- max(1L, length(unique(content$records[visible][content$records[visible] > 0L])))
  stopifnot("palette frame must match the terminal" = length(rows) == size[["height"]])
  rows
}

#' Apply One Palette Navigation Event Without Running Commands
#' @param state Palette state.
#' @param event Native input event, after mouse-selection dispatch.
#' @param config Console configuration.
#' @param catalog Command catalog.
#' @param page Commands per page.
#' @param hit Command index under the pointer, or NA.
#' @return Updated state; chosen is insertion text, never evaluated code.
#' @noRd
.palette_apply <- function(state, event, config, catalog = .command_catalog(config$commands_file), page = 8L, hit = NA_integer_) {
  stopifnot("state and event must be lists" = is.list(state) && is.list(event),
            "page must be positive" = .is_count(page, 1L))
  data <- .palette_data(catalog, state$query, state$sort)
  key <- .viewer_action(event, config)
  ctrl <- "ctrl" %in% event$modifiers
  state$status <- ""
  if (identical(event$type, "eof") || key == "escape") { state$quit <- TRUE; return(state) }
  if (key == "tab" || (ctrl && identical(event$key, "s"))) {
    selected <- if (nrow(data)) data$id[[min(state$cursor, nrow(data))]] else ""
    state$sort <- if (state$sort == "groups") "alphabetical" else "groups"
    data <- .palette_data(catalog, state$query, state$sort)
    state$cursor <- if (selected %in% data$id) match(selected, data$id) else 1L
    state$top <- 1L
  } else if (identical(event$type, "mouse")) {
    if (event$key %in% c("wheelup", "wheeldown")) state$cursor <- state$cursor +
      if (event$key == "wheelup") -3L else 3L
    else if (identical(event$action, "press") && identical(event$button, "left") &&
             !is.na(hit) && hit >= 1L && hit <= nrow(data)) {
      state$cursor <- as.integer(hit)
      state$chosen <- data$insert[[hit]]
    }
  } else if (key == "enter" && nrow(data)) state$chosen <- data$insert[[min(state$cursor, nrow(data))]]
  else if (key == "up") state$cursor <- state$cursor - 1L
  else if (key == "down") state$cursor <- state$cursor + 1L
  else if (key == "pageup") state$cursor <- state$cursor - page
  else if (key == "pagedown") state$cursor <- state$cursor + page
  else if (key == "home") state$cursor <- 1L
  else if (key == "end") state$cursor <- nrow(data)
  else if (key == "help") state$status <- "Type to search names and descriptions. Tab changes sort. Enter or a click stages; Escape cancels."
  else {
    before <- state$query
    if (ctrl && identical(event$key, "u")) state$query <- ""
    else if (key == "backspace") state$query <- paste(utils::head(.editor_graphemes(state$query), -1L), collapse = "")
    else if (event$type %in% c("text", "paste") && !ctrl) state$query <- substr(paste0(state$query,
      gsub("[\r\n\t]", " ", .safe_text(event$text))), 1L, 200L)
    if (!identical(before, state$query)) { state$cursor <- state$top <- 1L }
  }
  count <- nrow(.palette_data(catalog, state$query, state$sort))
  state$cursor <- max(1L, min(max(1L, count), state$cursor))
  stopifnot("palette cursor must be positive" = state$cursor >= 1L,
            "chosen command must be NULL or scalar text" = is.null(state$chosen) || .is_text(state$chosen))
  state
}

#' Choose a Command to Stage in the Console Input
#' @param config Console configuration.
#' @param query Initial name or description filter.
#' @param catalog Command metadata to browse; defaults to this configuration's
#'   built-in and custom commands. Selecting an entry only stages its text.
#' @return Insertion text or NULL; this function never executes a command.
#' @noRd
.command_palette <- function(config, query = "", catalog = .command_catalog(config$commands_file)) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"),
            "query must be scalar text" = is.character(query) && length(query) == 1L && !is.na(query))
  state <- .palette_state(query)
  session <- .viewer_start(config)
  on.exit(.viewer_finish(session, config), add = TRUE)
  size <- term_size()
  if (is.null(session$handle)) {
    data <- .palette_data(catalog, query)
    .emit(.palette_lines(data, .viewer_width(config))$lines, "output", config)
    return(invisible(NULL))
  }
  cache <- new.env(parent = emptyenv())
  previous <- NULL
  mouse <- .mouse_state()
  frame <- NULL
  redraw <- TRUE
  repeat {
    if (redraw) {
      frame <- .palette_frame(state, config, size, catalog, cache)
      state$top <- attr(frame, "palette_top")
      previous <- .panel_draw(.mouse_paint(frame, mouse, config), previous, size, config)
      redraw <- FALSE
    }
    event <- term_read(session$handle, 50L)
    if (identical(event$type, "timeout")) next
    dispatched <- .mouse_dispatch(event, frame, mouse, config)
    mouse <- dispatched$selection
    redraw <- isTRUE(dispatched$redraw)
    if (nzchar(dispatched$status)) state$status <- dispatched$status
    event <- dispatched$event
    if (is.null(event)) next
    if (identical(event$type, "resize")) { size <- term_size(); previous <- NULL; redraw <- TRUE; next }
    hits <- attr(frame, "palette_records")
    hit <- if (identical(event$type, "mouse") && !is.null(event$y) && event$y >= 1L && event$y <= length(hits))
      hits[[event$y]] else NA_integer_
    state <- .palette_apply(state, event, config, catalog, attr(frame, "palette_page"), hit)
    if (state$quit || !is.null(state$chosen)) break
    redraw <- TRUE
  }
  out <- if (state$quit) NULL else state$chosen
  stopifnot("palette must return insertion text or NULL" = is.null(out) || .is_text(out))
  invisible(out)
}

#' Open the Searchable Command Reference
#' @param config Console configuration.
#' @param query Optional literal filter.
#' @return Selected insertion text, or NULL.
#' @noRd
.command_help <- function(config, query = "") {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  out <- .command_palette(config, query)
  stopifnot("help must return text or NULL" = is.null(out) || .is_text(out))
  invisible(out)
}
