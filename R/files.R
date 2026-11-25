#' Read a Bounded Directory Listing
#' @param path Directory to inspect.
#' @param limit Maximum entries, excluding the parent entry.
#' @param query Literal, case-insensitive filename filter.
#' @return A data frame of names, paths, directory flags and byte sizes.
#' @noRd
.file_entries <- function(path, limit = 1000L, query = "") {
  stopifnot("directory must be scalar text" = .is_text(path), "limit must be positive" = .is_count(limit, 1L),
            "query must be a scalar string" = is.character(query) && length(query) == 1L && !is.na(query))
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = TRUE)
  stopifnot("path must be a directory" = dir.exists(path))
  names <- list.files(path, all.files = TRUE, no.. = TRUE, full.names = FALSE)
  if (nzchar(query)) names <- names[stringi::stri_detect_fixed(names, query,
    opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))]
  count <- length(names)
  names <- utils::head(names, limit)
  paths <- file.path(path, names)
  metadata <- file.info(paths)
  dirs <- !is.na(metadata$isdir) & metadata$isdir
  order <- order(!dirs, tolower(names), names)
  out <- data.frame(name = c("..", names[order]), path = c(dirname(path), paths[order]),
    directory = c(TRUE, dirs[order]), bytes = c(NA_real_, metadata$size[order]), stringsAsFactors = FALSE)
  attr(out, "total") <- count
  stopifnot("directory listing must have aligned names and paths" = nrow(out) <= limit + 1L &&
              !anyNA(out$name) && all(nzchar(out$path)))
  out
}

#' Recognize Explicit Unicode Byte Order Marks
#' @param raw Bounded file bytes.
#' @return Encoding name and number of leading BOM bytes, or UTF-8 with no BOM.
#' @noRd
.file_text_encoding <- function(raw) {
  stopifnot("file bytes must be raw" = is.raw(raw))
  marks <- list(`UTF-32BE` = as.raw(c(0, 0, 254, 255)),
    `UTF-32LE` = as.raw(c(255, 254, 0, 0)), `UTF-8` = as.raw(c(239, 187, 191)),
    `UTF-16BE` = as.raw(c(254, 255)), `UTF-16LE` = as.raw(c(255, 254)))
  found <- which(vapply(marks, function(mark) identical(utils::head(raw, length(mark)), mark), logical(1L)))
  out <- if (length(found)) list(encoding = names(marks)[found[[1L]]], bom = length(marks[[found[[1L]]]]))
    else list(encoding = "UTF-8", bom = 0L)
  stopifnot("encoding must be recognized Unicode" = out$encoding %in% names(marks),
    "BOM size must be bounded" = .is_count(out$bom) && out$bom <= 4L)
  out
}

#' Clip Only an Incomplete Character at a Preview Byte Boundary
#' @param raw Bounded bytes with any BOM removed.
#' @param encoding Explicit Unicode encoding name.
#' @param truncated Whether further bytes exist beyond the preview budget.
#' @return A raw prefix ending at a complete encoding unit when truncated.
#' @noRd
.file_text_prefix <- function(raw, encoding, truncated) {
  stopifnot("file bytes must be raw" = is.raw(raw),
    "encoding must be recognized" = encoding %in% c("UTF-8", "UTF-16LE", "UTF-16BE", "UTF-32LE", "UTF-32BE"),
    "truncation flag must be logical" = .is_flag(truncated))
  count <- length(raw)
  keep <- count
  if (truncated && count) {
    if (startsWith(encoding, "UTF-32")) keep <- count - count %% 4L
    else if (startsWith(encoding, "UTF-16")) {
      keep <- count - count %% 2L
      if (keep >= 2L) {
        pair <- as.integer(raw[(keep - 1L):keep])
        unit <- if (encoding == "UTF-16LE") pair[[1L]] + 256L * pair[[2L]] else pair[[2L]] + 256L * pair[[1L]]
        if (unit >= 55296L && unit <= 56319L) keep <- keep - 2L
      }
    } else {
      units <- as.integer(raw)
      leads <- which(units < 128L | units >= 192L)
      if (length(leads)) {
        last <- utils::tail(leads, 1L)
        lead <- units[[last]]
        needed <- if (lead >= 194L && lead <= 223L) 2L else if (lead >= 224L && lead <= 239L) 3L
          else if (lead >= 240L && lead <= 244L) 4L else 1L
        if (count - last + 1L < needed) keep <- last - 1L
      }
    }
  }
  out <- utils::head(raw, keep)
  stopifnot("complete prefix must stay within the input" = is.raw(out) && length(out) <= count)
  out
}

#' Convert File Bytes with Portable Unicode Converters
#' @param raw Bytes to decode or re-encode.
#' @param from,to Explicit input and output encodings.
#' @return Converted raw bytes, or NULL on error, and a malformed-input flag.
#' @noRd
.file_text_convert <- function(raw, from, to = "UTF-8") {
  stopifnot("file bytes must be raw" = is.raw(raw), "encoding names must be text" = .is_text(from) && .is_text(to))
  invalid <- FALSE
  value <- tryCatch(withCallingHandlers(stringi::stri_encode(raw, from = from, to = to,
    to_raw = TRUE)[[1L]], warning = function(w) {
      invalid <<- TRUE
      invokeRestart("muffleWarning")
    }), error = function(e) NULL)
  out <- list(bytes = value, invalid = invalid)
  stopifnot("converted bytes must be raw or absent" = is.null(out$bytes) || is.raw(out$bytes),
    "malformed-input status must be logical" = .is_flag(out$invalid))
  out
}

#' Recognize Conservative BOMless UTF-16 Text
#' @param raw Bounded bytes without an explicit BOM.
#' @param truncated Whether the byte budget omitted later content.
#' @return A UTF-16 encoding name, or NULL when evidence is insufficient.
#' @noRd
.file_text_utf16 <- function(raw, truncated) {
  stopifnot("file bytes must be raw" = is.raw(raw), "truncation flag must be logical" = .is_flag(truncated))
  pairs <- length(raw) %/% 2L
  if (pairs < 8L) return(NULL)
  odd <- raw[seq.int(1L, pairs * 2L, by = 2L)] == as.raw(0L)
  even <- raw[seq.int(2L, pairs * 2L, by = 2L)] == as.raw(0L)
  encoding <- if (!any(odd) && mean(even) >= 0.6) "UTF-16LE" else
    if (!any(even) && mean(odd) >= 0.6) "UTF-16BE" else return(NULL)
  prefix <- .file_text_prefix(raw, encoding, truncated)
  converted <- .file_text_convert(prefix, encoding)
  if (is.null(converted$bytes) || converted$invalid || any(converted$bytes == as.raw(0L))) return(NULL)
  points <- utf8ToInt(rawToChar(converted$bytes))
  if (any((points < 32L & !points %in% c(9L, 10L, 13L)) | (points >= 127L & points <= 159L))) return(NULL)
  restored <- .file_text_convert(converted$bytes, "UTF-8", encoding)
  out <- if (!restored$invalid && identical(restored$bytes, prefix)) encoding else NULL
  stopifnot("inferred encoding must be absent or UTF-16" = is.null(out) || out %in% c("UTF-16LE", "UTF-16BE"))
  out
}

#' Prepare Safe Text from One File Without Executing It
#' @param path File to preview.
#' @param bytes Preview byte budget. One extra byte detects truncation; at least
#'   four bytes are read to distinguish Unicode byte-order marks.
#' @param lines Maximum logical lines retained.
#' @return Plain text, language and encoding hints, and a truncation flag.
#' @noRd
.file_preview <- function(path, bytes = 65536L, lines = 400L) {
  stopifnot("file must be text" = .is_text(path), "byte and line limits must be positive" =
              .is_count(bytes, 1L) && .is_count(lines, 1L))
  stopifnot("file must exist and must not be a directory" = file.exists(path) && !dir.exists(path))
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  # Four bytes distinguish UTF-32 from UTF-16 even for a tiny preview budget.
  raw <- readBin(connection, "raw", n = max(4, as.double(bytes) + 1))
  truncated <- length(raw) > bytes
  format <- .file_text_encoding(raw)
  payload <- utils::head(raw, bytes)
  payload <- utils::tail(payload, max(0L, length(payload) - format$bom))
  if (!format$bom && any(payload == as.raw(0L))) {
    inferred <- .file_text_utf16(payload, truncated)
    if (!is.null(inferred)) format$encoding <- inferred
  }
  payload <- .file_text_prefix(payload, format$encoding, truncated)
  converted <- .file_text_convert(payload, format$encoding)
  binary <- is.null(converted$bytes) || any(converted$bytes == as.raw(0L))
  text <- if (binary) "" else rawToChar(converted$bytes)
  points <- if (binary) integer() else utf8ToInt(text)
  controls <- (points < 32L & !points %in% c(9L, 10L, 12L, 13L, 27L)) | (points >= 127L & points <= 159L)
  binary <- binary || (sum(controls) >= 2L && mean(controls) > 0.1)
  if (binary) return(list(lines = paste0("Binary file: text preview is unavailable.",
    if (!format$bom) " If this is text, save a copy as UTF-8 or UTF-16 with a byte order mark." else ""),
    language = "text", encoding = format$encoding, truncated = truncated))
  text <- gsub("\r\n?", "\n", text, perl = TRUE)
  parts <- stringi::stri_split_fixed(text, "\n", omit_empty = FALSE)[[1L]]
  truncated <- truncated || length(parts) > lines
  out <- list(lines = .safe_text(utils::head(parts, lines)),
    language = if (grepl("\\.[Rr]$", path)) "R" else "text", encoding = format$encoding, truncated = truncated)
  if (converted$invalid) out$lines <- c(out$lines, "", paste0("[Invalid ", format$encoding,
    " sequences were replaced in this preview; file is unchanged.]"))
  if (truncated) out$lines <- c(out$lines, "", "[Preview truncated; file is unchanged.]")
  stopifnot("preview must be safe text" = is.character(out$lines) && !anyNA(out$lines))
  out
}

#' Format Directory Rows in Stable Columns
#' @param entries Directory-entry data frame.
#' @param width Content width.
#' @return Aligned rows and an aligned heading.
#' @noRd
.file_table <- function(entries, width) {
  stopifnot("entries must have file metadata" = is.data.frame(entries) &&
              all(c("name", "directory", "bytes") %in% names(entries)), "width must be positive" = .is_count(width, 1L))
  names <- paste0(.safe_text(entries$name), ifelse(entries$directory, "/", ""))
  if (width < 32L) return(list(lines = .panel_fit(names, width), heading = .panel_fit("Name", width)))
  a <- width - 24L
  kind <- ifelse(entries$directory, "folder", ifelse(grepl("\\.[Rr]$", entries$name), "R script", "file"))
  size <- vapply(seq_len(nrow(entries)), function(i) if (entries$directory[[i]] || is.na(entries$bytes[[i]])) "" else
    format(structure(entries$bytes[[i]], class = "object_size"), units = "auto"), character(1L))
  out <- list(lines = paste0(.explorer_fit(names, a), "  ", .panel_fit(kind, 9L), "  ", .panel_fit(size, 11L)),
    heading = paste0(.panel_fit("Name", a), "  ", .panel_fit("Kind", 9L), "  ", .panel_fit("Size", 11L)))
  stopifnot("file rows must fit" = all(.panel_cells(out$lines) <= width))
  out
}

#' Browse Files and Stage Explicit Directory or Source Commands
#' @param path Starting directory or preview file.
#' @param config Configuration.
#' @return A staged command, or NULL on close. Browsing never executes files.
#' @noRd
.file_explorer <- function(path, config) {
  stopifnot("path must be text" = .is_text(path), "config must be a console configuration" = inherits(config, "prismrepl_config"))
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = TRUE)
  preview_path <- if (dir.exists(path)) NULL else path
  directory <- if (is.null(preview_path)) path else dirname(path)
  entries <- .file_entries(directory)
  preview <- if (is.null(preview_path)) NULL else .file_preview(preview_path)
  session <- .viewer_start(config)
  on.exit(.viewer_finish(session, config), add = TRUE)
  if (is.null(session$handle)) {
    .emit(if (is.null(preview)) .file_table(entries, .viewer_width(config))$lines else preview$lines, "output", config)
    return(invisible(NULL))
  }
  size <- term_size()
  cursor <- top <- 1L
  query <- ""
  searching <- FALSE
  previous <- NULL
  cache <- new.env(parent = emptyenv())
  selection <- .mouse_state()
  status <- ""
  last_click <- NULL
  redraw <- TRUE
  frame <- NULL
  repeat {
    layout <- .panel_layout(size, 3L)
    width <- max(1L, layout$width - 2L)
    if (redraw) {
      data <- .screen_cached(cache, "files_content", list(entries, preview_path, preview,
        width, config$theme, config$color, config$rainbow), {
          prepared <- if (is.null(preview)) .file_table(entries, width) else list(
            lines = if (preview$language == "R") .highlight_r_window(paste(preview$lines, collapse = "\n"),
              1L, max(1L, length(preview$lines)), config$theme, config$color,
              rainbow = config$rainbow) else preview$lines,
            heading = "Read-only preview; Escape returns to the directory")
          if (!is.null(preview)) prepared$lines <- unlist(lapply(prepared$lines,
            .screen_wrap, width = width), use.names = FALSE)
          prepared
        })
      lines <- data$lines
      cursor <- max(1L, min(length(lines), cursor))
      if (cursor < top) top <- cursor
      if (cursor >= top + layout$page) top <- cursor - layout$page + 1L
      hints <- if (is.null(preview)) stats::setNames(c("open", "parent", "filter", "stage :cd", "stage :source", "close"),
        c(.viewer_keys(config, "submit"), .viewer_keys(config, "backspace"), "/", "c", "s", .viewer_keys(config, "escape"))) else
        stats::setNames(c("scroll", "page", "stage :source", "back"),
          c(.viewer_keys(config, c("up", "down")), .viewer_keys(config, "scroll_down"), "s", .viewer_keys(config, "escape")))
      caption <- if (searching) paste0("Filter: ", query) else .footer_hints(hints, config, layout$width - 1L)
      footer <- if (nzchar(status)) status else if (!is.null(preview)) paste0("Line ", cursor, "/", length(lines), "  File is unchanged") else
        paste0(nrow(entries) - 1L, " entries", if (attr(entries, "total") > nrow(entries) - 1L) " (bounded listing; filter to narrow it)" else "",
               "  Click selects; double-click opens; drag copies")
      frame <- .panel_frame(lines, "Files", config, size, cursor = cursor, top = top,
        caption = caption, footer = footer, cache = cache,
        header = c(if (is.null(preview)) directory else preview_path, paste0(" ", data$heading)),
        active_rows = if (is.null(preview)) cursor else integer(), styled = !is.null(preview))
      previous <- .panel_draw(.mouse_paint(frame, selection, config), previous, size, config)
      redraw <- FALSE
    }
    event <- term_read(session$handle, 50L)
    if (identical(event$type, "timeout")) next
    if (identical(event$type, "resize")) { size <- term_size(); previous <- NULL; selection <- .mouse_state(); redraw <- TRUE; next }
    if (!identical(event$type, "mouse") || identical(event$action, "move") ||
        event$key %in% c("wheelup", "wheeldown")) last_click <- NULL
    routed <- .mouse_dispatch(event, frame, selection, config)
    selection <- routed$selection
    if (.is_text(routed$status)) status <- routed$status
    event <- routed$event
    if (is.null(event)) { redraw <- isTRUE(routed$redraw); next }
    key <- .viewer_action(event, config)
    text <- if (is.null(event$text)) "" else event$text
    redraw <- TRUE
    if (identical(event$type, "eof")) return(invisible(NULL))
    if (!searching && identical(event$type, "paste")) {
      if (is.null(preview)) {
        searching <- TRUE
        query <- substr(gsub("[\r\n\t]", " ", .safe_text(text)), 1L, 200L)
        entries <- .file_entries(directory, query = query)
        cursor <- top <- 1L
      } else status <- "This file preview is read-only. Escape returns to the directory filter."
      next
    }
    if (searching) {
      if (key %in% c("escape", "enter")) searching <- FALSE
      else if (key == "backspace") query <- paste(utils::head(.editor_graphemes(query), -1L), collapse = "")
      else if (event$type %in% c("text", "paste")) query <- substr(paste0(query, gsub("[\r\n\t]", " ", text)), 1L, 200L)
      entries <- .file_entries(directory, query = query)
      cursor <- top <- 1L
      next
    }
    if (key == "escape" || text == "q") {
      if (!is.null(preview)) { preview <- preview_path <- NULL; cursor <- top <- 1L } else return(invisible(NULL))
      next
    }
    if (text == "c" && is.null(preview)) return(paste0(":cd ", directory))
    if (text == "s") {
      chosen <- if (is.null(preview)) entries$path[[cursor]] else preview_path
      if (!dir.exists(chosen) && grepl("\\.[Rr]$", chosen)) return(paste0(":source ", chosen))
      status <- "Select an R script to stage :source."
      next
    }
    if (text == "/" && is.null(preview)) { searching <- TRUE; query <- ""; next }
    if (identical(event$type, "mouse")) {
      if (event$key %in% c("wheelup", "wheeldown")) cursor <- cursor + if (event$key == "wheelup") -3L else 3L
      else if (identical(event$button, "left") && identical(event$action, "press") &&
          event$y > layout$header && event$y <= layout$header + layout$page) {
        hit <- top + event$y - layout$header - 1L
        if (hit > length(lines)) next
        now <- proc.time()[["elapsed"]]
        location <- list(directory, preview_path, hit)
        open <- !is.null(last_click) && identical(location, last_click$location) && now - last_click$time < 0.45
        cursor <- hit
        last_click <- list(location = location, time = now)
        if (open) key <- "enter"
      }
    }
    if (key == "up") cursor <- cursor - 1L
    if (key == "down") cursor <- cursor + 1L
    if (key == "pageup") cursor <- cursor - layout$page
    if (key == "pagedown") cursor <- cursor + layout$page
    if (key == "home") cursor <- 1L
    if (key == "end") cursor <- length(lines)
    cursor <- max(1L, min(length(lines), cursor))
    if (key == "backspace" || (key == "left" && is.null(preview))) {
      if (!is.null(preview)) preview <- preview_path <- NULL else directory <- dirname(directory)
      entries <- .file_entries(directory)
      cursor <- top <- 1L
      query <- ""
    } else if (key %in% c("enter", "right") && is.null(preview)) {
      chosen <- entries$path[[cursor]]
      if (entries$directory[[cursor]]) {
        directory <- normalizePath(chosen, winslash = "/", mustWork = TRUE)
        entries <- .file_entries(directory)
      } else {
        preview <- .file_preview(chosen)
        preview_path <- chosen
      }
      cursor <- top <- 1L
      query <- ""
      status <- ""
    }
  }
}

#' Compose Sidebar Modules with a Fair Height Budget
#' @param groups Named character vectors, optionally carrying commands and
#'   named hints attributes. Hints replace the final row and remain visible
#'   when a module's content is clipped.
#' @param width,height Available sidebar cells and rows.
#' @param config Configuration.
#' @return Styled rows and staged command targets.
#' @noRd
.sidebar_frame <- function(groups, width, height, config) {
  stopifnot("sidebar groups must be a named list" = is.list(groups) && !is.null(names(groups)),
            "sidebar dimensions must be positive" = .is_count(width, 1L) && .is_count(height, 1L))
  n <- length(groups)
  if (!n) return(list(lines = rep("", height), actions = rep("", height)))
  available <- max(0L, height - n - 1L)
  budgets <- rep(available %/% n, n)
  if (available %% n) budgets[seq_len(available %% n)] <- budgets[seq_len(available %% n)] + 1L
  counts <- lengths(groups)
  budgets <- pmin(budgets, counts)
  while (sum(budgets) < available && any(budgets < counts)) {
    i <- which(budgets < counts)[[1L]]
    budgets[[i]] <- budgets[[i]] + 1L
  }
  result <- lapply(seq_along(groups), function(i) {
    content <- .safe_text(as.character(groups[[i]]))
    commands <- attr(groups[[i]], "commands", exact = TRUE)
    hints <- attr(groups[[i]], "hints", exact = TRUE)
    if (is.null(commands)) commands <- rep("", length(content))
    has_hints <- length(hints) > 0L && length(content) > 0L
    primary <- switch(names(groups)[[i]], WORKSPACE = ":objects", PACKAGES = ":packages", FILES = ":files", ":help")
    count <- length(content) - as.integer(has_hints)
    budget <- max(0L, budgets[[i]] - as.integer(has_hints))
    selected <- seq_len(min(count, budget))
    shown <- content[selected]
    actions <- commands[selected]
    if (count > budget && budget > 0L) {
      shown[[length(shown)]] <- paste0("+", count - budget + 1L, " more")
      actions[[length(actions)]] <- primary
    }
    lines <- if (length(shown)) paste0("  ", .explorer_fit(shown, max(1L, width - 2L))) else character()
    if (has_hints && budgets[[i]] > 0L) {
      hint <- .footer_hints(hints, config, max(1L, width - 2L))
      lines <- c(lines, .screen_paint(paste0("  ", hint), "status", config))
      actions <- c(actions, primary)
    }
    list(lines = c(.screen_paint(paste0(" ", names(groups)[[i]]), "accent", config),
                   lines), actions = c("", actions))
  })
  lines <- c(.screen_rule("SESSION INFO", width, config), unlist(lapply(result, `[[`, "lines"), use.names = FALSE))
  commands <- c("", unlist(lapply(result, `[[`, "actions"), use.names = FALSE))
  out <- list(lines = utils::head(c(lines, rep("", height)), height), actions = utils::head(c(commands, rep("", height)), height))
  stopifnot("sidebar rows and actions must align" = length(out$lines) == height && length(out$actions) == height)
  out
}

#' Resolve a Short Sidebar Click to Editable Input
#' @param event A released short mouse click, replayed as a press.
#' @param frame The unpainted main screen frame.
#' @return A staged command string or NULL.
#' @noRd
.sidebar_command <- function(event, frame) {
  stopifnot("event must be a list" = is.list(event), "frame must be a list or NULL" = is.null(frame) || is.list(frame))
  if (is.null(frame) || !isTRUE(frame$layout$sidebar) || !identical(event$type, "mouse") ||
      !identical(event$button, "left") || !identical(event$action, "press") ||
      is.null(event$x) || event$x <= frame$layout$main_width + 1L || is.null(event$y) ||
      event$y < 1L || event$y > length(frame$sidebar_actions)) return(NULL)
  out <- frame$sidebar_actions[[event$y]]
  if (!.is_text(out)) return(NULL)
  stopifnot("sidebar actions must stage colon commands" = startsWith(out, ":"))
  out
}
