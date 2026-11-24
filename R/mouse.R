#' Start a Screen-Cell Selection
#' @return An empty, inactive selection record.
#' @noRd
.mouse_state <- function() list(anchor = NULL, end = NULL, down = FALSE,
  moved = FALSE, rectangle = FALSE, press = NULL, lines = character())

#' Read the Visible Rows of Either Renderer
#' @param frame Styled rows or a compositor frame with lines.
#' @return Styled character rows.
#' @noRd
.mouse_lines <- function(frame) {
  lines <- if (is.list(frame)) frame$lines else frame
  stopifnot("mouse selection needs rendered character rows" = is.character(lines) && !anyNA(lines))
  lines
}

#' Find Whole Graphemes Overlapping a Cell Range
#' @param line One safe styled row.
#' @param first,last One-based, inclusive display columns.
#' @return Token metadata and inclusive printable-grapheme bounds.
#' @noRd
.mouse_span <- function(line, first, last) {
  stopifnot("line must be one string" = is.character(line) && length(line) == 1L && !is.na(line),
            "columns must be ordered positive integers" = .is_count(first, 1L) &&
              .is_count(last, first))
  parts <- .screen_tokens(line)
  widths <- parts$widths[parts$printable]
  ends <- cumsum(widths)
  starts <- ends - widths + 1L
  found <- which(ends >= first & starts <= last)
  list(parts = parts, first = if (length(found)) found[[1L]] else 1L,
       last = if (length(found)) tail(found, 1L) else 0L, count = length(widths))
}

#' Describe Selected Cell Ranges in Reading Order
#' @param selection A mouse selection record.
#' @return A data frame of one-based row, first, and last columns.
#' @noRd
.mouse_ranges <- function(selection) {
  stopifnot("selection must be a list" = is.list(selection))
  empty <- data.frame(row = integer(), first = integer(), last = integer())
  if (is.null(selection$anchor) || is.null(selection$end) || !isTRUE(selection$moved)) return(empty)
  a <- selection$anchor
  b <- selection$end
  if (a[[2L]] > b[[2L]] || (a[[2L]] == b[[2L]] && a[[1L]] > b[[1L]])) {
    a <- selection$end
    b <- selection$anchor
  }
  rows <- seq.int(a[[2L]], b[[2L]])
  if (selection$rectangle) {
    out <- data.frame(row = rows, first = min(a[[1L]], b[[1L]]), last = max(a[[1L]], b[[1L]]))
  } else {
    widths <- vapply(selection$lines[rows], .screen_width, numeric(1L))
    first <- rep(1L, length(rows))
    last <- pmax(1L, widths)
    first[[1L]] <- a[[1L]]
    last[[length(rows)]] <- b[[1L]]
    out <- data.frame(row = rows, first = first, last = pmax(first, last))
  }
  stopifnot("selection must use valid rows and columns" = all(out$row >= 1L &
              out$row <= length(selection$lines)) && all(out$last >= out$first))
  out
}

#' Extract Screen Selection Without ANSI or Partial Unicode Characters
#' @param selection A completed or active mouse selection.
#' @return One plain UTF-8 string in reading order.
#' @noRd
.mouse_text <- function(selection) {
  stopifnot("selection must be a list" = is.list(selection))
  ranges <- .mouse_ranges(selection)
  lines <- vapply(seq_len(nrow(ranges)), function(i) {
    row <- ranges[i, ]
    span <- .mouse_span(selection$lines[[row$row]], row$first, row$last)
    value <- if (span$last >= span$first) paste0(
      span$parts$tokens[span$parts$printable[seq.int(span$first, span$last)]], collapse = "") else ""
    if (selection$rectangle) value else sub("[ ]+$", "", value)
  }, character(1L))
  out <- enc2utf8(paste(lines, collapse = "\n"))
  stopifnot("selected text must be one valid string" = length(out) == 1L && !is.na(out) &&
              isTRUE(utf8::utf8_valid(out)))
  out
}

#' Overlay a Mouse Selection While Retaining Layout Metadata
#' @param frame Styled rows or a compositor frame.
#' @param selection A mouse selection record or NULL.
#' @param config Console configuration.
#' @return The same frame shape and attributes with selected cells highlighted.
#' @noRd
.mouse_paint <- function(frame, selection, config) {
  stopifnot("selection must be NULL or a list" = is.null(selection) || is.list(selection),
            "config must be a list" = is.list(config))
  if (is.null(selection) || !isTRUE(selection$moved)) return(frame)
  lines <- .mouse_lines(frame)
  ranges <- .mouse_ranges(selection)
  ranges <- ranges[ranges$row <= length(lines), , drop = FALSE]
  if (!nrow(ranges)) return(frame)
  selected <- vapply(seq_len(nrow(ranges)), function(i) {
    row <- ranges[i, ]
    span <- .mouse_span(lines[[row$row]], row$first, row$last)
    if (span$last < span$first) return(lines[[row$row]])
    before <- .screen_token_slice(span$parts, 1L, span$first - 1L)
    after <- .screen_token_slice(span$parts, span$last + 1L, span$count)
    value <- paste0(span$parts$tokens[span$parts$printable[seq.int(span$first, span$last)]], collapse = "")
    marked <- if (isTRUE(config$color)) style_text(value, "selection", config$theme, TRUE) else
      paste0("\033[7m", value, "\033[27m")
    paste0(before, marked, after)
  }, character(1L))
  lines[ranges$row] <- selected
  if (is.list(frame)) frame$lines <- lines else frame[] <- lines
  stopifnot("selection overlay must preserve row count" = length(.mouse_lines(frame)) == length(lines))
  frame
}

#' Route Mouse Selection Before View-Specific Click Actions
#' @param event Native event.
#' @param frame The unpainted visible frame.
#' @param selection Selection returned by the previous dispatch.
#' @param config Console configuration.
#' @return Event, selection, redraw flag, and a short status string. A NULL event
#'   is consumed. A click is delivered on release; dragging never activates it.
#' @noRd
.mouse_dispatch <- function(event, frame, selection = .mouse_state(), config) {
  stopifnot("event and config must be lists" = is.list(event) && is.list(config),
            "selection must be a list" = is.list(selection))
  finish <- function(event, selection, redraw = FALSE, status = "")
    list(event = event, selection = selection, redraw = redraw, status = status)
  if (!identical(event$type, "mouse")) {
    if (event$type %in% c("timeout")) return(finish(event, selection))
    return(finish(event, .mouse_state(), isTRUE(selection$moved)))
  }
  if (!isTRUE(config$mouse)) return(finish(event, .mouse_state(), isTRUE(selection$moved)))
  lines <- .mouse_lines(frame)
  if (!length(lines) || !.is_count(event$x, 1L) || !.is_count(event$y, 1L))
    return(finish(NULL, selection))
  point <- c(as.integer(event$x), as.integer(min(event$y, length(lines))))
  width <- max(1L, .screen_width(lines[[point[[2L]]]]))
  point[[1L]] <- min(point[[1L]], width)
  if (identical(event$button, "right") && identical(event$action, "press")) {
    clipboard <- .clipboard_read(if (.is_count(config$max_paste, 1L)) config$max_paste else 100000L)
    selection <- .mouse_state()
    if (!clipboard$ok) return(finish(NULL, selection, TRUE, clipboard$reason))
    pasted <- list(type = "paste", key = "", text = clipboard$text, modifiers = character(),
      truncated = FALSE, x = NA_integer_, y = NA_integer_, button = "", action = "")
    return(finish(pasted, selection, TRUE, clipboard$reason))
  }
  if (identical(event$button, "right")) return(finish(NULL, selection))
  if (identical(event$button, "left") && identical(event$action, "press")) {
    redraw <- isTRUE(selection$moved)
    selection <- .mouse_state()
    selection$anchor <- selection$end <- point
    selection$press <- event
    selection$down <- TRUE
    selection$rectangle <- "ctrl" %in% event$modifiers
    selection$lines <- lines
    return(finish(NULL, selection, redraw))
  }
  if (isTRUE(selection$down) && identical(event$button, "left") &&
      event$action %in% c("move", "release")) {
    changed <- !identical(selection$end, point)
    selection$end <- point
    selection$moved <- selection$moved || !identical(selection$anchor, point)
    if (identical(event$action, "move")) return(finish(NULL, selection, changed))
    selection$down <- FALSE
    if (!selection$moved) return(finish(selection$press, .mouse_state()))
    copied <- .clipboard_write(.mouse_text(selection))
    return(finish(NULL, selection, TRUE, copied$reason))
  }
  if (identical(event$action, "move") || identical(event$action, "release"))
    return(finish(NULL, selection))
  finish(event, .mouse_state(), isTRUE(selection$moved))
}

.clipboard_state <- new.env(parent = emptyenv())
.clipboard_state$text <- ""
.clipboard_state$has_text <- FALSE

#' Select an Installed Clipboard Helper Without Shell Interpolation
#' @param action Read or write.
#' @return A helper record or NULL.
#' @noRd
.clipboard_program <- function(action) {
  stopifnot("clipboard action must be read or write" = action %in% c("read", "write"))
  read <- action == "read"
  candidates <- list()
  if (nzchar(Sys.getenv("WAYLAND_DISPLAY"))) candidates <- c(candidates, list(list(
    command = if (read) "wl-paste" else "wl-copy", args = if (read) "--no-newline" else character(),
    backend = "Wayland")))
  if (nzchar(Sys.getenv("DISPLAY"))) candidates <- c(candidates, list(
    list(command = "xclip", args = c("-selection", "clipboard", if (read) "-o" else "-i"), backend = "X11"),
    list(command = "xsel", args = c("--clipboard", if (read) "--output" else "--input"), backend = "X11")))
  if (identical(Sys.info()[["sysname"]], "Darwin")) candidates <- c(candidates, list(list(
    command = if (read) "pbpaste" else "pbcopy", args = character(), backend = "macOS")))
  if (nzchar(Sys.getenv("WSL_DISTRO_NAME"))) candidates <- c(candidates, list(list(
    command = "powershell.exe", args = c("-NoProfile", "-NonInteractive", "-Command",
      shQuote(if (read) paste0("[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false); ",
        "[Console]::Write((Get-Clipboard -Raw))") else paste0(
        "[Console]::InputEncoding = [System.Text.UTF8Encoding]::new($false); ",
        "Set-Clipboard -Value ([Console]::In.ReadToEnd())"))), backend = "Windows through WSL")))
  for (candidate in candidates) {
    executable <- Sys.which(candidate$command)
    if (nzchar(executable)) { candidate$command <- unname(executable); return(candidate) }
  }
  NULL
}

#' Run a Bounded Clipboard Helper with Text Passed Through Files
#' @param helper A fixed executable and fixed arguments.
#' @param text NULL to read; a UTF-8 scalar to write.
#' @param limit Maximum pasted UTF-8 bytes.
#' @return An ok, text, reason record.
#' @noRd
.clipboard_external <- function(helper, text = NULL, limit = 100000L) {
  stopifnot("helper must have a command and arguments" = is.list(helper) && .is_text(helper$command),
            "text must be NULL or one string" = is.null(text) || (is.character(text) &&
              length(text) == 1L && !is.na(text)), "limit must be positive" = .is_count(limit, 1L))
  input <- tempfile("prism-clipboard-in-")
  output <- tempfile("prism-clipboard-out-")
  error <- tempfile("prism-clipboard-error-")
  on.exit(unlink(c(input, output, error)), add = TRUE)
  if (!is.null(text)) writeBin(charToRaw(enc2utf8(text)), input)
  status <- tryCatch(suppressWarnings(system2(helper$command, helper$args,
    stdin = if (is.null(text)) "" else input, stdout = output, stderr = error, timeout = 2)),
    error = function(e) 1L)
  fail <- function(reason) list(ok = FALSE, text = "", reason = reason)
  if (!identical(as.integer(status), 0L)) return(fail("The system clipboard helper is unavailable or timed out."))
  if (!is.null(text)) return(list(ok = TRUE, text = "", reason = ""))
  bytes <- file.info(output)$size
  if (is.na(bytes) || bytes > limit) return(fail("Clipboard text exceeds the paste limit; nothing was pasted."))
  raw <- readBin(output, "raw", n = limit + 1L)
  if (any(raw == as.raw(0))) return(fail("The clipboard contains embedded NUL bytes; nothing was pasted."))
  value <- rawToChar(raw)
  Encoding(value) <- "UTF-8"
  if (!isTRUE(utf8::utf8_valid(value))) return(fail("The clipboard helper did not return UTF-8 text."))
  list(ok = TRUE, text = value, reason = "")
}

#' Encode Plain UTF-8 Text for an OSC 52 Copy Request
#' @param text One string.
#' @return One base64-encoded ASCII string.
#' @noRd
.clipboard_base64 <- function(text) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text))
  bytes <- as.integer(charToRaw(enc2utf8(text)))
  if (!length(bytes)) return("")
  padding <- (3L - length(bytes) %% 3L) %% 3L
  blocks <- matrix(c(bytes, rep(0L, padding)), nrow = 3L)
  indices <- rbind(blocks[1L, ] %/% 4L,
    (blocks[1L, ] %% 4L) * 16L + blocks[2L, ] %/% 16L,
    (blocks[2L, ] %% 16L) * 4L + blocks[3L, ] %/% 64L, blocks[3L, ] %% 64L)
  alphabet <- c(LETTERS, letters, as.character(0:9), "+", "/")
  encoded <- alphabet[as.vector(indices) + 1L]
  if (padding) encoded[seq.int(length(encoded) - padding + 1L, length(encoded))] <- "="
  out <- paste0(encoded, collapse = "")
  stopifnot("base64 must use only its ASCII alphabet" = grepl("^[A-Za-z0-9+/]*={0,2}$", out))
  out
}

#' Copy an Explicit Mouse Selection to the Available Clipboard
#' @param text One plain Unicode string.
#' @return An ok, text, reason, backend record. Session copy is always retained.
#' @noRd
.clipboard_write <- function(text) {
  stopifnot("clipboard text must be valid Unicode" = is.character(text) && length(text) == 1L &&
              !is.na(text) && isTRUE(utf8::utf8_valid(text)))
  .clipboard_state$text <- enc2utf8(text)
  .clipboard_state$has_text <- TRUE
  if (.Platform$OS.type == "windows") {
    native_text <- gsub("\n", "\r\n", gsub("\r\n?", "\n", enc2utf8(text)), fixed = TRUE)
    out <- tryCatch(.Call(C_clipboard_write, native_text), error = function(e)
      list(ok = FALSE, text = "", reason = conditionMessage(e)))
    if (out$ok) return(list(ok = TRUE, text = "", reason = "Copied selection to the Windows clipboard.", backend = "Windows"))
  } else {
    helper <- .clipboard_program("write")
    if (!is.null(helper)) {
      out <- .clipboard_external(helper, text)
      if (out$ok) return(list(ok = TRUE, text = "", reason = paste0("Copied selection to the ", helper$backend,
        " clipboard."), backend = helper$backend))
    }
  }
  # OSC 52 is write-only here. Its receipt cannot be confirmed; terminals may
  # deliberately refuse clipboard requests. Never wait for clipboard queries.
  if (nchar(text, type = "bytes") <= 100000L && isTRUE(term_output_state()$supported) &&
      !Sys.getenv("TERM") %in% c("", "dumb")) {
    sent <- tryCatch({ term_write(paste0("\033]52;c;", .clipboard_base64(text), "\007")); TRUE },
      error = function(e) FALSE)
    if (sent) return(list(ok = TRUE, text = "", reason =
      "Copied within Prism; sent a clipboard request to the terminal.", backend = "OSC52 request"))
  }
  list(ok = TRUE, text = "", reason =
    "Copied within Prism. For the system clipboard, install wl-clipboard or xclip, or use terminal selection with Shift.",
    backend = "session")
}

#' Read Clipboard Text after an Explicit Paste Gesture
#' @param limit Maximum UTF-8 bytes; oversized input is rejected intact.
#' @return An ok, text, reason record. Falls back to the last Prism selection
#'   only when a system clipboard provider is unavailable.
#' @noRd
.clipboard_read <- function(limit = 100000L) {
  stopifnot("paste limit must be a positive integer" = .is_count(limit, 1L))
  limit <- as.integer(min(limit, 8388608L))
  available <- FALSE
  if (.Platform$OS.type == "windows") {
    available <- TRUE
    out <- .Call(C_clipboard_read, limit)
    if (out$ok) return(out)
  } else {
    helper <- .clipboard_program("read")
    if (!is.null(helper)) {
      available <- TRUE
      out <- .clipboard_external(helper, limit = limit)
      if (out$ok) return(out)
    }
  }
  # An inaccessible, busy or oversized system clipboard should not silently
  # substitute an older local selection when a real provider was found.
  if (available) return(out)
  if (.clipboard_state$has_text) {
    if (nchar(.clipboard_state$text, type = "bytes") > limit) return(list(ok = FALSE, text = "",
      reason = "Copied selection exceeds the paste limit; nothing was pasted."))
    return(list(ok = TRUE, text = .clipboard_state$text, reason = "Pasted the last Prism selection."))
  }
  list(ok = FALSE, text = "", reason =
    "Clipboard reading needs wl-paste, xclip, xsel or a host paste shortcut (usually Ctrl+Shift+V).")
}
