#' Construct a Fullscreen Session Model
#' @param max_lines Maximum retained transcript lines.
#' @param title,version Header text.
#' @return A session-local environment; no terminal changes are made.
#' @keywords internal
#' @noRd
screen_state <- function(max_lines = 4000L, title = "PRISM REPL", version = "0.6.5") {
  stopifnot("max_lines must be a positive integer" = .is_count(max_lines, 1L),
            "title and version must be text" = .is_text(title) && .is_text(version))
  out <- new.env(parent = emptyenv())
  out$entries <- list()
  out$max_lines <- as.integer(max_lines)
  out$title <- gsub("[\r\n]", " ", .safe_text(title))
  out$version <- gsub("[\r\n]", " ", .safe_text(version))
  out$status <- character()
  out$sidebar <- list()
  out$mode <- "READY"
  out$scroll <- 0L
  out$active <- FALSE
  out$revision <- 0L
  out$next_id <- 0L
  out$last_editor <- NULL
  out$last_frame <- NULL
  out$last_base_frame <- NULL
  out$mouse_selection <- NULL
  out$cache <- new.env(parent = emptyenv())
  stopifnot("screen state must be inactive and empty" = is.environment(out) &&
              !out$active && length(out$entries) == 0L)
  out
}

#' Enter the Terminal's Alternate Screen
#' @param screen A session model.
#' @param config Console configuration.
#' @return The screen invisibly.
#' @keywords internal
#' @noRd
screen_enter <- function(screen, config) {
  stopifnot("screen must be an environment" = is.environment(screen),
            "config must be a list" = is.list(config))
  if (!isTRUE(screen$active)) {
    term_output_screen(TRUE)
    term_write("\033[?1049h\033[?25l\033[0m\033[2J\033[H")
    screen$active <- TRUE
    screen$last_frame <- NULL
  }
  stopifnot("alternate screen must be active" = isTRUE(screen$active))
  invisible(screen)
}

#' Leave the Alternate Screen and Restore the Cursor
#' @param screen A session model.
#' @return NULL invisibly.
#' @keywords internal
#' @noRd
screen_leave <- function(screen) {
  stopifnot("screen must be an environment" = is.environment(screen))
  if (isTRUE(screen$active)) {
    term_write("\033[0m\033[?25h\033[?1049l")
    term_output_screen(FALSE)
    screen$active <- FALSE
    screen$last_frame <- NULL
  }
  stopifnot("alternate screen must be inactive" = !isTRUE(screen$active))
  invisible(NULL)
}

#' Append a Bounded Transcript Entry
#' @param screen A session model.
#' @param lines Output text, possibly with embedded newlines.
#' @param role Semantic output role.
#' @param ansi Whether text already contains permitted SGR or web hyperlinks.
#' @return The screen invisibly.
#' @keywords internal
#' @noRd
screen_append <- function(screen, lines, role = "output", ansi = FALSE) {
  stopifnot("screen must be an environment" = is.environment(screen),
            "lines must be nonmissing text" = is.character(lines) && !anyNA(lines),
            "role must be text" = .is_text(role),
            "ansi must be a logical scalar" = .is_flag(ansi))
  if (!length(lines)) return(invisible(screen))
  condition <- attr(lines, "prism_condition", exact = TRUE)
  source <- attr(lines, "prism_source", exact = TRUE)
  plain <- attr(lines, "prism_plain", exact = TRUE)
  link <- attr(lines, "prism_link", exact = TRUE)
  if (is.list(condition) || is.list(source)) {
    plain <- NULL
    link <- NULL
  }
  if (!is.null(link)) {
    stopifnot("semantic link must have a safe HTTP(S) target and one label" =
      is.list(link) && .is_text(link$url) && grepl("^https?://", link$url) &&
        !grepl("[[:cntrl:]]", link$url, perl = TRUE) &&
        is.character(link$label) && length(link$label) == 1L && !is.na(link$label))
    link <- list(url = enc2utf8(link$url), label = .safe_text(link$label))
    lines <- link$label
    ansi <- FALSE
  } else if (!is.null(plain)) {
    stopifnot("semantic plain output must be nonmissing text" =
      is.character(plain) && length(plain) > 0L && !anyNA(plain))
    lines <- plain
    ansi <- FALSE
  }
  safe <- if (ansi) vapply(lines, .screen_safe_ansi, character(1), USE.NAMES = FALSE) else .safe_text(lines)
  safe <- unlist(lapply(safe, .screen_split_lines), use.names = FALSE)
  if (length(safe) > screen$max_lines) {
    condition <- NULL
    source <- NULL
    link <- NULL
  }
  safe <- utils::tail(safe, screen$max_lines)
  screen$next_id <- screen$next_id + 1L
  screen$entries <- c(screen$entries, list(list(lines = safe, role = role, ansi = ansi,
                                               condition = condition, source = source, link = link, id = screen$next_id)))
  counts <- vapply(screen$entries, function(entry) length(entry$lines), integer(1))
  keep <- rev(cumsum(rev(counts))) <= screen$max_lines
  if (any(!keep)) {
    first <- which(keep)[1L]
    available <- screen$max_lines - sum(counts[keep])
    if (!is.na(first) && first > 1L && available > 0L) {
      previous <- screen$entries[[first - 1L]]
      previous$lines <- utils::tail(previous$lines, available)
      previous$condition <- NULL
      previous$source <- NULL
      previous$link <- NULL
      screen$next_id <- screen$next_id + 1L
      previous$id <- screen$next_id
      screen$entries <- c(list(previous), screen$entries[keep])
    } else screen$entries <- screen$entries[keep]
  }
  screen$scroll <- 0L
  screen$revision <- screen$revision + 1L
  stopifnot("transcript must stay bounded" =
              sum(vapply(screen$entries, function(entry) length(entry$lines), integer(1))) <= screen$max_lines)
  invisible(screen)
}

#' Clear the Session Transcript
#' @param screen A session model.
#' @return The screen invisibly.
#' @keywords internal
#' @noRd
screen_clear <- function(screen) {
  stopifnot("screen must be an environment" = is.environment(screen))
  screen$entries <- list()
  screen$scroll <- 0L
  screen$revision <- screen$revision + 1L
  screen_invalidate(screen)
  stopifnot("transcript must be empty" = !length(screen$entries))
  invisible(screen)
}

#' Invalidate Cached Screen Content After a Theme or Modal Change
#' @param screen Session model.
#' @return The screen invisibly, with its transcript and input retained.
#' @keywords internal
#' @noRd
screen_invalidate <- function(screen) {
  stopifnot("screen must be an environment" = is.environment(screen))
  screen$last_frame <- NULL
  screen$last_base_frame <- NULL
  screen$mouse_selection <- NULL
  screen$cache <- new.env(parent = emptyenv())
  stopifnot("next frame must repaint the screen" = is.null(screen$last_frame) &&
              is.environment(screen$cache))
  invisible(screen)
}

#' Recognize Only Display Styling and Safe Web Hyperlinks
#' @return A regular expression.
#' @keywords internal
#' @noRd
.screen_ansi_pattern <- function() {
  "(?:\033\\[[0-9;:]*m|\033\\]8;;(?:https?://[^\033\007\r\n]*)?(?:\007|\033\\\\))"
}

#' Preserve Styling While Neutralizing Other Terminal Controls
#' @param text One string.
#' @return Safe text containing only approved ANSI sequences.
#' @keywords internal
#' @noRd
.screen_safe_ansi <- function(text) {
  stopifnot("text must be one string" = is.character(text) && length(text) == 1L && !is.na(text))
  hits <- gregexpr(.screen_ansi_pattern(), text, perl = TRUE)[[1L]]
  if (hits[1L] < 0L) return(.safe_text(text))
  ends <- hits + attr(hits, "match.length") - 1L
  pieces <- lapply(seq_along(hits), function(i) {
    start <- if (i == 1L) 1L else ends[[i - 1L]] + 1L
    c(.safe_text(substr(text, start, hits[[i]] - 1L)), substr(text, hits[[i]], ends[[i]]))
  })
  out <- paste0(c(unlist(pieces, use.names = FALSE),
                   .safe_text(substr(text, utils::tail(ends, 1L) + 1L, nchar(text)))), collapse = "")
  stopifnot("safe ANSI text must be one string" = length(out) == 1L && !is.na(out))
  out
}

#' Tokenize Styled Text into Zero-Width Controls and Graphemes
#' @param text Safe styled text.
#' @return A list with tokens, display widths, and control flags.
#' @keywords internal
#' @noRd
.screen_tokens <- function(text) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text))
  hits <- gregexpr(.screen_ansi_pattern(), text, perl = TRUE)[[1L]]
  if (hits[1L] < 0L) {
    tokens <- .editor_graphemes(text)
    control <- rep(FALSE, length(tokens))
  } else {
    ends <- hits + attr(hits, "match.length") - 1L
    pieces <- lapply(seq_along(hits), function(i) {
      start <- if (i == 1L) 1L else ends[[i - 1L]] + 1L
      c(.editor_graphemes(substr(text, start, hits[[i]] - 1L)), substr(text, hits[[i]], ends[[i]]))
    })
    tokens <- c(unlist(pieces, use.names = FALSE),
      .editor_graphemes(substr(text, utils::tail(ends, 1L) + 1L, nchar(text))))
    control <- startsWith(tokens, "\033")
  }
  widths <- rep(0L, length(tokens))
  printable <- !control & tokens != "\n"
  widths[printable] <- .editor_widths(tokens[printable])
  out <- list(tokens = tokens, widths = widths, control = control,
              printable = which(!control), controls = which(control))
  stopifnot("token metadata must align" = length(out$tokens) == length(out$widths) &&
              length(out$tokens) == length(out$control))
  out
}

#' Measure Styled Text Without Consulting Color Detection
#' @param text One styled line.
#' @return A nonnegative number of display cells.
#' @keywords internal
#' @noRd
.screen_width <- function(text) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text))
  plain <- gsub(.screen_ansi_pattern(), "", text, perl = TRUE)
  out <- if (grepl("^[ -~]*$", plain, perl = TRUE)) nchar(plain, type = "bytes") else {
    plain <- gsub("\t", "    ", gsub("\n", "", plain, fixed = TRUE), fixed = TRUE)
    utf8::utf8_width(plain, encode = FALSE, utf8 = TRUE)
  }
  stopifnot("display width must be nonnegative" = is.finite(out) && out >= 0L)
  out
}

#' Split Logical Lines While Carrying Terminal Styling
#' @param text One sanitized string containing approved ANSI styling.
#' @return Logical lines with active styling reopened after each newline.
#' @keywords internal
#' @noRd
.screen_split_lines <- function(text) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text))
  if (!grepl("\n", text, fixed = TRUE)) return(text)
  if (!grepl("\033", text, fixed = TRUE))
    return(strsplit(paste0(text, "\n"), "\n", fixed = TRUE)[[1L]])
  pattern <- paste0(.screen_ansi_pattern(), "|\n")
  hits <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  ends <- hits + attr(hits, "match.length") - 1L
  output <- character(stringi::stri_count_fixed(text, "\n") + 1L)
  row <- 1L
  current <- character()
  sgr <- character()
  link <- ""
  start <- 1L
  for (i in seq_along(hits)) {
    current <- c(current, substr(text, start, hits[[i]] - 1L))
    token <- substr(text, hits[[i]], ends[[i]])
    if (identical(token, "\n")) {
      output[[row]] <- paste0(c(current, if (length(sgr) || nzchar(link))
        "\033]8;;\033\\\033[0m"), collapse = "")
      row <- row + 1L
      current <- c(sgr, link)
    } else {
      current <- c(current, token)
      if (startsWith(token, "\033[")) {
        code <- substr(token, 3L, nchar(token) - 1L)
        if (!nzchar(code) || identical(code, "0") || startsWith(code, "0;")) sgr <- character()
        if (nzchar(code) && !identical(code, "0")) sgr <- c(sgr, token)
      } else link <- if (identical(token, "\033]8;;\033\\") || identical(token, "\033]8;;\007")) "" else token
    }
    start <- ends[[i]] + 1L
  }
  output[[row]] <- paste0(c(current, substr(text, start, nchar(text)),
    if (length(sgr) || nzchar(link)) "\033]8;;\033\\\033[0m"), collapse = "")
  stopifnot("logical lines must preserve line count" = length(output) ==
              stringi::stri_count_fixed(text, "\n") + 1L && !anyNA(output))
  output
}

#' Slice ANSI Text at Grapheme Boundaries Without Color Heuristics
#' @param text One safe styled line.
#' @param first,last Inclusive grapheme indices.
#' @return The styled substring.
#' @keywords internal
#' @noRd
.screen_grapheme_slice <- function(text, first, last) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text),
            "indices must be ordered integers" = .is_count(first, 1L) &&
              .is_count(last) && last >= first - 1L)
  if (last < first) return("")
  out <- .screen_token_slice(.screen_tokens(text), first, last)
  stopifnot("ANSI substring must be scalar text" = is.character(out) && length(out) == 1L)
  out
}

#' Slice Prepared Grapheme Tokens with Only the Active Leading Style
#' @param parts Token metadata from `.screen_tokens()`.
#' @param first,last Inclusive printable-grapheme positions.
#' @return One styled substring.
#' @keywords internal
#' @noRd
.screen_token_slice <- function(parts, first, last) {
  stopifnot("parts must contain aligned token metadata" = is.list(parts) &&
              length(parts$tokens) == length(parts$control),
            "bounds must be ordered" = .is_count(first, 1L) && .is_count(last) && last >= first - 1L)
  positions <- if (is.null(parts$printable)) which(!parts$control) else parts$printable
  controls <- if (is.null(parts$controls)) which(parts$control) else parts$controls
  if (last < first || first > length(positions)) return("")
  from <- positions[[first]]
  to <- positions[[min(last, length(positions))]]
  leading <- parts$tokens[controls[controls < from]]
  sgr <- leading[startsWith(leading, "\033[")]
  reset <- which(grepl("^\033\\[(?:0(?:;|m)|m)", sgr, perl = TRUE))
  if (length(reset)) sgr <- utils::tail(sgr, length(sgr) - utils::tail(reset, 1L) + 1L)
  link <- utils::tail(leading[startsWith(leading, "\033]")], 1L)
  tokens <- parts$tokens[seq.int(from, to)]
  out <- paste0(c(sgr, link, tokens), collapse = "")
  if (length(leading) || any(controls >= from & controls <= to))
    out <- paste0(out, "\033]8;;\033\\\033[0m")
  stopifnot("token substring must be scalar text" = is.character(out) && length(out) == 1L)
  out
}

#' Wrap Styled Text by Terminal Cells
#' @param text One safe styled string, optionally with newlines.
#' @param width Maximum physical line width.
#' @return Complete styled lines with inherited styles reopened at each row.
#' @keywords internal
#' @noRd
.screen_wrap <- function(text, width) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text),
            "width must be a positive integer" = .is_count(width, 1L))
  if (grepl("\n", text, fixed = TRUE)) return(unlist(lapply(
    .screen_split_lines(text), .screen_wrap, width = width), use.names = FALSE))
  if (.screen_width(text) <= width) return(if (grepl("\033", text, fixed = TRUE))
    paste0(text, "\033]8;;\033\\\033[0m") else text)
  if (grepl("^[ -~]*$", text, perl = TRUE)) {
    first <- seq.int(1L, nchar(text, type = "bytes"), by = width)
    return(substring(text, first, first + width - 1L))
  }
  parts <- .screen_tokens(text)
  if (!length(parts$tokens)) return("")
  lines <- character()
  current <- character()
  used <- 0L
  sgr <- character()
  link <- ""
  flush_line <- function() {
    line <- paste0(current, collapse = "")
    if (length(sgr) || nzchar(link)) line <- paste0(line, "\033]8;;\033\\\033[0m")
    lines <<- c(lines, line)
    current <<- c(sgr, link)
    used <<- 0L
  }
  for (i in seq_along(parts$tokens)) {
    token <- parts$tokens[[i]]
    if (parts$control[[i]]) {
      current <- c(current, token)
      if (startsWith(token, "\033[")) {
        code <- substr(token, 3L, nchar(token) - 1L)
        if (!nzchar(code) || identical(code, "0") || startsWith(code, "0;")) sgr <- character()
        if (nzchar(code) && !identical(code, "0")) sgr <- c(sgr, token)
      } else link <- if (identical(token, "\033]8;;\033\\") || identical(token, "\033]8;;\007")) "" else token
    } else if (identical(token, "\n")) {
      flush_line()
    } else {
      cells <- parts$widths[[i]]
      if (used > 0L && used + cells > width) flush_line()
      if (cells <= width) {
        current <- c(current, token)
        used <- used + cells
      } else {
        current <- c(current, "?")
        used <- used + 1L
      }
    }
  }
  flush_line()
  stopifnot("wrapped output must contain text lines" = is.character(lines) &&
              length(lines) >= 1L && !anyNA(lines))
  lines
}

#' Fit One Styled Line to an Exact Cell Width
#' @param text One styled line.
#' @param width Target width.
#' @return Padded and clipped text.
#' @keywords internal
#' @noRd
.screen_fit <- function(text, width) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text),
            "width must be a nonnegative integer" = .is_count(width))
  if (!width) return("")
  if (grepl("^[ -~]*$", text, perl = TRUE)) {
    length <- nchar(text, type = "bytes")
    return(if (length > width) substr(text, 1L, width) else
      paste0(text, strrep(" ", width - length)))
  }
  if (!grepl("\n", text, fixed = TRUE)) {
    cells <- .screen_width(text)
    if (cells <= width) return(paste0(text,
      if (grepl("\033", text, fixed = TRUE)) "\033]8;;\033\\\033[0m" else "",
      strrep(" ", width - cells)))
  }
  parts <- .screen_tokens(text)
  positions <- cumsum(parts$widths)
  newline <- which(parts$tokens == "\n")
  last <- if (length(newline)) newline[[1L]] - 1L else length(parts$tokens)
  keep <- which(seq_along(parts$tokens) <= last & positions <= width)
  result <- paste0(parts$tokens[keep], collapse = "")
  used <- sum(parts$widths[keep])
  if (any(parts$control[keep])) result <- paste0(result, "\033]8;;\033\\\033[0m")
  out <- paste0(result, strrep(" ", max(0L, width - used)))
  stopifnot("fitted line must be one string" = is.character(out) && length(out) == 1L)
  out
}

#' Apply a Screen Role with a Backward-Compatible Fallback
#' @param text Text to paint.
#' @param role Theme role.
#' @param config Console configuration.
#' @return Styled text.
#' @keywords internal
#' @noRd
.screen_paint <- function(text, role, config) {
  stopifnot("text must be character" = is.character(text),
            "role must be text" = .is_text(role),
            "config must contain a theme" = is.list(config) && is.list(config$theme))
  if (!role %in% names(config$theme$colors)) role <- switch(role,
    background = "output", panel = "output", panel_header = "heading",
    border = "muted", status = "muted", "output")
  out <- style_text(text, role, config$theme, isTRUE(config$color))
  stopifnot("painting must preserve vector length" = is.character(out) && length(out) == length(text))
  out
}

#' Compute a Responsive Terminal Layout
#' @param screen Session model or equivalent list.
#' @param config Console configuration.
#' @param width,height Actual terminal columns and rows.
#' @param editor_lines Requested editor content rows.
#' @return Named geometry, using zero-based screen rows and columns.
#' @keywords internal
#' @noRd
.screen_layout <- function(screen, config, width, height, editor_lines = 1L) {
  stopifnot("screen must be a model" = is.environment(screen) || is.list(screen),
            "config must be a list" = is.list(config),
            "terminal dimensions must be positive integers" = .is_count(width, 1L) && .is_count(height, 1L),
            "editor_lines must be positive" = .is_count(editor_lines, 1L))
  usable <- max(1L, as.integer(width) - 1L)
  compact <- height < 8L || usable < 24L
  sidebar_enabled <- is.null(config$sidebar) || isTRUE(config$sidebar)
  sidebar <- !compact && sidebar_enabled && usable >= 109L && height >= 16L && length(screen$sidebar) > 0L
  side_width <- if (sidebar) min(38L, max(28L, as.integer(usable * 0.28))) else 0L
  main <- usable - if (sidebar) side_width + 1L else 0L
  cap <- if (is.null(config$editor_height)) 8L else as.integer(config$editor_height)
  if (compact) {
    header <- as.integer(height >= 3L)
    footer <- as.integer(height >= 2L)
    editor_height <- max(1L, height - header - footer)
    out <- list(width = as.integer(width), height = as.integer(height), usable = usable,
      compact = TRUE, sidebar = FALSE, sidebar_width = 0L, main_width = usable,
      content_width = max(1L, usable - 2L), header_row = if (header) 0L else -1L,
      transcript_title = -1L, transcript_start = -1L, transcript_height = 0L,
      editor_title = -1L, editor_start = header, editor_height = editor_height,
      status_row = -1L, footer_row = if (footer) height - 1L else -1L)
  } else {
    editor_height <- min(max(1L, cap), max(1L, height - 7L), max(3L, editor_lines))
    transcript_height <- max(1L, height - editor_height - 5L)
    editor_title <- 2L + transcript_height
    out <- list(width = as.integer(width), height = as.integer(height), usable = usable,
      compact = FALSE, sidebar = sidebar, sidebar_width = side_width, main_width = main,
      content_width = max(1L, main - 2L), header_row = 0L,
      transcript_title = 1L, transcript_start = 2L, transcript_height = transcript_height,
      editor_title = editor_title, editor_start = editor_title + 1L, editor_height = editor_height,
      status_row = height - 2L, footer_row = height - 1L)
  }
  stopifnot("editor must fit inside the terminal" = out$editor_start >= 0L &&
              out$editor_start + out$editor_height <= height && out$content_width >= 1L)
  out
}

#' Obtain the Current Transcript Interior Width
#' @param screen Session model.
#' @param config Configuration.
#' @param size Named terminal dimensions.
#' @return A positive integer.
#' @keywords internal
#' @noRd
screen_content_width <- function(screen, config, size = term_size()) {
  stopifnot("terminal size must contain width and height" = is.numeric(size) &&
              all(c("width", "height") %in% names(size)))
  out <- .screen_layout(screen, config, max(1L, size[["width"]]), max(1L, size[["height"]]))$content_width
  stopifnot("content width must be positive" = .is_count(out, 1L))
  out
}

#' Format a Titled Horizontal Panel Rule
#' @param title Title text.
#' @param width Rule width.
#' @param config Configuration.
#' @return One styled rule.
#' @keywords internal
#' @noRd
.screen_rule <- function(title, width, config) {
  stopifnot("title must be text" = .is_text(title), "width must be positive" = .is_count(width, 1L))
  bar <- if (isTRUE(config$unicode)) "\u2500" else "-"
  label <- paste0(" ", title, " ")
  out <- .screen_paint(.screen_fit(paste0(label, strrep(bar, width)), width), "panel_header", config)
  stopifnot("panel rule must be scalar text" = length(out) == 1L)
  out
}

#' Prepare Transcript Lines for a Particular Pane Width
#' @param screen Session model or equivalent list.
#' @param config Configuration.
#' @param width Pane interior width.
#' @return Styled physical transcript rows.
#' @keywords internal
#' @noRd
.screen_transcript <- function(screen, config, width) {
  stopifnot("screen must be a model" = is.environment(screen) || is.list(screen),
            "width must be positive" = .is_count(width, 1L))
  out <- unlist(lapply(screen$entries, .screen_entry_lines, config = config, width = width), use.names = FALSE)
  if (is.null(out)) out <- character()
  stopifnot("transcript rows must be text" = is.character(out) && !anyNA(out))
  out
}

#' Format One Transcript Entry at the Current Width and Theme
#' @param entry Retained transcript entry.
#' @param config Configuration.
#' @param width Pane interior width.
#' @return Styled physical lines.
#' @keywords internal
#' @noRd
.screen_entry_lines <- function(entry, config, width) {
  stopifnot("entry must contain lines" = is.list(entry) && is.character(entry$lines),
            "width must be positive" = .is_count(width, 1L))
  lines <- if (is.list(entry$source)) {
    count <- length(entry$source$prefixes)
    colored <- .highlight_r_window(entry$source$text, 1L, max(1L, count),
                                   theme = config$theme, color = config$color, rainbow = config$rainbow)
    paste0(style_text(entry$source$prefixes, "prompt", config$theme, config$color), colored)
  } else if (is.list(entry$condition)) {
    .condition_lines(entry$condition$text, entry$condition$role, config,
                      trace = if (is.null(entry$condition$trace)) character() else entry$condition$trace,
                      width = max(12L, width))
  } else if (is.list(entry$link)) {
    label <- .screen_paint(entry$link$label, entry$role, config)
    if (isTRUE(config$hyperlinks)) paste0("\033]8;;", entry$link$url, "\033\\", label, "\033]8;;\033\\") else
      .screen_paint(paste0(entry$link$label, " ", entry$link$url), entry$role, config)
  } else if (isTRUE(entry$ansi)) entry$lines else .screen_paint(entry$lines, entry$role, config)
  out <- unlist(lapply(lines, .screen_wrap, width = width), use.names = FALSE)
  stopifnot("entry output must contain text" = is.character(out) && !anyNA(out))
  out
}

#' Reuse a Screen Component Until Its Inputs Change
#' @param cache Optional local cache environment.
#' @param name Component identifier.
#' @param key Values determining the component.
#' @param value Lazily evaluated component value.
#' @return The computed or cached component.
#' @keywords internal
#' @noRd
.screen_cached <- function(cache, name, key, value) {
  stopifnot("cache must be NULL or an environment" = is.null(cache) || is.environment(cache),
            "component name must be text" = .is_text(name))
  item <- if (is.environment(cache)) cache[[name]] else NULL
  if (!is.null(item) && identical(item$key, key)) return(item$value)
  out <- force(value)
  if (is.environment(cache)) cache[[name]] <- list(key = key, value = out)
  stopifnot("screen component must have a value" = !is.null(out))
  out
}

#' Format a Small Logical-Line Block of a Transcript Entry
#' @param entry Retained transcript entry.
#' @param first,last Inclusive retained line positions.
#' @param config Configuration.
#' @param width Pane width in cells.
#' @return Styled physical rows for the requested block.
#' @keywords internal
#' @noRd
.screen_entry_block <- function(entry, first, last, config, width) {
  stopifnot("entry must contain lines" = is.list(entry) && is.character(entry$lines),
            "line bounds must be valid" = .is_count(first, 1L) &&
              .is_count(last, first) && last <= length(entry$lines))
  if (is.list(entry$condition) || is.list(entry$link)) return(.screen_entry_lines(entry, config, width))
  selected <- seq.int(first, last)
  lines <- if (is.list(entry$source)) {
    paste0(style_text(entry$source$prefixes[selected], "prompt", config$theme, config$color),
      .highlight_r_window(entry$source$text, first, last, config$theme, config$color,
                           rainbow = config$rainbow))
  } else if (isTRUE(entry$ansi)) entry$lines[selected] else
    .screen_paint(entry$lines[selected], entry$role, config)
  out <- unlist(lapply(lines, .screen_wrap, width = width), use.names = FALSE)
  stopifnot("formatted block must contain physical lines" = is.character(out) &&
              length(out) >= length(selected) && !anyNA(out))
  out
}

#' Obtain Only the Requested Transcript Viewport
#' @param screen Session model with a local cache.
#' @param config Configuration.
#' @param width,height Pane dimensions.
#' @return Visible rows, the realized scroll offset and a retained-row lower bound.
#' @details Formatting proceeds backwards in small logical blocks. Typing does
#'   not traverse older entries, and scrolling formats another block only when
#'   it reaches content outside the already prepared suffix.
#' @keywords internal
#' @noRd
.screen_viewport <- function(screen, config, width, height) {
  stopifnot("screen must have a cache" = is.environment(screen) && is.environment(screen$cache),
            "pane dimensions must be valid" = .is_count(width, 1L) && .is_count(height))
  cache <- screen$cache
  style <- list(width, config$theme, config$color, config$unicode, config$rainbow)
  if (!identical(cache$transcript_style, style)) {
    cache$transcript_style <- style
    cache$blocks <- new.env(parent = emptyenv())
    cache$tail_revision <- NULL
  }
  if (!identical(cache$tail_revision, screen$revision)) {
    cache$tail_rows <- character()
    cache$tail_entry <- length(screen$entries)
    cache$tail_line <- if (length(screen$entries)) length(utils::tail(screen$entries, 1L)[[1L]]$lines) else 0L
    cache$tail_revision <- screen$revision
    # Bound block-cache lifetime as the retained transcript ages out.
    if (length(ls(cache$blocks, all.names = TRUE)) > max(64L, length(screen$entries) * 2L))
      cache$blocks <- new.env(parent = emptyenv())
  }
  requested <- max(0L, if (is.null(screen$scroll)) 0L else as.integer(screen$scroll))
  needed <- requested + height
  while (length(cache$tail_rows) < needed && cache$tail_entry > 0L) {
    entry <- screen$entries[[cache$tail_entry]]
    last <- cache$tail_line
    first <- if (is.list(entry$condition) || is.list(entry$link)) 1L else max(1L, last - 63L)
    id <- if (is.null(entry$id)) paste0("row", cache$tail_entry) else as.character(entry$id)
    key <- paste(id, first, last, sep = ":")
    rows <- cache$blocks[[key]]
    if (is.null(rows)) {
      rows <- .screen_entry_block(entry, first, last, config, width)
      cache$blocks[[key]] <- rows
    }
    cache$tail_rows <- c(rows, cache$tail_rows)
    if (first <= 1L) {
      cache$tail_entry <- cache$tail_entry - 1L
      cache$tail_line <- if (cache$tail_entry) length(screen$entries[[cache$tail_entry]]$lines) else 0L
    } else cache$tail_line <- first - 1L
  }
  count <- length(cache$tail_rows)
  offset <- min(requested, max(0L, count - height))
  bottom <- count - offset
  first <- max(1L, bottom - height + 1L)
  rows <- if (height > 0L && bottom >= first) cache$tail_rows[seq.int(first, bottom)] else character()
  out <- list(rows = rows, count = count, scroll = offset, complete = cache$tail_entry == 0L)
  stopifnot("viewport must be bounded" = length(out$rows) <= height && out$scroll >= 0L)
  out
}

#' Compose Fullscreen Rows Without Writing to a Terminal
#' @param screen Session model or equivalent list.
#' @param editor_frame Editor lines and zero-based cursor coordinates.
#' @param config Configuration.
#' @param width,height Actual terminal dimensions.
#' @param transcript Optional preformatted transcript rows.
#' @param cache Optional session-local component cache.
#' @return Exact-height rows, cursor coordinates, and layout metadata.
#' @keywords internal
#' @noRd
.screen_compose <- function(screen, editor_frame, config, width, height, transcript = NULL, cache = NULL) {
  stopifnot("editor_frame must contain text lines" = is.list(editor_frame) && is.character(editor_frame$lines),
            "config must be a list" = is.list(config))
  layout <- .screen_layout(screen, config, width, height, max(1L, length(editor_frame$lines)))
  style <- list(config$theme, config$color, config$unicode)
  blank <- .screen_cached(cache, "blank", list(layout$usable, style),
    .screen_paint(strrep(" ", layout$usable), "background", config))
  rows <- rep(blank, height)
  if (layout$header_row >= 0L) {
    rows[[layout$header_row + 1L]] <- .screen_cached(cache, "header",
      list(screen$mode, screen$title, screen$version, layout$usable, layout$compact, style), {
        mode <- if (is.null(screen$mode)) "READY" else toupper(gsub("[\r\n]", " ", .safe_text(screen$mode)))
        badge <- paste0(" ", mode, " ")
        badge_width <- min(layout$usable, .screen_width(badge))
        left <- if (layout$compact) " PRISM" else paste0(" ", screen$title, "  ", screen$version)
        badge_role <- switch(mode, ERROR = "error_badge", WARNING = "warning_badge",
          READY = "success_badge", RUNNING = "message_badge", SEARCH = "selection", "accent")
        paste0(.screen_paint(.screen_fit(left, layout$usable - badge_width), "panel_header", config),
          .screen_paint(.screen_fit(badge, badge_width), badge_role, config))
      })
  }
  main_blank <- .screen_cached(cache, "main_blank", list(layout$main_width, style),
    .screen_paint(strrep(" ", layout$main_width), "background", config))
  main <- rep(main_blank, height)
  count <- 0L
  if (!layout$compact) {
    if (is.null(transcript)) transcript <- .screen_transcript(screen, config, layout$content_width)
    if (is.list(transcript)) {
      count <- transcript$count
      scroll <- transcript$scroll
      visible <- transcript$rows
    } else {
      count <- length(transcript)
      scroll <- min(max(0L, if (is.null(screen$scroll)) 0L else screen$scroll),
                    max(0L, count - layout$transcript_height))
      bottom <- max(0L, count - scroll)
      first <- max(1L, bottom - layout$transcript_height + 1L)
      visible <- if (bottom >= first) transcript[seq.int(first, bottom)] else character()
    }
    label <- if (scroll > 0L) paste0("SESSION  ", scroll, " lines above live") else "SESSION  live"
    main[[layout$transcript_title + 1L]] <- .screen_cached(cache, "transcript_title",
      list(label, layout$main_width, style), .screen_rule(label, layout$main_width, config))
    transcript_rows <- .screen_cached(cache, "transcript_rows",
      list(visible, count == 0L, layout$content_width, layout$transcript_height, style), {
        if (!count) visible <- .screen_paint(c("Your R session starts here.",
          "Type an expression below. Use the footer shortcuts or :help."), "muted", config)
        visible <- utils::tail(visible, layout$transcript_height)
        .screen_paint(paste0(" ", vapply(visible, .screen_fit, character(1),
          width = layout$content_width), " "), "background", config)
      })
    if (length(transcript_rows)) main[layout$transcript_start + seq_along(transcript_rows)] <- transcript_rows
    editor_title <- if (identical(toupper(screen$mode), "RUNNING")) "R  running" else
      if (isTRUE(editor_frame$searching)) "R  history search" else "R  edit"
    main[[layout$editor_title + 1L]] <- .screen_cached(cache, "editor_title",
      list(editor_title, layout$main_width, style), .screen_rule(editor_title, layout$main_width, config))
  }
  main[layout$editor_start + seq_len(layout$editor_height)] <- .screen_cached(cache, "editor_rows",
    list(editor_frame$lines, layout$editor_height, layout$content_width, layout$main_width, style), {
      editor_lines <- utils::head(editor_frame$lines, layout$editor_height)
      editor_lines <- c(editor_lines, rep("", layout$editor_height - length(editor_lines)))
      fitted <- paste0(" ", vapply(editor_lines, .screen_fit, character(1), width = layout$content_width), " ")
      if (layout$main_width < 3L) fitted <- vapply(fitted, .screen_fit, character(1), width = layout$main_width)
      .screen_paint(fitted, "panel", config)
    })
  body_first <- if (layout$compact) layout$editor_start else 1L
  body_last <- layout$editor_start + layout$editor_height - 1L
  body_rows <- seq.int(body_first, body_last)
  sidebar_actions <- rep("", height)
  if (layout$sidebar) {
    sidebar <- .screen_cached(cache, "sidebar", list(screen$sidebar, layout$sidebar_width, length(body_rows), style), {
      groups <- screen$sidebar
      if (is.character(groups)) groups <- list(WORKSPACE = groups)
      if (is.null(names(groups))) names(groups) <- paste("Section", seq_along(groups))
      .sidebar_frame(groups, layout$sidebar_width, length(body_rows), config)
    })
    divider <- .screen_paint(if (isTRUE(config$unicode)) "\u2502" else "|", "border", config)
    side <- paste0(divider, .screen_paint(vapply(sidebar$lines, .screen_fit, character(1),
      width = layout$sidebar_width), "panel", config))
    rows[body_rows + 1L] <- paste0(main[body_rows + 1L], side)
    sidebar_actions[body_rows + 1L] <- sidebar$actions
  } else rows[body_rows + 1L] <- main[body_rows + 1L]
  if (layout$status_row >= 0L) {
    rows[[layout$status_row + 1L]] <- .screen_cached(cache, "status", list(screen$status, layout$usable, style), {
      status <- if (length(screen$status)) paste(.safe_text(screen$status), collapse = "  |  ") else
        "Ready  |  local R process  |  :status configures session information"
      .screen_paint(.screen_fit(paste0(" ", status), layout$usable), "status", config)
    })
  }
  if (layout$footer_row >= 0L) {
    rows[[layout$footer_row + 1L]] <- .screen_cached(cache, "footer",
      list(config$keybindings, layout$usable, style), {
        label <- function(action, fallback) {
          chords <- config$keybindings[[action]]
          if (length(chords)) chords[[1L]] else if (is.null(config$keybindings)) fallback else ""
        }
        actions <- c("submit", if (layout$usable >= 90L) "newline",
          if (layout$usable >= 35L) "complete", if (layout$usable >= 50L) "history_search",
          "function_help", "help", if (layout$usable >= 90L) "scroll_up")
        descriptions <- c(submit = "run", newline = "newline", complete = "complete",
          history_search = "history", function_help = "docs", help = "keys", scroll_up = "scroll")
        defaults <- c(submit = "Enter", newline = "Ctrl+J", complete = "Tab",
          history_search = "Ctrl+R", help = "F2", function_help = "F1", scroll_up = "PageUp")
        hints <- stats::setNames(unname(descriptions[actions]),
          vapply(actions, function(action) label(action, defaults[[action]]), character(1L)))
        .screen_paint(.screen_fit(paste0(" ", .footer_hints(hints, config,
          max(1L, layout$usable - 1L))), layout$usable), "status", config)
      })
  }
  cursor_row <- layout$editor_start + min(max(0L, editor_frame$cursor_row), layout$editor_height - 1L)
  cursor_column <- min(layout$main_width - 1L, max(0L, 1L + editor_frame$cursor_column))
  out <- list(lines = rows, cursor_row = cursor_row, cursor_column = cursor_column,
              layout = layout, transcript_count = count, sidebar_actions = sidebar_actions)
  stopifnot("screen frame must fit its terminal" = length(out$lines) == height &&
              out$cursor_row >= 0L && out$cursor_row < height &&
              out$cursor_column >= 0L && out$cursor_column < width)
  out
}

#' Assemble a Single Terminal Update for Changed Rows
#' @param frame,previous Current and optional previous frames, each containing
#'   `lines`, zero-based cursor positions, and `layout$width` and `layout$height`.
#' @param config Configuration.
#' @param force Whether every row and the background must be repainted.
#' @param show_cursor Whether the final cursor is visible.
#' @return One UTF-8 scalar, or an empty string when nothing changed.
#' @details Screen rows reserve the terminal's final column. Each row is
#'   addressed absolutely, without LF or CR output or terminal autowrapping.
#'   The returned string is passed to one native write, bypassing R output sinks.
#' @keywords internal
#' @noRd
.screen_build_update <- function(frame, previous = NULL, config, force = FALSE, show_cursor = TRUE) {
  stopifnot("frame must have rows and dimensions" = is.list(frame) && is.character(frame$lines) &&
              is.list(frame$layout) && all(c("width", "height") %in% names(frame$layout)),
            "config must have a theme" = is.list(config) && is.list(config$theme),
            "redraw flags must be logical" = .is_flag(force) && .is_flag(show_cursor))
  full <- force || is.null(previous) ||
    !identical(previous$layout[c("width", "height")], frame$layout[c("width", "height")]) ||
    !identical(previous$style_key, frame$style_key)
  changed <- if (full) seq_along(frame$lines) else which(frame$lines != previous$lines)
  cursor <- sprintf("\033[%d;%dH", frame$cursor_row + 1L, frame$cursor_column + 1L)
  if (!length(changed)) {
    same_cursor <- !is.null(previous) && identical(previous$cursor_row, frame$cursor_row) &&
      identical(previous$cursor_column, frame$cursor_column)
    same_cursor <- same_cursor &&
      (is.null(previous$cursor_visible) || identical(previous$cursor_visible, show_cursor)) &&
      identical(previous$cursor_color, frame$cursor_color)
    return(if (same_cursor) "" else paste0(cursor, if (show_cursor) "\033[?25h" else "\033[?25l"))
  }
  background <- config$theme$colors[["background"]]
  erase <- if (isTRUE(config$color) && length(background) && nzchar(background))
    paste0("\033[", background, "m") else "\033[0m"
  rows <- paste0(sprintf("\033[%d;1H", changed), frame$lines[changed], erase, "\033[K")
  out <- enc2utf8(paste0("\033[?25l", if (full) paste0(erase, "\033[2J") else "",
    paste0(rows, collapse = ""), "\033[0m", cursor, if (show_cursor) "\033[?25h" else "\033[?25l"))
  stopifnot("terminal update must be one nonmissing string" =
              is.character(out) && length(out) == 1L && !is.na(out))
  out
}

#' Paint Changed Fullscreen Rows and Restore the Editing Cursor
#' @param screen Active session model.
#' @param editor_frame Optional editor frame; the previous editor is retained.
#' @param config Configuration.
#' @param size Named terminal dimensions.
#' @return The rendered frame invisibly.
#' @keywords internal
#' @noRd
screen_render <- function(screen, editor_frame = NULL, config, size = term_size()) {
  stopifnot("screen must be an active environment" = is.environment(screen) && isTRUE(screen$active),
            "config must be a list" = is.list(config),
            "size must contain terminal dimensions" = is.numeric(size) && all(c("width", "height") %in% names(size)))
  if (is.null(editor_frame)) editor_frame <- screen$last_editor
  if (is.null(editor_frame)) editor_frame <- list(lines = "r> ", cursor_row = 0L, cursor_column = 3L)
  screen$last_editor <- editor_frame
  width <- max(1L, as.integer(size[["width"]]))
  height <- max(1L, as.integer(size[["height"]]))
  layout <- .screen_layout(screen, config, width, height, max(1L, length(editor_frame$lines)))
  previous <- screen$last_frame
  style <- list(config$theme, config$color, config$unicode)
  key <- list(layout, style, screen$revision, screen$scroll, screen$mode,
    screen$status, screen$sidebar, screen$title, screen$version, config$keybindings,
    editor_frame$lines, editor_frame$searching)
  if (!is.null(screen$last_base_frame) && !is.null(previous) && identical(screen$cache$frame_key, key)) {
    frame <- screen$last_base_frame
    frame$cursor_row <- layout$editor_start + min(max(0L, editor_frame$cursor_row), layout$editor_height - 1L)
    frame$cursor_column <- min(layout$main_width - 1L, max(0L, 1L + editor_frame$cursor_column))
  } else {
    transcript <- .screen_viewport(screen, config, layout$content_width, layout$transcript_height)
    frame <- .screen_compose(screen, editor_frame, config, width, height, transcript, cache = screen$cache)
    frame$style_key <- style
    screen$scroll <- transcript$scroll
    key[[4L]] <- screen$scroll
    screen$cache$frame_key <- key
  }
  screen$last_base_frame <- frame
  frame <- .mouse_paint(frame, screen$mouse_selection, config)
  frame <- .cursor_frame(frame, config, role = "panel")
  update <- .screen_build_update(frame, previous, config)
  if (nzchar(update)) term_write(update, synchronize = isTRUE(config$synchronize),
                                cursor = frame$cursor_color)
  screen$last_frame <- frame
  stopifnot("rendered frame must retain terminal height" = length(frame$lines) == height)
  invisible(frame)
}
