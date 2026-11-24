#' Create an Optional Mistral Explanation Link
#'
#' Creates a URL locally. Nothing is sent to a service and no browser is opened.
#' Opening the returned link shares its text with Mistral. Source code and error
#' messages can contain secrets; inspect the link or set `include_context = FALSE`.
#' Object values, the workspace, and the session history are never inspected.
#' Use [ai_link()] to choose another provider.
#'
#' @param error A condition or a character error message.
#' @param code Optional character source code.
#' @param trace Optional character vector of calls.
#' @param include_context Include source and trace.
#' @param max_chars Maximum prompt characters before URL encoding.
#' @return One HTTPS URL, at most 8000 bytes long.
#' @export
#' @examples
#' mistral_link("object 'x' not found", "mean(x)")
mistral_link <- function(error, code = NULL, trace = NULL,
                         include_context = TRUE, max_chars = 1800L) {
  out <- ai_link(error, code, trace, include_context, max_chars, provider = "mistral")
  stopifnot("link output must be a bounded HTTPS URL" = .is_text(out) &&
              startsWith(out, "https://chat.mistral.ai/chat?q=") && nchar(out, type = "bytes") <= 8000L)
  out
}

#' Capture Human-Readable Calls Without Inspecting Object Values
#' @param calls A list of calls.
#' @param skip Number of leading frames to omit.
#' @param limit Maximum number of frames.
#' @return A character vector of call labels.
#' @keywords internal
#' @noRd
.trace_calls <- function(calls, skip = 0L, limit = 12L) {
  stopifnot("calls must be a list" = is.list(calls),
            "skip must be a nonnegative integer" = .is_count(skip),
            "limit must be a nonnegative integer" = .is_count(limit))
  selected <- if (length(calls) > skip) calls[seq.int(skip + 1L, length(calls))] else list()
  labels <- vapply(selected, function(call) paste(deparse(call, width.cutoff = 100L),
                                                collapse = " "), character(1L))
  internal <- grepl("^(withCallingHandlers|tryCatch|tryCatchList|tryCatchOne|doTryCatch|withVisible|eval|[.]handleSimpleError|h)\\(", labels)
  labels <- labels[!internal]
  if (length(labels) && grepl("function \\(.*", labels[length(labels)])) labels <- head(labels, -1L)
  out <- .safe_text(tail(labels, limit))
  stopifnot("trace output must be character" = is.character(out) && !anyNA(out))
  out
}

#' Count visible cells in safe condition text
#' @param text Character text with no ANSI controls.
#' @return Nonnegative display widths.
#' @keywords internal
#' @noRd
.condition_cells <- function(text) {
  stopifnot("text must be nonmissing character" = is.character(text) && !anyNA(text))
  out <- utf8::utf8_width(text, encode = FALSE, utf8 = TRUE)
  stopifnot("condition widths must be nonnegative" = !anyNA(out) && all(out >= 0))
  out
}

#' Wrap condition text by display cells without cutting a grapheme
#' @param text Safe plain-text lines.
#' @param width Maximum display cells per line.
#' @param continuation Indentation for wrapped continuation lines.
#' @return Plain text lines bounded by width.
#' @keywords internal
#' @noRd
.condition_wrap <- function(text, width, continuation = "  ") {
  stopifnot("text must be nonmissing character" = is.character(text) && !anyNA(text),
            "width must be an integer of at least four" = is.numeric(width) &&
              length(width) == 1L && is.finite(width) && width >= 4 && width == floor(width),
            "continuation must be one short space string" = is.character(continuation) &&
              length(continuation) == 1L && !is.na(continuation) &&
              grepl("^ *$", continuation) && nchar(continuation) < width)
  logical_lines <- unlist(stringi::stri_split_fixed(text, "\n", omit_empty = FALSE), use.names = FALSE)
  out <- unlist(lapply(logical_lines, function(line) {
    if (!nzchar(line)) return("")
    chars <- stringi::stri_split_boundaries(line, type = "character")[[1L]]
    sizes <- .condition_cells(chars)
    pieces <- character()
    prefix <- ""
    while (length(chars)) {
      limit <- width - nchar(prefix)
      fits <- which(cumsum(sizes) <= limit)
      take <- if (length(fits)) max(fits) else 1L
      consumed <- take
      if (take < length(chars)) {
        spaces <- which(grepl("^ +$", chars[seq_len(take)]))
        if (length(spaces) && tail(spaces, 1L) > 1L) {
          consumed <- tail(spaces, 1L)
          take <- consumed - 1L
        }
      }
      piece <- sub(" +$", "", paste0(chars[seq_len(take)], collapse = ""))
      pieces <- c(pieces, paste0(prefix, piece))
      chars <- chars[-seq_len(consumed)]
      sizes <- sizes[-seq_len(consumed)]
      if (length(chars)) {
        leading <- match(FALSE, chars == " ", nomatch = length(chars) + 1L) - 1L
        if (leading > 0L) {
          chars <- chars[-seq_len(leading)]
          sizes <- sizes[-seq_len(leading)]
        }
      }
      prefix <- continuation
    }
    pieces
  }), use.names = FALSE)
  stopifnot("wrapped condition must be nonmissing text" = is.character(out) && !anyNA(out),
            "wrapped condition must respect display width" = all(.condition_cells(out) <= width))
  out
}

#' Format strongly labeled conditions for an inline or fullscreen transcript
#'
#' This helper only returns lines. It does not print, call an output hook, or
#' query the terminal. Each returned line has independent ANSI state.
#'
#' @param text Condition message text.
#' @param role One of error, warning, or message.
#' @param config Console configuration.
#' @param trace Plain call labels, ordered from outermost to innermost.
#' @param width Explicit pane width; otherwise config width or 80 columns.
#' @return Formatted lines with a `prism_condition` descriptor attribute. A
#'   compositor can reuse the descriptor after a resize or theme change.
#' @keywords internal
#' @noRd
.condition_lines <- function(text, role, config, trace = character(), width = NULL) {
  stopifnot("text must be nonmissing character" = is.character(text) && !anyNA(text),
            "role must identify a condition" = is.character(role) && length(role) == 1L &&
              role %in% c("error", "warning", "message"),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "trace must be nonmissing character" = is.character(trace) && !anyNA(trace),
            "width must be NULL or a finite integer of at least 12" = is.null(width) ||
              (is.numeric(width) && length(width) == 1L && is.finite(width) &&
                 width >= 12L && width == floor(width)))
  text <- paste(gsub("\r\n?", "\n", text, perl = TRUE), collapse = "\n")
  descriptor <- list(text = text, role = role, trace = trace)
  if (is.null(width)) width <- if (is.null(config$width)) 80L else config$width
  width <- as.integer(min(1000L, width))
  stopifnot("resolved condition width must be at least 12" = width >= 12L)
  depth <- config$color_depth
  if (is.null(depth)) depth <- config$theme$color_depth
  if (is.null(depth)) depth <- 16L
  paint <- function(x, style) style_text(x, style, config$theme, config$color, depth)
  # Existing SGR from other condition libraries is replaced by this theme.
  # All remaining controls are escaped before entering a terminal transcript.
  clean <- .safe_text(gsub("\033\\[[0-9;:]*m", "", text, perl = TRUE))
  clean <- sub(paste0("^", role, ":[ ]*"), "", clean, ignore.case = TRUE)
  clean <- sub("\n+$", "", clean)
  if (!length(clean) || !any(nzchar(clean))) clean <- "(empty condition message)"
  trace <- .safe_text(gsub("\033\\[[0-9;:]*m", "", trace, perl = TRUE))
  unicode <- isTRUE(config$unicode)
  glyph <- if (unicode) switch(role, error = "\u00d7", warning = "!", message = "\u2022") else
    switch(role, error = "x", warning = "!", message = "i")
  badge <- paste0(" ", glyph, " [", toupper(role), "] ")
  badge_role <- paste0(role, "_badge")
  frame_role <- paste0(role, "_frame")
  panel_role <- paste0(role, "_panel")
  if (width < 20L) {
    out <- c(paint(paste0("[", toupper(role), "]"), badge_role),
      paste0("  ", paint(.condition_wrap(clean, width - 2L), role)))
    if (length(trace)) out <- c(out, paint("Call trace", "trace"),
      paste0("  ", paint(.condition_wrap(paste0(seq_along(trace), ". ", trace), width - 2L), "trace")))
    attr(out, "prism_condition") <- descriptor
    stopifnot("compact condition must be text" = is.character(out) && !anyNA(out))
    return(out)
  }
  edges <- if (unicode) c("\u256d", "\u256e", "\u2570", "\u256f", "\u2500", "\u2502") else
    c("+", "+", "+", "+", "-", "|")
  inner <- width - 4L
  body <- .condition_wrap(clean, inner)
  roles <- rep(role, length(body))
  if (length(trace)) {
    body <- c(body, "", "Call trace (outer calls first)")
    roles <- c(roles, panel_role, "heading")
    traces <- lapply(seq_along(trace), function(i)
      .condition_wrap(paste0(i, ". ", trace[[i]]), inner, continuation = "   "))
    body <- c(body, unlist(traces, use.names = FALSE))
    roles <- c(roles, rep("trace", sum(lengths(traces))))
  }
  # Headings also wrap when a narrow pane is used.
  expanded <- lapply(seq_along(body), function(i) {
    lines <- .condition_wrap(body[[i]], inner)
    list(lines = lines, roles = rep(roles[[i]], length(lines)))
  })
  body <- unlist(lapply(expanded, `[[`, "lines"), use.names = FALSE)
  roles <- unlist(lapply(expanded, `[[`, "roles"), use.names = FALSE)
  filler <- width - 5L - .condition_cells(badge)
  top <- paste0(paint(paste0(edges[[1L]], edges[[5L]], " "), frame_role),
    paint(badge, badge_role), paint(paste0(" ", strrep(edges[[5L]], filler), edges[[2L]]), frame_role))
  middle <- vapply(seq_along(body), function(i) {
    padded <- paste0(paint(body[[i]], roles[[i]]), strrep(" ", inner - .condition_cells(body[[i]])))
    paint(paste0(paint(paste0(edges[[6L]], " "), frame_role), padded,
                 paint(paste0(" ", edges[[6L]]), frame_role)), panel_role)
  }, character(1), USE.NAMES = FALSE)
  bottom <- paint(paste0(edges[[3L]], strrep(edges[[5L]], width - 2L), edges[[4L]]), frame_role)
  out <- c(top, middle, bottom)
  attr(out, "prism_condition") <- descriptor
  stopifnot("formatted conditions must be nonmissing text" = is.character(out) && !anyNA(out),
            "formatted conditions must contain a visible badge" =
              any(grepl(paste0("[", toupper(role), "]"), out, fixed = TRUE)))
  out
}

#' Format a condition object without output side effects
#' @param condition An R condition or message text.
#' @param role One of error, warning, or message.
#' @param config Console configuration.
#' @param trace Plain call labels.
#' @param width Explicit pane width.
#' @return Formatted condition lines.
#' @keywords internal
#' @noRd
.format_condition <- function(condition, role, config, trace = character(), width = NULL) {
  stopifnot("condition must be a condition or nonmissing character" =
              inherits(condition, "condition") || (is.character(condition) && !anyNA(condition)),
            "trace must be nonmissing character" = is.character(trace) && !anyNA(trace))
  text <- if (inherits(condition, "condition")) conditionMessage(condition) else condition
  if (inherits(condition, "condition") && !length(trace) && !is.null(conditionCall(condition)))
    trace <- paste(deparse(conditionCall(condition), width.cutoff = 100L), collapse = " ")
  out <- .condition_lines(text, role, config, trace, width)
  stopifnot("formatted condition must return text" = is.character(out) && !anyNA(out))
  out
}

#' Send trusted formatted lines to the transcript hook or console
#' @param lines Formatted lines containing only package-generated ANSI controls.
#' @param role A semantic output role.
#' @param config Console configuration, optionally with output_hook(lines, role).
#' @return The lines invisibly.
#' @keywords internal
#' @noRd
.emit_lines <- function(lines, role, config) {
  stopifnot("lines must be nonmissing character" = is.character(lines) && !anyNA(lines),
            "role must be one string" = .is_text(role),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "output_hook must be NULL or a function" = is.null(config$output_hook) ||
              is.function(config$output_hook))
  if (length(lines)) {
    if (is.function(config$output_hook)) config$output_hook(lines, role)
    else cat(paste(lines, collapse = "\n"), "\n", sep = "")
  }
  stopifnot("emitted lines must remain character" = is.character(lines))
  invisible(lines)
}

#' Render semantic output and strongly flagged condition blocks
#' @param text Plain text.
#' @param role A theme role.
#' @param config Console configuration.
#' @return The sanitized input text invisibly.
#' @keywords internal
#' @noRd
.emit <- function(text, role, config) {
  stopifnot("text must be nonmissing character" = is.character(text) && !anyNA(text),
            "role must be a character scalar" = .is_text(role),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  out <- .safe_text(text)
  width <- if (is.null(config$width)) .display_width(config) else config$width
  if (role %in% c("error", "warning", "message")) {
    lines <- .condition_lines(text, role, config, width = width)
  } else {
    plain <- unlist(stringi::stri_split_fixed(out, "\n", omit_empty = FALSE), use.names = FALSE)
    depth <- if (is.null(config$color_depth)) config$theme$color_depth else config$color_depth
    lines <- style_text(plain, role, config$theme, config$color, depth)
    attr(lines, "prism_plain") <- plain
  }
  .emit_lines(lines, role, config)
  stopifnot("emitted output must be character" = is.character(out))
  invisible(out)
}

#' Render an Error, Call Trace, and Clickable Explanation Link
#' @param result Evaluation result.
#' @param config Console configuration.
#' @return The explanation URL invisibly, or NULL.
#' @keywords internal
#' @noRd
.show_error <- function(result, config) {
  stopifnot("result must be a list" = is.list(result),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  if (is.null(result$error)) return(invisible(NULL))
  width <- if (is.null(config$width)) .display_width(config) else config$width
  trace <- if (is.null(result$trace)) character() else result$trace
  .emit_lines(.format_condition(result$error, "error", config, trace, width), "error", config)
  provider <- if (is.null(config$ai_provider)) "mistral" else config$ai_provider
  target <- .ai_target(provider, config$ai_url)
  url <- ai_link(result$error, result$code, trace, include_context = config$link_context,
    provider = provider, url = config$ai_url)
  label <- paste0(if (target$prefill) "Explain with " else "Open ", target$label)
  if (isTRUE(config$hyperlinks)) {
    label <- paste0(label, if (isTRUE(config$unicode)) " \u2197" else "")
    plain_label <- label
    depth <- if (is.null(config$color_depth)) config$theme$color_depth else config$color_depth
    label <- style_text(label, "link", config$theme, config$color, depth)
    line <- paste0("  \033]8;;", url, "\033\\", label, "\033]8;;\033\\")
    attr(line, "prism_link") <- list(url = url, label = paste0("  ", plain_label))
    .emit_lines(line, "link", config)
  } else {
    notice <- if (target$prefill) " (opening shares the link text):" else ":"
    .emit(c(paste0(label, notice), url), "link", config)
  }
  if (!target$prefill) .emit("  :explain copy  Copy the prompt, then paste it into the provider's page.",
    "status", config)
  stopifnot("error link must be one URL" = .is_text(url))
  invisible(url)
}
