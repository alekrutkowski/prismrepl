#' Keyboard shortcuts
#'
#' The enhanced editor uses these defaults. A terminal can reserve shortcuts or
#' transmit two shortcuts as the same bytes. In particular, Ctrl+Shift+M needs
#' a terminal with extended keyboard reporting; Alt+M is the portable fallback.
#' F1 opens documentation for the function under the cursor; F2 opens this list.
#' Escape closes overlays.
#'
#' @param config Console configuration whose effective bindings should be shown.
#' @return A data frame with `key` and `action` columns.
#' @export
#' @examples
#' keymap()
keymap <- function(config = repl_config()) {
  stopifnot("config must be a list" = is.list(config))
  result <- .keybinding_table(config)[c("key", "action")]
  stopifnot("keymap must pair shortcuts with actions" =
              is.data.frame(result) && nrow(result) > 0L && !anyNA(result))
  result
}

#' Complete R names and quoted file paths
#'
#' Completion lists visible names without evaluating R code. It also completes
#' exports of already loaded namespaces, arguments of directly named functions,
#' and names of existing lists or environments. Active bindings and unevaluated
#' promises are never forced. Quoted string completion lists files on disk.
#' It does not load a package merely to complete its namespace.
#'
#' @param text One string containing the current input.
#' @param cursor Cursor position as a zero-based Unicode code-point offset.
#' @param envir Environment used to find visible names.
#' @param limit Maximum number of candidates.
#' @return A list containing `start` and `end`, the zero-based code-point offsets
#'   delimiting the replacement span, and a character vector `candidates`.
#' @export
#' @examples
#' complete_r("mea")
#' complete_r("stats::rno")
complete_r <- function(text, cursor = nchar(text, type = "chars"),
                       envir = .GlobalEnv, limit = 200L) {
  stopifnot("text must be one valid string" = is.character(text) && length(text) == 1L &&
              !is.na(text) && isTRUE(utf8::utf8_valid(text)),
            "cursor must be a valid code-point offset" = is.numeric(cursor) &&
              length(cursor) == 1L && !is.na(cursor) && is.finite(cursor) &&
              cursor >= 0 && cursor <= nchar(text, type = "chars") && cursor == as.integer(cursor),
            "envir must be an environment" = is.environment(envir),
            "limit must be a positive integer" = is.numeric(limit) && length(limit) == 1L &&
              !is.na(limit) && is.finite(limit) && limit >= 1 && limit <= 10000 &&
              limit == as.integer(limit))
  before <- substr(text, 1L, cursor)
  command <- .complete_command(text, cursor, envir, limit)
  if (!is.null(command)) return(command)
  quoted <- .completion_quote(before)
  if (!is.null(quoted) && quoted$quote %in% c("'", '"')) {
    result <- .complete_path(before, quoted, cursor, limit)
  } else {
    located <- stringi::stri_locate_last_regex(before, "[\\p{L}\\p{N}\\p{M}_.]+$")
    start <- if (is.na(located[1L])) cursor else located[1L] - 1L
    token <- substr(before, start + 1L, cursor)
    prefix <- substr(before, 1L, start)
    namespace <- stringi::stri_match_last_regex(prefix,
      "([\\p{L}\\p{N}_.]+)(:{2,3})$")
    member <- stringi::stri_match_last_regex(prefix, "([\\p{L}\\p{N}_.]+)\\$$")
    pool <- if (!is.na(namespace[1L, 1L])) {
      package <- namespace[1L, 2L]
      if (!package %in% loadedNamespaces()) character() else if
        (identical(namespace[1L, 3L], ":::")) ls(getNamespace(package), all.names = TRUE) else
          getNamespaceExports(package)
    } else if (!is.na(member[1L, 1L])) {
      value <- .completion_binding(member[1L, 2L], envir)
      if (!value$found) character() else if (is.environment(value$value))
        ls(value$value, all.names = TRUE) else {
          names <- attr(value$value, "names", exact = TRUE)
          if (is.character(names)) names else character()
        }
    } else if (!is.null(quoted)) character() else {
      arguments <- .completion_arguments(prefix, envir)
      c(arguments, if (nzchar(token)) c(.completion_names(envir),
          "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf", "if", "else", "for",
          "while", "repeat", "function", "next", "break") else character())
    }
    pool <- sort(unique(pool[!is.na(pool) & startsWith(pool, token)]))
    pool <- vapply(pool, function(name) if (endsWith(name, " = ") ||
      name %in% c("TRUE", "FALSE", "NULL", "NA", "NaN", "Inf", "if", "else", "for",
                  "while", "repeat", "function", "next", "break")) name else
                    .completion_name(name), character(1), USE.NAMES = FALSE)
    result <- list(start = as.integer(start), end = as.integer(cursor),
                   candidates = utils::head(pool, limit))
  }
  stopifnot("completion must return a valid span and text candidates" =
              is.list(result) && is.character(result$candidates) &&
              !anyNA(result$candidates) && result$start >= 0L &&
              result$start <= result$end && result$end <= nchar(text, type = "chars") &&
              length(result$candidates) <= limit)
  result
}

#' Determine the unfinished quote at the cursor
#' @keywords internal
#' @noRd
.completion_quote <- function(text) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text))
  points <- strsplit(text, "", fixed = TRUE)[[1L]]
  state <- Reduce(function(state, index) {
    point <- points[[index]]
    if (state$comment) {
      if (identical(point, "\n")) state$comment <- FALSE
    } else if (state$escape) state$escape <- FALSE else if (nzchar(state$quote)) {
      if (identical(point, "\\")) state$escape <- TRUE else if
        (identical(point, state$quote)) state$quote <- ""
    } else if (point %in% c("'", '"', "`")) {
      state$quote <- point
      state$start <- index
    } else if (identical(point, "#")) state$comment <- TRUE
    state
  }, seq_along(points), init = list(quote = "", start = 0L, escape = FALSE, comment = FALSE))
  if (state$comment) state$quote <- "#"
  result <- if (nzchar(state$quote)) state else NULL
  stopifnot("quote context must be NULL or a list" = is.null(result) || is.list(result))
  result
}

#' Complete a quoted path without evaluating an expression
#' @keywords internal
#' @noRd
.complete_path <- function(before, quote, cursor, limit) {
  stopifnot("path completion needs a quote context" = is.character(before) &&
              length(before) == 1L && is.list(quote) && quote$start >= 1L)
  literal <- paste0(substr(before, quote$start, nchar(before)), quote$quote)
  parsed <- tryCatch(parse(text = literal, keep.source = FALSE), error = function(e) NULL)
  path <- if (length(parsed) == 1L && is.character(parsed[[1L]]) &&
              length(parsed[[1L]]) == 1L) parsed[[1L]] else NULL
  candidates <- character()
  if (!is.null(path) && !grepl("[\r\n]", path) && !grepl("^[[:alpha:]][[:alnum:]+.-]*://", path)) {
    if (identical(.Platform$OS.type, "windows")) path <- gsub("\\", "/", path, fixed = TRUE)
    slash <- stringi::stri_locate_last_fixed(path, "/")[1L]
    directory <- if (is.na(slash)) "." else substr(path, 1L, slash)
    stem <- if (is.na(slash)) "" else substr(path, 1L, slash)
    token <- if (is.na(slash)) path else substr(path, slash + 1L, nchar(path))
    if (dir.exists(path.expand(directory))) {
      files <- list.files(path.expand(directory), all.files = TRUE, no.. = TRUE)
      files <- utils::head(files[startsWith(files, token)], limit)
      candidates <- paste0(stem, files,
          ifelse(dir.exists(file.path(path.expand(directory), files)), "/", ""))
      candidates <- vapply(candidates, function(candidate) {
        encoded <- encodeString(candidate, quote = quote$quote)
        substr(encoded, 2L, nchar(encoded) - 1L)
      }, character(1), USE.NAMES = FALSE)
    }
  }
  result <- list(start = as.integer(quote$start), end = as.integer(cursor),
                 candidates = candidates)
  stopifnot("path completion must return candidates" = is.character(result$candidates))
  result
}

#' List names without forcing their bindings
#' @keywords internal
#' @noRd
.completion_names <- function(envir, depth = 0L) {
  stopifnot("envir must be an environment" = is.environment(envir),
            "depth must be nonnegative" = is.numeric(depth) && length(depth) == 1L && depth >= 0)
  result <- if (identical(envir, emptyenv()) || depth > 100L) character() else
    c(ls(envir, all.names = TRUE), .completion_names(parent.env(envir), depth + 1L))
  stopifnot("visible names must be character" = is.character(result))
  result
}

#' Obtain a value only from an already evaluated binding
#' @keywords internal
#' @noRd
.completion_binding <- function(name, envir, depth = 0L) {
  stopifnot("name must be scalar text" = is.character(name) && length(name) == 1L && !is.na(name),
            "envir must be an environment" = is.environment(envir))
  result <- if (identical(envir, emptyenv()) || depth > 100L) list(found = FALSE) else {
    status <- binding_info(envir, name)[[1L]]
    if (identical(status, "value")) list(found = TRUE, value = get(name, envir, inherits = FALSE)) else
      if (!identical(status, "missing")) list(found = FALSE) else
        .completion_binding(name, parent.env(envir), depth + 1L)
  }
  stopifnot("binding lookup must report whether it found a value" =
              is.list(result) && is.logical(result$found) && length(result$found) == 1L)
  result
}

#' Complete formal arguments of a directly named function
#' @keywords internal
#' @noRd
.completion_arguments <- function(prefix, envir) {
  stopifnot("prefix must be scalar text" = is.character(prefix) && length(prefix) == 1L,
            "envir must be an environment" = is.environment(envir))
  matched <- stringi::stri_match_last_regex(prefix, "([\\p{L}\\p{N}_.]+)\\([^()]*$")
  result <- character()
  if (!is.na(matched[1L, 1L])) {
    value <- .completion_binding(matched[1L, 2L], envir)
    if (value$found && identical(typeof(value$value), "closure")) {
      arguments <- names(formals(value$value))
      result <- paste0(arguments[arguments != "..."], " = ")
    }
  }
  stopifnot("formal completions must be text" = is.character(result))
  result
}

#' Quote an unusual completion name
#' @keywords internal
#' @noRd
.completion_name <- function(name) {
  stopifnot("name must be scalar text" = is.character(name) && length(name) == 1L && !is.na(name))
  result <- if (identical(make.names(name), name)) name else encodeString(name, quote = "`")
  stopifnot("quoted name must be scalar text" = is.character(result) && length(result) == 1L)
  result
}

#' Split editable text into Unicode grapheme clusters
#' @keywords internal
#' @noRd
.editor_graphemes <- function(text) {
  stopifnot("text must be one valid string" = is.character(text) && length(text) == 1L &&
              !is.na(text) && isTRUE(utf8::utf8_valid(text)))
  result <- if (!nzchar(text)) character() else
    stringi::stri_split_boundaries(enc2utf8(text), type = "character")[[1L]]
  stopifnot("graphemes must reconstruct the input" =
              is.character(result) && identical(paste0(result, collapse = ""), enc2utf8(text)))
  result
}

#' Construct an editor state
#' @keywords internal
#' @noRd
.editor_state <- function(text = "", history = character()) {
  stopifnot("text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text),
            "history must be text" = is.character(history) && !anyNA(history))
  chars <- .editor_graphemes(text)
  result <- list(chars = chars, cursor = length(chars), bytes = nchar(text, type = "bytes"),
                 line_count = sum(chars == "\n") + 1L, history_index = length(history) + 1L,
                 draft = text, kill = "", search = NULL, overlay = character(),
                 completion = NULL, action = "edit", clear = FALSE, help_request = NULL)
  stopifnot("editor state must have a valid cursor" = result$cursor <= length(result$chars))
  result
}

#' Replace a grapheme span in an editor buffer
#' @keywords internal
#' @noRd
.editor_replace <- function(state, from = state$cursor, to = state$cursor, text = "") {
  stopifnot("replacement must be inside the buffer" = is.list(state) &&
              length(from) == 1L && length(to) == 1L && from >= 0 &&
              to >= from && to <= length(state$chars),
            "replacement text must be scalar" = is.character(text) && length(text) == 1L && !is.na(text))
  before <- if (from > 0L) state$chars[seq_len(from)] else character()
  after <- if (to < length(state$chars)) state$chars[seq.int(to + 1L, length(state$chars))] else character()
  removed <- if (to > from) state$chars[seq.int(from + 1L, to)] else character()
  old_length <- length(state$chars)
  state$bytes <- (if (is.null(state$bytes)) sum(nchar(state$chars, type = "bytes")) else state$bytes) +
    nchar(text, type = "bytes") - sum(nchar(removed, type = "bytes"))
  state$line_count <- (if (is.null(state$line_count)) sum(state$chars == "\n") + 1L else state$line_count) +
    stringi::stri_count_fixed(text, "\n") - sum(removed == "\n")
  printable_ascii <- function(x) !length(x) || all(grepl("^[ -~]+$", x, perl = TRUE))
  if (!nzchar(text) && printable_ascii(utils::tail(before, 1L)) &&
      printable_ascii(utils::head(after, 1L))) {
    state$chars <- c(before, after)
    state$cursor <- length(before)
  } else {
    if (nzchar(text) && printable_ascii(text)) {
      # Printable ASCII forms a stable boundary for regional-indicator runs.
      # Re-segment the adjacent graphemes to retain combining/prepend marks.
      fixed_before <- utils::head(before, -1L)
      local_before <- utils::tail(before, 1L)
      local_after <- utils::head(after, 1L)
      fixed_after <- utils::tail(after, -1L)
    } else {
      # A newline is a hard Unicode grapheme boundary. Only the affected
      # logical lines need re-segmentation, even for flags and ZWJ sequences.
      left_break <- max(c(0L, which(before == "\n")))
      right_break <- match("\n", after, nomatch = length(after) + 1L) - 1L
      fixed_before <- utils::head(before, left_break)
      local_before <- utils::tail(before, length(before) - left_break)
      local_after <- utils::head(after, right_break)
      fixed_after <- utils::tail(after, length(after) - right_break)
    }
    head <- paste0(c(local_before, text), collapse = "")
    middle <- .editor_graphemes(paste0(c(local_before, text, local_after), collapse = ""))
    state$chars <- c(fixed_before, middle, fixed_after)
    state$cursor <- min(length(fixed_before) + length(.editor_graphemes(head)), length(state$chars))
  }
  state$completion <- NULL
  state$limit_rejected <- FALSE
  if (!is.null(state$autopairs)) state$autopairs <- .editor_pairs_update(
    state$autopairs, from, to, length(state$chars) - old_length, state$chars)
  stopifnot("edited state must retain a valid cursor" =
              state$cursor >= 0L && state$cursor <= length(state$chars))
  state
}

#' Apply the Same Input Limits to Every Insertion and Recall Path
#' @param state Editor state.
#' @param config Configuration, or NULL for unrestricted internal operations.
#' @param from,to Zero-based replacement boundaries.
#' @param text Replacement text.
#' @return Updated state, or the original text with a visible limit explanation.
#' @keywords internal
#' @noRd
.editor_replace_checked <- function(state, config, from = state$cursor, to = state$cursor, text = "") {
  stopifnot("replacement bounds must be valid" = is.list(state) && from >= 0L &&
              to >= from && to <= length(state$chars),
            "replacement must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text))
  removed <- if (to > from) state$chars[seq.int(from + 1L, to)] else character()
  bytes <- (if (is.null(state$bytes)) sum(nchar(state$chars, type = "bytes")) else state$bytes) +
    nchar(text, type = "bytes") - sum(nchar(removed, type = "bytes"))
  lines <- (if (is.null(state$line_count)) sum(state$chars == "\n") + 1L else state$line_count) +
    stringi::stri_count_fixed(text, "\n") - sum(removed == "\n")
  rejected <- !is.null(config) &&
    ((!is.null(config$max_paste) && bytes > config$max_paste) ||
     (!is.null(config$max_lines) && lines > config$max_lines))
  if (rejected) {
    state$overlay <- "Input limit reached. Increase max_paste or max_lines in repl_config()."
    state$completion <- NULL
  } else state <- .editor_replace(state, from, to, text)
  state$limit_rejected <- rejected
  stopifnot("checked replacement must report its outcome" = .is_flag(state$limit_rejected) &&
              state$cursor >= 0L && state$cursor <= length(state$chars))
  state
}

#' Locate the current logical line
#' @keywords internal
#' @noRd
.editor_line <- function(state) {
  stopifnot("state must have a valid cursor" = is.list(state) && state$cursor >= 0L &&
              state$cursor <= length(state$chars))
  breaks <- which(state$chars == "\n")
  result <- list(start = max(c(0L, breaks[breaks <= state$cursor])),
                 end = min(c(length(state$chars) + 1L, breaks[breaks > state$cursor])) - 1L,
                 row = sum(breaks <= state$cursor) + 1L, breaks = breaks)
  stopifnot("line must contain the cursor" = result$start <= state$cursor && result$end >= state$cursor)
  result
}

#' Move through history while retaining the unfinished draft
#' @keywords internal
#' @noRd
.editor_history <- function(state, direction, history, config = NULL) {
  stopifnot("direction must be one step" = direction %in% c(-1L, 1L),
            "history must be text" = is.character(history) && !anyNA(history))
  if (state$history_index == length(history) + 1L)
    state$draft <- paste0(state$chars, collapse = "")
  index <- max(1L, min(length(history) + 1L, state$history_index + direction))
  text <- if (index > length(history)) state$draft else history[[index]]
  state <- .editor_replace_checked(state, config, 0L, length(state$chars), text)
  if (isTRUE(state$limit_rejected)) return(state)
  state$history_index <- index
  stopifnot("history position must be valid" =
              state$history_index >= 1L && state$history_index <= length(history) + 1L)
  state
}

#' Find a word boundary
#' @keywords internal
#' @noRd
.editor_word <- function(chars, cursor, direction) {
  stopifnot("cursor must be inside text" = is.character(chars) &&
              cursor >= 0L && cursor <= length(chars),
            "direction must be one step" = direction %in% c(-1L, 1L))
  if (direction < 0L) {
    prefix <- if (cursor > 0L) chars[seq_len(cursor)] else character()
    matched <- stringi::stri_locate_last_regex(paste0(prefix, collapse = ""),
      "(?:[\\p{L}\\p{N}\\p{M}_.]+|[^\\p{L}\\p{N}\\p{M}_.\\s]+)\\s*$")
    result <- if (is.na(matched[1L])) 0L else length(.editor_graphemes(
      substr(paste0(prefix, collapse = ""), 1L, matched[1L] - 1L)))
  } else {
    suffix <- if (cursor < length(chars)) chars[seq.int(cursor + 1L, length(chars))] else character()
    matched <- stringi::stri_locate_first_regex(paste0(suffix, collapse = ""),
      "^\\s*(?:[\\p{L}\\p{N}\\p{M}_.]+|[^\\p{L}\\p{N}\\p{M}_.\\s]+)")
    result <- if (is.na(matched[2L])) length(chars) else cursor + length(.editor_graphemes(
      substr(paste0(suffix, collapse = ""), 1L, matched[2L])))
  }
  stopifnot("word boundary must be inside text" = result >= 0L && result <= length(chars))
  result
}

#' Apply a reverse-history search event
#' @keywords internal
#' @noRd
.editor_search <- function(state, event, history, config = list()) {
  stopifnot("search must be active" = is.list(state$search),
            "history must be text" = is.character(history))
  action <- .key_action(event, config)
  if (identical(action, "escape")) {
    original <- state$search$original
    state <- .editor_replace(state, 0L, length(state$chars), original$text)
    state$cursor <- original$cursor
    state$search <- NULL
    state$overlay <- character()
    return(state)
  }
  if (identical(action, "submit")) {
    if (!isTRUE(state$search$found)) {
      state$overlay <- paste0("No history match. Change the query or press ",
        .editor_key_label(config, "escape", "Escape"), " to return.")
      return(state)
    }
    state$search <- NULL
    state$overlay <- character()
    return(state)
  }
  if (identical(event$type, "text") || identical(event$type, "paste")) {
    state$search$query <- paste0(state$search$query, gsub("[\r\n]", " ", event$text))
    state$search$query <- paste0(utils::head(.editor_graphemes(state$search$query), 256L), collapse = "")
    state$search$index <- 1L
  } else if (identical(action, "backspace")) {
    query <- .editor_graphemes(state$search$query)
    state$search$query <- paste0(utils::head(query, -1L), collapse = "")
    state$search$index <- 1L
  } else if (identical(action, "history_search")) state$search$index <- state$search$index + 1L
  matches <- rev(history[grepl(state$search$query, history, fixed = TRUE)])
  state$search$found <- length(matches) > 0L
  state$search$index <- if (length(matches)) ((state$search$index - 1L) %% length(matches)) + 1L else 1L
  if (length(matches)) {
    search <- state$search
    state <- .editor_replace_checked(state, config, 0L, length(state$chars), matches[[search$index]])
    state$search <- search
    if (isTRUE(state$limit_rejected)) {
      state$search$found <- FALSE
      return(state)
    }
  }
  state$overlay <- paste0("History search: ", state$search$query,
      " [", if (length(matches)) state$search$index else 0L, "/", length(matches),
      "]  ", .editor_key_label(config, "submit", "Enter"), " accepts; ",
      .editor_key_label(config, "escape", "Escape"), " cancels")
  stopifnot("search state must remain valid" = is.list(state) && is.list(state$search))
  state
}

#' Insert or cycle a completion
#' @keywords internal
#' @noRd
.editor_complete <- function(state, envir, backwards = FALSE, config = NULL) {
  stopifnot("state must be an editor state" = is.list(state),
            "envir must be an environment" = is.environment(envir),
            "backwards must be logical" = is.logical(backwards) && length(backwards) == 1L)
  state$limit_rejected <- FALSE
  if (is.null(state$completion)) {
    text <- paste0(state$chars, collapse = "")
    cursor <- nchar(paste0(utils::head(state$chars, state$cursor), collapse = ""), type = "chars")
    matched <- complete_r(text, cursor, envir)
    if (!length(matched$candidates)) {
      state$overlay <- if (!is.null(matched$status) && nzchar(matched$status)) matched$status else
        "No completions. R names, commands, package names and file paths are supported."
      return(state)
    }
    from <- length(.editor_graphemes(substr(text, 1L, matched$start)))
    to <- state$cursor
    candidates <- matched$candidates
    common <- .completion_common(candidates)
    original <- substr(text, matched$start + 1L, matched$end)
    if (length(candidates) == 1L) {
      state <- .editor_replace_checked(state, config, from, to, candidates[[1L]])
      if (isTRUE(state$limit_rejected)) return(state)
      state$overlay <- character()
      return(state)
    }
    if (nchar(common) > nchar(original)) state <- .editor_replace_checked(state, config, from, to, common)
    if (isTRUE(state$limit_rejected)) return(state)
    state$completion <- list(from = from, to = state$cursor, candidates = candidates, index = 0L,
      status = matched$status)
  } else {
    completion <- state$completion
    index <- if (backwards) completion$index - 1L else completion$index + 1L
    index <- ((index - 1L) %% length(completion$candidates)) + 1L
    state <- .editor_replace_checked(state, config, completion$from, completion$to, completion$candidates[[index]])
    if (isTRUE(state$limit_rejected)) return(state)
    completion$index <- index
    completion$to <- state$cursor
    state$completion <- completion
  }
  candidates <- state$completion$candidates
  state$overlay <- c(paste0(length(candidates), " completions. ",
      .editor_key_label(config, "complete", "Tab"), " cycles; ",
      .editor_key_label(config, "complete_previous", "Shift+Tab"), " reverses."),
                     paste(utils::head(candidates, 12L), collapse = "   "),
                     if (!is.null(state$completion$status) && nzchar(state$completion$status)) state$completion$status)
  stopifnot("completion state must contain choices" = length(state$completion$candidates) > 0L)
  state
}

#' Describe the Effective Shortcut for an Editor Hint
#' @param config Configuration, optionally containing effective bindings.
#' @param action Action identifier.
#' @param fallback Default label when no binding table was supplied.
#' @return One human-readable shortcut label.
#' @keywords internal
#' @noRd
.editor_key_label <- function(config, action, fallback) {
  stopifnot("action and fallback must be text" = .is_text(action) && .is_text(fallback))
  chords <- config$keybindings[[action]]
  out <- if (length(chords)) chords[[1L]] else if (is.null(config$keybindings)) fallback else "(unbound)"
  stopifnot("shortcut label must be text" = .is_text(out))
  out
}

#' Find a common Unicode prefix
#' @keywords internal
#' @noRd
.completion_common <- function(candidates) {
  stopifnot("candidates must be nonempty text" = is.character(candidates) && length(candidates) > 0L)
  result <- Reduce(function(prefix, item) {
    left <- .editor_graphemes(prefix)
    right <- .editor_graphemes(item)
    count <- min(length(left), length(right))
    if (!count) return("")
    different <- which(left[seq_len(count)] != right[seq_len(count)])
    paste0(utils::head(left, if (length(different)) different[[1L]] - 1L else count), collapse = "")
  }, candidates)
  stopifnot("common prefix must be scalar text" = is.character(result) && length(result) == 1L)
  result
}

#' Apply one editor event
#' @keywords internal
#' @noRd
.editor_step <- function(state, event, history, envir, config) {
  stopifnot("editor event must be a list" = is.list(state) && is.list(event),
            "history must be text" = is.character(history),
            "envir must be an environment" = is.environment(envir),
            "config must be a list" = is.list(config))
  action <- .key_action(event, config)
  state$help_request <- NULL
  if (identical(event$type, "eof")) state$action <- "eof" else if
    (identical(action, "cancel")) state$action <- "cancel" else if (!is.null(state$search))
      return(.editor_search(state, event, history, config)) else if
    (!nzchar(action) && event$type %in% c("text", "paste")) {
      text <- gsub("\r\n?", "\n", event$text)
      if (stringi::stri_detect_regex(text, "[\\p{Cc}&&[^\\n\\t]]")) {
        state$overlay <- "Input contains terminal control characters and was rejected."
      } else {
        state <- if (identical(event$type, "text")) .editor_type(state, text, config) else
          .editor_replace_checked(state, config, text = text)
        if (!isTRUE(state$limit_rejected)) state$overlay <- if (isTRUE(event$truncated))
          "Paste was truncated by the terminal input limit. Review it before Enter." else character()
      }
    } else if (action %in% c("complete", "complete_previous")) {
      return(.editor_complete(state, envir, identical(action, "complete_previous"), config))
    } else if (nzchar(action)) {
      state$completion <- NULL
      if (!action %in% c("help", "escape")) state$overlay <- character()
      line <- .editor_line(state)
      if (identical(action, "pipe")) {
        state <- .editor_replace_checked(state, config, text = paste0(" ", config$pipe, " "))
      } else if (action %in% c("submit", "newline")) {
        text <- paste0(state$chars, collapse = "")
        if (identical(action, "submit") && (startsWith(trimws(text), ":") ||
                              !identical(parse_status(text), "incomplete"))) {
          state$action <- "submit"
        } else {
          current <- paste0(if (line$start < state$cursor)
            state$chars[seq.int(line$start + 1L, state$cursor)] else character(), collapse = "")
          indent <- stringi::stri_extract_first_regex(current, "^ *")
          if (grepl("[({\\[]\\s*$", current, perl = TRUE)) indent <- paste0(indent, "  ")
          state <- .editor_replace_checked(state, config, text = paste0("\n", indent))
        }
      } else if (identical(action, "backspace") && state$cursor > 0L) {
        state <- .editor_pair_backspace(state, config)
      } else if (action %in% c("delete", "delete_or_exit")) {
        if (identical(action, "delete_or_exit") && !length(state$chars)) state$action <- "eof" else
          if (state$cursor < length(state$chars))
            state <- .editor_replace(state, state$cursor, state$cursor + 1L)
      } else if (action %in% c("left", "word_left")) {
        state$cursor <- if (identical(action, "word_left")) .editor_word(state$chars, state$cursor, -1L) else
          max(0L, state$cursor - 1L)
      } else if (action %in% c("right", "word_right")) {
        state$cursor <- if (identical(action, "word_right")) .editor_word(state$chars, state$cursor, 1L) else
          min(length(state$chars), state$cursor + 1L)
      } else if (identical(action, "line_start")) state$cursor <- line$start else
        if (identical(action, "line_end")) state$cursor <- line$end else
        if (identical(action, "buffer_start")) state$cursor <- 0L else
        if (identical(action, "buffer_end")) state$cursor <- length(state$chars) else
        if (action %in% c("history_previous", "history_next")) {
          state <- .editor_history(state, if (identical(action, "history_previous")) -1L else 1L, history, config)
        } else if (action %in% c("up", "down")) {
          direction <- if (identical(action, "up")) -1L else 1L
          if ((direction < 0L && line$row == 1L) ||
              (direction > 0L && line$row > length(line$breaks))) {
            state <- .editor_history(state, direction, history, config)
          } else {
            starts <- c(0L, line$breaks)
            ends <- c(line$breaks - 1L, length(state$chars))
            row <- line$row + direction
            state$cursor <- min(ends[[row]], starts[[row]] + state$cursor - line$start)
          }
        } else if (action %in% c("kill_start", "kill_end", "kill_word")) {
          from <- if (identical(action, "kill_end")) state$cursor else if (identical(action, "kill_word"))
            .editor_word(state$chars, state$cursor, -1L) else line$start
          to <- if (identical(action, "kill_end")) line$end else state$cursor
          if (from == to && identical(action, "kill_end") && to < length(state$chars)) to <- to + 1L
          state$kill <- paste0(if (to > from) state$chars[seq.int(from + 1L, to)] else character(), collapse = "")
          state <- .editor_replace(state, from, to)
        } else if (identical(action, "yank")) state <- .editor_replace_checked(state, config, text = state$kill) else
        if (identical(action, "history_search")) {
          state$search <- list(query = "", index = 1L,
              original = list(text = paste0(state$chars, collapse = ""), cursor = state$cursor))
          state <- .editor_search(state, list(type = "key", key = "", modifiers = character()), history, config)
        } else if (identical(action, "function_help")) {
          state <- .editor_request_help(state)
        } else if (identical(action, "help")) {
          keys <- keymap(config)
          state$overlay <- if (length(state$overlay)) character() else
            paste(keys$key, keys$action, sep = "  :  ")
        } else if (identical(action, "escape")) state$overlay <- character() else
        if (identical(action, "redraw")) state$clear <- TRUE
    }
  stopifnot("editor action and cursor must be valid" = state$action %in% c("edit", "submit", "cancel", "eof") &&
              state$cursor >= 0L && state$cursor <= length(state$chars))
  state
}

#' Measure safe grapheme display widths
#' @keywords internal
#' @noRd
.editor_widths <- function(chars) {
  stopifnot("chars must contain text" = is.character(chars) && !anyNA(chars))
  result <- utf8::utf8_width(chars, encode = FALSE, utf8 = TRUE)
  result[chars == "\t"] <- 4L
  result[is.na(result)] <- nchar(encodeString(chars[is.na(result)]))
  stopifnot("widths must be nonnegative" = is.numeric(result) && !anyNA(result) && all(result >= 0))
  result
}

#' Create a bounded single-line viewport
#' @keywords internal
#' @noRd
.editor_slice <- function(chars, width, cursor = NULL) {
  stopifnot("chars must contain text" = is.character(chars),
            "width must be positive" = is.numeric(width) && length(width) == 1L && width >= 1)
  widths <- .editor_widths(chars)
  positions <- c(0L, cumsum(widths))
  location <- if (is.null(cursor)) 0L else positions[[cursor + 1L]]
  left <- max(0L, location - width + 2L)
  first <- max(1L, which(positions <= left))
  if (first > length(chars)) first <- length(chars) + 1L
  offset <- positions[[first]]
  available <- max(1L, width - as.integer(first > 1L))
  keep <- which(seq_along(chars) >= first & positions[-1L] - offset <= available)
  last <- if (length(keep)) max(keep) else first - 1L
  suffix <- last < length(chars)
  if (suffix && length(keep) && sum(widths[keep]) >= available) keep <- utils::head(keep, -1L)
  last <- if (length(keep)) max(keep) else first - 1L
  text <- paste0(if (first > 1L) "<" else "", paste0(chars[keep], collapse = ""),
                 if (suffix) ">" else "")
  text <- gsub("\t", "    ", text, fixed = TRUE)
  result <- list(text = text, column = location - offset + as.integer(first > 1L),
                 truncated = first > 1L || suffix, first = first, last = last,
                 left = first > 1L, right = suffix)
  stopifnot("viewport text must be scalar" = is.character(result$text) && length(result$text) == 1L)
  result
}

#' Compute a display frame without writing terminal control codes
#' @keywords internal
#' @noRd
.editor_frame <- function(state, width, height, prompt, continuation, status, config) {
  stopifnot("terminal dimensions must be positive" = width >= 1L && height >= 1L,
            "prompts and status must be text" = is.character(prompt) && is.character(continuation) && is.character(status))
  line <- .editor_line(state)
  starts <- c(0L, line$breaks)
  ends <- c(line$breaks - 1L, length(state$chars))
  extras <- c(status, state$overlay)
  max_extra <- min(length(extras), max(0L, height %/% 2L))
  footer_rows <- as.integer(height >= 4L)
  budget <- max(1L, height - max_extra - 2L - footer_rows)
  first <- max(1L, line$row - budget + 1L)
  last <- min(length(starts), first + budget - 1L)
  rows <- seq.int(first, last)
  source <- paste0(state$chars, collapse = "")
  focus <- .editor_pair_focus(state, source, config)
  cache <- config$highlight_cache
  cache_key <- list(source, first, last, config$theme, config$color, config$rainbow)
  if (is.environment(cache) && identical(cache$key, cache_key)) {
    colored <- cache$value
  } else {
    colored <- .highlight_r_window(source, first, last, theme = config$theme,
      color = config$color, rainbow = !identical(config$rainbow, FALSE))
    if (is.environment(cache)) {
      cache$key <- cache_key
      cache$value <- colored
    }
  }
  rendered <- lapply(rows, function(row) {
    prefix <- if (row == 1L) prompt else continuation
    if (row == first && first > 1L) prefix <- "^ "
    prefix <- gsub("[\r\n]", " ", .safe_text(prefix))
    prefix <- .screen_fit(prefix, min(.screen_width(prefix), max(0L, width - 2L)))
    prefix_width <- .screen_width(prefix)
    text <- if (ends[[row]] > starts[[row]])
      state$chars[seq.int(starts[[row]] + 1L, ends[[row]])] else character()
    cursor <- if (row == line$row) state$cursor - starts[[row]] else NULL
    slice <- .editor_slice(text, max(1L, width - prefix_width - 1L), cursor)
    safe_before <- .safe_text(paste0(utils::head(text, slice$first - 1L), collapse = ""))
    safe_through <- .safe_text(paste0(utils::head(text, slice$last), collapse = ""))
    code <- if (slice$last < slice$first) "" else .screen_grapheme_slice(colored[[row - first + 1L]],
      length(.editor_graphemes(safe_before)) + 1L, length(.editor_graphemes(safe_through)))
    code <- paste0(if (slice$left) "<" else "", code, if (slice$right) ">" else "")
    role <- if (row == 1L) "prompt" else "continuation"
    painted <- if (isTRUE(config$color) && nzchar(config$theme$colors[[role]]))
      paste0("\033[", config$theme$colors[[role]], "m", prefix, "\033[0m") else prefix
    visible_first <- starts[[row]] + slice$first
    visible_last <- starts[[row]] + slice$last
    painted <- paste0(painted, code)
    if (nrow(focus) && visible_last >= visible_first &&
        any(focus[, 1L] <= visible_last & focus[, 2L] >= visible_first))
      painted <- .editor_pair_row(painted, focus, visible_first, visible_last,
        length(.editor_graphemes(prefix)) + as.integer(slice$left), config,
        chars = state$chars[seq.int(visible_first, visible_last)])
    list(text = painted, column = prefix_width + slice$column)
  })
  extra <- if (max_extra) utils::head(extras, max_extra) else character()
  if (length(extras) > max_extra && max_extra > 0L)
    extra[[max_extra]] <- paste0("More shortcuts: keymap() or :keys. ",
      .editor_key_label(config, "escape", "Escape"), " closes this panel.")
  extra <- vapply(extra, function(text) .editor_slice(.editor_graphemes(
                    gsub("[\r\n]", " ", .safe_text(text))), max(1L, width - 1L))$text,
                  character(1), USE.NAMES = FALSE)
  if (last < length(starts) && length(extra)) extra[[1L]] <- .editor_slice(
    .editor_graphemes(paste0("v More input below. ", extra[[1L]])), max(1L, width - 1L))$text
  footer <- if (footer_rows) .footer_hints(stats::setNames(
    c("run", "newline", "complete", "function help", "shortcuts", "commands"),
    c(.editor_key_label(config, "submit", "Enter"), .editor_key_label(config, "newline", "Ctrl+J"),
      .editor_key_label(config, "complete", "Tab"), .editor_key_label(config, "function_help", "F1"),
      .editor_key_label(config, "help", "F2"), ":help")),
    config, max(1L, width - 1L)) else character()
  result <- list(lines = c(vapply(rendered, `[[`, character(1), "text"), extra, footer),
                 cursor_row = line$row - first,
                 cursor_column = rendered[[line$row - first + 1L]]$column,
                 searching = !is.null(state$search), logical_lines = length(starts))
  stopifnot("frame must have a visible cursor" = length(result$lines) > result$cursor_row &&
              result$cursor_row >= 0L && result$cursor_column >= 0L)
  result
}

#' Lay Out an Editor Buffer as Grapheme-Aligned Visual Rows
#' @param state Editor state.
#' @param width Editor width in terminal cells.
#' @param prompt,continuation Prompt strings.
#' @param config Configuration with an optional local highlight cache.
#' @return Logical and physical row metadata, including hit-test boundaries.
#' @keywords internal
#' @noRd
.editor_wrap_geometry <- function(state, width, prompt, continuation, config) {
  stopifnot("state must contain graphemes" = is.list(state) && is.character(state$chars),
            "width must be positive" = .is_count(width, 1L))
  .screen_cached(config$highlight_cache, "editor_geometry",
    list(state$chars, width, prompt, continuation, config$unicode), {
      chars <- state$chars
      source <- paste0(chars, collapse = "")
      breaks <- which(chars == "\n")
      starts <- c(0L, breaks)
      ends <- c(breaks - 1L, length(chars))
      ascii <- grepl("^[ -~\n\t]*$", source, perl = TRUE)
      widths <- if (ascii) rep.int(1L, length(chars)) else .editor_widths(chars)
      widths[chars == "\t"] <- 4L
      widths[chars == "\n"] <- 0L
      safe_lengths <- rep.int(1L, length(chars))
      safe_lengths[chars == "\t"] <- 4L
      pattern <- "[\\x{0001}-\\x{0008}\\x{000B}-\\x{001F}\\x{007F}-\\x{009F}]"
      unsafe <- if (!ascii && grepl(pattern, source, perl = TRUE))
        grepl(pattern, chars, perl = TRUE) else rep(FALSE, length(chars))
      if (any(unsafe)) {
        safe <- .safe_text(chars[unsafe])
        safe_lengths[unsafe] <- nchar(safe)
        widths[unsafe] <- nchar(safe)
      }
      safe_positions <- c(0L, cumsum(safe_lengths))
      exceptional <- unique(findInterval(which(chars == "\t" | unsafe) - 1L, starts))
      if (length(exceptional)) invisible(lapply(exceptional, function(line) {
        selected <- seq.int(starts[[line]] + 1L, ends[[line]])
        safe <- .safe_text(chars[selected])
        source_ends <- c(0L, cumsum(nchar(safe, type = "chars")))
        grapheme_ends <- cumsum(nchar(.editor_graphemes(paste0(safe, collapse = "")), type = "chars"))
        safe_positions[c(starts[[line]] + 1L, selected + 1L)] <<-
          safe_positions[[starts[[line]] + 1L]] + findInterval(source_ends, grapheme_ends)
      }))
      make_prefix <- function(text) {
        plain <- gsub("[\r\n]", " ", .safe_text(text))
        cells <- min(.screen_width(plain), max(0L, width - 2L))
        list(text = .screen_fit(plain, cells), width = cells)
      }
      prefixes <- list(make_prefix(prompt), make_prefix(continuation),
        make_prefix(if (isTRUE(config$unicode)) "\u21aa  " else ">  "))
      cell_positions <- c(0L, cumsum(widths))
      prefix_index <- ifelse(seq_along(starts) == 1L, 1L, 2L)
      prefix_widths <- vapply(prefixes, `[[`, numeric(1), "width")
      available <- pmax(1L, width - prefix_widths[prefix_index] - 1L)
      logical_widths <- cell_positions[ends + 1L] - cell_positions[starts + 1L]
      wrapped <- which(logical_widths > available)
      counts <- rep.int(1L, length(starts))
      groups <- lapply(wrapped, function(line) {
        from <- starts[[line]]
        end <- ends[[line]]
        positions <- cell_positions[seq.int(from + 1L, end + 1L)] - cell_positions[[from + 1L]]
        froms <- tos <- integer()
        part <- 1L
        repeat {
          prefix <- if (part > 1L) 3L else prefix_index[[line]]
          room <- max(1L, width - prefix_widths[[prefix]] - 1L)
          relative <- from - starts[[line]]
          to <- min(end, starts[[line]] + findInterval(positions[[relative + 1L]] + room, positions) - 1L)
          if (to <= from && from < end) to <- from + 1L
          froms <- c(froms, from)
          tos <- c(tos, to)
          if (to >= end) break
          from <- to
          part <- part + 1L
        }
        list(from = froms, to = tos)
      })
      if (length(wrapped)) counts[wrapped] <- vapply(groups, function(x) length(x$from), integer(1))
      logical <- rep.int(seq_along(starts), counts)
      row_starts <- starts[logical]
      row_ends <- ends[logical]
      row_prefix <- prefix_index[logical]
      cumulative <- c(0L, cumsum(counts))
      if (length(wrapped)) invisible(lapply(seq_along(wrapped), function(i) {
        line <- wrapped[[i]]
        selected <- seq.int(cumulative[[line]] + 1L, cumulative[[line + 1L]])
        row_starts[selected] <<- groups[[i]]$from
        row_ends[selected] <<- groups[[i]]$to
        row_prefix[selected[-1L]] <<- 3L
      }))
      out <- list(starts = row_starts, ends = row_ends, line = logical,
        prefix_index = row_prefix, prefixes = prefixes, widths = widths,
        cell_positions = cell_positions, safe_positions = safe_positions,
        logical_starts = starts, logical_lines = length(starts),
        source = source, width = width)
      stopifnot("wrapped geometry must cover the buffer" = length(out$starts) >= 1L &&
                  out$starts[[1L]] == 0L && utils::tail(out$ends, 1L) == length(chars))
      out
    })
}

#' Materialize One Visible Row from Compact Editor Geometry
#' @param geometry Prepared wrapped geometry.
#' @param index Physical row index.
#' @return Prompt, buffer positions, and cell boundaries for one row.
#' @keywords internal
#' @noRd
.editor_geometry_row <- function(geometry, index) {
  stopifnot("index must identify a visual row" = .is_count(index, 1L) && index <= length(geometry$starts))
  from <- geometry$starts[[index]]
  to <- geometry$ends[[index]]
  line <- geometry$line[[index]]
  prefix <- geometry$prefixes[[geometry$prefix_index[[index]]]]
  room <- max(1L, geometry$width - prefix$width - 1L)
  widths <- if (to > from) geometry$widths[seq.int(from + 1L, to)] else integer()
  out <- list(line = line, from = from, to = to, prefix = prefix$text, prefix_width = prefix$width,
    wrapped = geometry$prefix_index[[index]] == 3L, oversized = any(widths > room),
    columns = prefix$width + c(0L, cumsum(pmin(widths, room))),
    safe_first = geometry$safe_positions[[from + 1L]] -
      geometry$safe_positions[[geometry$logical_starts[[line]] + 1L]] + 1L,
    safe_last = geometry$safe_positions[[to + 1L]] -
      geometry$safe_positions[[geometry$logical_starts[[line]] + 1L]])
  stopifnot("row boundaries must match its graphemes" = length(out$columns) == to - from + 1L)
  out
}

#' Compose a Wrapped Editor Viewport and Mouse Hit Map
#' @param state Editor state.
#' @param geometry Prepared visual rows.
#' @param width,height Editor dimensions.
#' @param config Configuration.
#' @return A bounded editor frame with zero-based cursor and hit-test positions.
#' @keywords internal
#' @noRd
.editor_wrapped_frame <- function(state, geometry, width, height, config) {
  stopifnot("geometry must contain visual rows" = is.list(geometry) && length(geometry$starts) > 0L,
            "dimensions must be positive" = .is_count(width, 1L) && .is_count(height, 1L))
  current <- max(1L, min(length(geometry$starts), findInterval(state$cursor, geometry$starts)))
  extra_count <- min(length(state$overlay), height %/% 2L)
  budget <- max(1L, height - extra_count)
  first <- max(1L, current - budget + 1L)
  last <- min(length(geometry$starts), first + budget - 1L)
  visible <- lapply(seq.int(first, last), function(index) .editor_geometry_row(geometry, index))
  rendered <- .screen_cached(config$highlight_cache, "wrapped_render",
    list(geometry$source, visible, first, width, config$theme, config$color, config$rainbow), {
      from_line <- visible[[1L]]$line
      to_line <- utils::tail(visible, 1L)[[1L]]$line
      colored <- .highlight_r_window(geometry$source, from_line, to_line, config$theme,
        config$color, rainbow = !identical(config$rainbow, FALSE))
      tokens <- lapply(colored, .screen_tokens)
      vapply(seq_along(visible), function(i) {
        row <- visible[[i]]
        prefix <- row$prefix
        if (i == 1L && first > 1L && !row$wrapped)
          prefix <- .screen_fit("^ ", row$prefix_width)
        code <- if (isTRUE(row$oversized)) "?" else
          .screen_token_slice(tokens[[row$line - from_line + 1L]], row$safe_first, row$safe_last)
        role <- if (row$line == 1L && !row$wrapped) "prompt" else "continuation"
        paste0(style_text(prefix, role, config$theme, config$color), code)
      }, character(1))
    })
  focus <- .editor_pair_focus(state, geometry$source, config)
  if (nrow(focus)) rendered <- vapply(seq_along(visible), function(i) {
    row <- visible[[i]]
    if (isTRUE(row$oversized) || !any(focus[, 1L] <= row$to & focus[, 2L] > row$from))
      return(rendered[[i]])
    prefix <- if (i == 1L && first > 1L && !row$wrapped)
      .screen_fit("^ ", row$prefix_width) else row$prefix
    .editor_pair_row(rendered[[i]], focus, row$from + 1L, row$to,
      length(.editor_graphemes(prefix)), config, positions = geometry$safe_positions)
  }, character(1))
  extras <- if (extra_count) .screen_cached(config$highlight_cache, "editor_overlays",
    list(state$overlay, extra_count, width), {
      text <- utils::head(state$overlay, extra_count)
      if (length(state$overlay) > extra_count)
        text[[extra_count]] <- "More: :keys. Use the escape shortcut to close."
      vapply(gsub("[\r\n]", " ", .safe_text(text)), .screen_fit, character(1), width = max(1L, width - 1L))
    }) else character()
  row <- visible[[current - first + 1L]]
  out <- list(lines = c(rendered, extras), cursor_row = current - first,
    cursor_column = min(width - 1L, row$columns[[state$cursor - row$from + 1L]]),
    searching = !is.null(state$search), logical_lines = geometry$logical_lines,
    geometry = geometry, hit_rows = visible, visual_row = current)
  stopifnot("wrapped frame must retain a visible cursor" = length(out$lines) <= height &&
              out$cursor_row >= 0L && out$cursor_row < length(out$lines) && out$cursor_column >= 0L)
  out
}

#' Place the Editor Cursor from a Terminal Mouse Event
#' @param state Editor state.
#' @param event One-based native mouse coordinates.
#' @param frame Current editor frame with a hit map.
#' @param layout Fullscreen geometry.
#' @return The editor state, unchanged for clicks outside editable rows.
#' @keywords internal
#' @noRd
.editor_mouse <- function(state, event, frame, layout) {
  stopifnot("mouse placement needs editor and layout metadata" = is.list(state) &&
              is.list(event) && is.list(frame) && is.list(layout))
  if (!identical(event$type, "mouse") || !identical(event$key, "left") ||
      !identical(event$action, "press") || anyNA(c(event$x, event$y))) return(state)
  row <- event$y - layout$editor_start
  column <- event$x - 2L
  if (row < 1L || row > length(frame$hit_rows) || column < 0L || column >= layout$content_width)
    return(state)
  hit <- frame$hit_rows[[row]]
  state$cursor <- hit$from + which.min(abs(hit$columns - column)) - 1L
  state$completion <- NULL
  state$search <- NULL
  state$overlay <- character()
  stopifnot("mouse cursor must land on a grapheme boundary" = state$cursor >= 0L &&
              state$cursor <= length(state$chars))
  state
}

#' Move Vertically Through Wrapped Rows or Whole History Entries
#' @param state Editor state.
#' @param frame Current wrapped editor frame.
#' @param direction Minus one for up, plus one for down.
#' @param history Complete history entries.
#' @param config Optional configuration for history insertion limits.
#' @return The moved editor state.
#' @keywords internal
#' @noRd
.editor_visual_move <- function(state, frame, direction, history, config = NULL) {
  stopifnot("direction must be one row" = direction %in% c(-1L, 1L),
            "frame must contain wrapped geometry" = is.list(frame$geometry))
  index <- frame$visual_row + direction
  if (index < 1L || index > length(frame$geometry$starts)) return(.editor_history(state, direction, history, config))
  row <- .editor_geometry_row(frame$geometry, index)
  state$cursor <- row$from + which.min(abs(row$columns - frame$cursor_column)) - 1L
  state$completion <- NULL
  state$overlay <- character()
  stopifnot("vertical movement must retain a valid cursor" = state$cursor >= 0L && state$cursor <= length(state$chars))
  state
}

#' Fit an Editor Buffer into the Current Screen Layout
#' @param state Editor state.
#' @param size Terminal dimensions.
#' @param prompt,continuation Prompt text.
#' @param status Inline status text.
#' @param config Configuration, optionally containing an active screen.
#' @return An editor frame.
#' @keywords internal
#' @noRd
.editor_display_frame <- function(state, size, prompt, continuation, status, config) {
  stopifnot("state and config must be lists" = is.list(state) && is.list(config),
            "size must contain dimensions" = is.numeric(size) && all(c("width", "height") %in% names(size)))
  full <- is.environment(config$screen) && isTRUE(config$screen$active)
  if (full) {
    initial <- .screen_layout(config$screen, config, size[["width"]], size[["height"]])
    geometry <- .editor_wrap_geometry(state, initial$content_width, prompt, continuation, config)
    wanted <- length(geometry$starts) + min(3L, length(state$overlay))
    layout <- .screen_layout(config$screen, config, size[["width"]], size[["height"]], wanted)
    result <- .editor_wrapped_frame(state, geometry, layout$content_width, layout$editor_height, config)
  } else result <- .editor_frame(state, size[["width"]], size[["height"]],
                                 prompt, continuation, status, config)
  stopifnot("display frame must have lines" = is.list(result) && length(result$lines) >= 1L)
  result
}

#' Paint one editor frame
#' @keywords internal
#' @noRd
.editor_draw <- function(frame, previous = NULL, clear = FALSE, config = list()) {
  stopifnot("frame must contain text lines" = is.list(frame) && is.character(frame$lines),
            "clear must be one logical" = is.logical(clear) && length(clear) == 1L)
  prefix <- if (clear) "\033[2J\033[H" else paste0("\r",
    if (!is.null(previous) && previous$cursor_row > 0L) sprintf("\033[%dA", previous$cursor_row) else "",
    "\033[J")
  up <- length(frame$lines) - 1L - frame$cursor_row
  update <- paste0("\033[?25l", prefix, paste(frame$lines, collapse = "\r\n"), "\033[0m\r",
    if (up > 0L) sprintf("\033[%dA", up) else "",
    if (frame$cursor_column > 0L) sprintf("\033[%dC", frame$cursor_column) else "", "\033[?25h")
  term_write(update, synchronize = isTRUE(config$synchronize), cursor = "")
  stopifnot("drawn frame must retain text" = is.character(frame$lines))
  invisible(frame)
}

#' Read one complete editable expression
#' @keywords internal
#' @noRd
read_expression <- function(prompt = "r> ", continuation = "+  ", history = character(),
                            envir = .GlobalEnv, config = list(), status = character()) {
  stopifnot("prompt and continuation must be strings" =
              is.character(prompt) && length(prompt) == 1L && !is.na(prompt) &&
              is.character(continuation) && length(continuation) == 1L && !is.na(continuation),
            "history and status must be text" = is.character(history) && !anyNA(history) &&
              is.character(status) && !anyNA(status),
            "envir must be an environment" = is.environment(envir),
            "config must be a list" = is.list(config))
  defaults <- list(theme = repl_theme(), color = TRUE, unicode = TRUE, pipe = "|>",
                   max_paste = 100000L, max_lines = 2000L, mouse = TRUE, synchronize = FALSE,
                   rainbow = TRUE, autopair = TRUE, match_pairs = TRUE)
  config_class <- class(config)
  configured_theme <- config$theme
  config <- utils::modifyList(defaults, config)
  class(config) <- config_class
  if (!is.null(configured_theme)) config$theme <- configured_theme
  config$highlight_cache <- new.env(parent = emptyenv())
  if (is.null(config$key_lookup)) config$key_lookup <- .key_lookup(
    if (is.null(config$keybindings)) .default_keybindings() else config$keybindings)
  full <- is.environment(config$screen) && isTRUE(config$screen$active)
  handle <- term_open(mouse = full && isTRUE(config$mouse))
  if (is.null(handle)) stop("The enhanced editor requires an interactive terminal.", call. = FALSE)
  on.exit(term_close(handle), add = TRUE)
  state <- .editor_state(history = history)
  if (!is.null(config$initial_text)) state <- .editor_replace_checked(state, config, text = config$initial_text)
  size <- term_size()
  size <- pmax(size, c(width = 1L, height = 1L))
  frame <- .editor_display_frame(state, size, prompt, continuation, status, config)
  if (full) {
    if (is.null(config$screen$mode)) config$screen$mode <- "READY"
    config$screen$mouse_selection <- .mouse_state()
    screen_render(config$screen, frame, config, size)
  } else .editor_draw(frame, config = config)
  idle_ticks <- 0L
  repeat {
    event <- term_read(handle, 50L)
    editing_event <- FALSE
    if (identical(event$type, "timeout")) {
      idle_ticks <- (idle_ticks + 1L) %% 4L
      if (idle_ticks != 0L) next
      next_size <- pmax(term_size(), c(width = 1L, height = 1L))
      if (identical(next_size, size)) next
      event$type <- "resize"
    }
    if (full) {
      visible <- config$screen$last_base_frame
      if (is.null(visible)) visible <- config$screen$last_frame
      mouse <- .mouse_dispatch(event, visible, config$screen$mouse_selection, config)
      config$screen$mouse_selection <- mouse$selection
      if (nzchar(mouse$status)) config$screen$status <- mouse$status
      event <- mouse$event
      if (is.null(event)) {
        if (mouse$redraw) screen_render(config$screen, frame, config, size)
        next
      }
    }
    action <- .key_action(event, config)
    if (identical(event$type, "resize")) {
      size <- pmax(term_size(), c(width = 1L, height = 1L))
    } else if (identical(action, "help")) {
      state <- .editor_open_shortcuts(state, config, handle, alternate = !full)
      size <- pmax(term_size(), c(width = 1L, height = 1L))
      editing_event <- TRUE
    } else if (full && (action %in% c("scroll_up", "scroll_down", "scroll_line_up", "scroll_line_down") ||
                         (identical(event$type, "mouse") && event$key %in% c("wheelup", "wheeldown")))) {
      amount <- if (action %in% c("scroll_up", "scroll_down"))
        max(1L, config$screen$last_frame$layout$transcript_height - 1L) else
          if (identical(event$type, "mouse")) 3L else 1L
      direction <- if (action %in% c("scroll_up", "scroll_line_up") || identical(event$key, "wheelup")) 1L else -1L
      config$screen$scroll <- max(0L, config$screen$scroll + direction * amount)
    } else if (full && identical(event$type, "mouse")) {
      if (!identical(event$action, "press") || !identical(event$key, "left")) next
      staged <- .sidebar_command(event, config$screen$last_base_frame)
      state <- if (!is.null(staged)) .editor_replace_checked(state, config,
        from = 0L, to = length(state$chars), text = staged) else
        .editor_mouse(state, event, frame, config$screen$last_frame$layout)
      editing_event <- TRUE
    } else if (full && is.null(state$search) && action %in% c("up", "down")) {
      state <- .editor_visual_move(state, frame, if (identical(action, "up")) -1L else 1L, history, config)
      editing_event <- TRUE
    } else {
      state <- .editor_step(state, event, history, envir, config)
      editing_event <- TRUE
    }
    if (!is.null(state$help_request)) {
      state <- .editor_open_help(state, config, handle, alternate = !full)
      size <- pmax(term_size(), c(width = 1L, height = 1L))
      editing_event <- TRUE
    }
    if (!identical(state$action, "edit")) break
    next_frame <- if (editing_event || identical(event$type, "resize"))
      .editor_display_frame(state, size, prompt, continuation, status, config) else frame
    if (full) {
      if (editing_event) config$screen$mode <- if (!is.null(state$search)) "SEARCH" else "EDIT"
      if (state$clear) {
        screen_invalidate(config$screen)
        config$screen$mouse_selection <- .mouse_state()
      }
      screen_render(config$screen, next_frame, config, size)
    } else .editor_draw(next_frame, frame, state$clear, config)
    state$clear <- FALSE
    frame <- next_frame
  }
  if (full) config$screen$mouse_selection <- .mouse_state()
  if (!full) {
    # Inline mode keeps output below the complete editing block in scrollback.
    down <- length(frame$lines) - 1L - frame$cursor_row
    term_write(paste0("\r", if (down > 0L) sprintf("\033[%dB", down) else "", "\033[0m\r\n"))
  }
  result <- list(action = state$action, text = paste0(state$chars, collapse = ""))
  stopifnot("expression result must have a terminal action and text" =
              result$action %in% c("submit", "cancel", "eof") &&
              is.character(result$text) && length(result$text) == 1L && !is.na(result$text))
  result
}
