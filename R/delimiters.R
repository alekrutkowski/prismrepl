#' Describe the Delimiters of a Lexical R String or Backtick Name
#' @param token One token from the tolerant R lexer.
#' @return NULL for other tokens, or widths and closure state for a quote token.
#' @keywords internal
#' @noRd
.r_quote_bounds <- function(token) {
  stopifnot("token must be scalar text" = is.character(token) &&
              length(token) == 1L && !is.na(token))
  size <- nchar(token, type = "chars")
  raw <- regexpr('^[rR]"-*[([{]', token, perl = TRUE)
  if (raw[[1L]] == 1L) {
    opening <- attr(raw, "match.length")[[1L]]
    bracket <- substring(token, opening, opening)
    closing <- paste0(c("(" = ")", "[" = "]", "{" = "}")[[bracket]],
      substring(token, 3L, opening - 1L), '"')
    closed <- size >= opening + nchar(closing) && endsWith(token, closing)
    out <- list(open = opening, close = if (closed) nchar(closing) else 0L,
      quote = '"', raw = TRUE, closed = closed, escaped = FALSE)
  } else {
    quote <- substring(token, 1L, 1L)
    if (!quote %in% c('"', "'", "`")) return(NULL)
    escaped_tail <- function(text) {
      slashes <- stringi::stri_extract_last_regex(text, "\\\\+$")
      !is.na(slashes) && nchar(slashes, type = "chars") %% 2L == 1L
    }
    closed <- size >= 2L && endsWith(token, quote) &&
      !escaped_tail(substring(token, 1L, size - 1L))
    out <- list(open = 1L, close = as.integer(closed), quote = quote,
      raw = FALSE, closed = closed, escaped = !closed && escaped_tail(token))
  }
  stopifnot("quote boundaries must fit within their token" =
              out$open >= 1L && out$close >= 0L && out$open + out$close <= size,
            "quote closure and escape states must be logical" =
              .is_flag(out$closed) && .is_flag(out$escaped))
  out
}

#' Assign Rainbow Levels Without Treating Literal Contents as R Syntax
#' @param tokens Character vector of complete lexical tokens.
#' @param through Last token whose context is needed.
#' @return Integer nesting levels, with zero for tokens without pair coloring.
#' @keywords internal
#' @noRd
.r_rainbow_levels <- function(tokens, through = length(tokens)) {
  stopifnot("tokens must be nonmissing text" = is.character(tokens) && !anyNA(tokens),
            "through must be a valid token boundary" = .is_count(through) &&
              through <= length(tokens))
  kinds <- match(tokens, c("(", "[", "{", ")", "]", "}"), nomatch = 0L)
  quoted <- substring(tokens, 1L, 1L) %in% c('"', "'", "`") |
    grepl('^[rR]"-*[([{]', tokens, perl = TRUE)
  out <- .r_rainbow_nesting(kinds, quoted, through)
  stopifnot("rainbow levels must preserve token count" = length(out) == length(tokens))
  out
}

#' Classify Delimiters From Lexer Spans Without Extracting Every Source Token
#' @param points Unicode code points for the complete source.
#' @param starts,ends Inclusive character positions of lexical tokens.
#' @param through Last token whose nesting context is needed.
#' @return Integer nesting levels aligned with the lexical spans.
#' @keywords internal
#' @noRd
.r_rainbow_spans <- function(points, starts, ends, through = length(starts)) {
  stopifnot("source points must be nonmissing integers" = is.integer(points) && !anyNA(points),
            "lexer spans must be aligned and within the source" =
              is.numeric(starts) && is.numeric(ends) && length(starts) == length(ends) &&
              !anyNA(starts) && !anyNA(ends) && all(starts >= 1L & starts <= ends) &&
              all(ends <= length(points)),
            "through must be a valid token boundary" = .is_count(through) && through <= length(starts))
  first <- points[starts]
  sizes <- ends - starts + 1L
  kinds <- match(first, c(40L, 91L, 123L, 41L, 93L, 125L), nomatch = 0L)
  kinds[sizes != 1L] <- 0L
  quoted <- first %in% c(34L, 39L, 96L) |
    (first %in% c(114L, 82L) & sizes >= 3L & points[pmin(starts + 1L, length(points))] == 34L)
  out <- .r_rainbow_nesting(kinds, quoted, through)
  stopifnot("rainbow levels must align with lexical spans" = length(out) == length(starts))
  out
}

#' Match Nesting Levels Using Only Delimiter Kinds and Quote Flags
#' @param kinds Integer kinds: zero, three openers, or three matching closers.
#' @param quoted Logical quote flags aligned with kinds.
#' @param through Last token whose nesting context is needed.
#' @return Nonnegative integer nesting levels for all tokens.
#' @keywords internal
#' @noRd
.r_rainbow_nesting <- function(kinds, quoted, through = length(kinds)) {
  stopifnot("kinds must be supported delimiter integers" = is.integer(kinds) &&
              !anyNA(kinds) && all(kinds >= 0L & kinds <= 6L),
            "quote flags must align with delimiter kinds" = is.logical(quoted) &&
              !anyNA(quoted) && length(quoted) == length(kinds),
            "through must be a valid token boundary" = .is_count(through) && through <= length(kinds))
  out <- integer(length(kinds))
  if (!through) return(out)
  positions <- which(kinds > 0L | quoted)
  positions <- positions[positions <= through]
  stack <- integer(length(positions))
  top <- 0L
  # A preallocated stack keeps malformed input and deep nesting linear in size.
  for (position in positions) {
    kind <- kinds[[position]]
    if (!kind) out[[position]] <- top + 1L else if (kind <= 3L) {
      top <- top + 1L
      stack[[top]] <- kind
      out[[position]] <- top
    } else if (top > 0L && stack[[top]] == kind - 3L) {
      out[[position]] <- top
      top <- top - 1L
    }
  }
  stopifnot("rainbow levels must preserve token count" =
              is.integer(out) && length(out) == length(kinds) && all(out >= 0L))
  out
}

#' Resolve a Theme Role for a Rainbow Nesting Level
#' @param level Positive nesting level.
#' @param theme Theme containing named colors.
#' @return One existing foreground role.
#' @keywords internal
#' @noRd
.rainbow_role <- function(level, theme) {
  stopifnot("level must be positive" = .is_count(level, 1L),
            "theme must contain named colors" = is.list(theme) &&
              is.character(theme$colors) && !is.null(names(theme$colors)))
  index <- (level - 1L) %% 6L + 1L
  role <- paste0("rainbow", index)
  out <- if (role %in% names(theme$colors)) role else
    c("operator", "number", "function_name", "string", "keyword", "accent")[[index]]
  if (!out %in% names(theme$colors)) out <- "input"
  stopifnot("rainbow role must be present in the theme" = out %in% names(theme$colors))
  out
}

#' Inspect the Lexical Context Immediately Before an Editing Cursor
#' @param before Source preceding the cursor.
#' @return A list with a context kind, quote character and escape flag.
#' @keywords internal
#' @noRd
.editor_pair_context <- function(before) {
  stopifnot("before must be scalar text" = is.character(before) &&
              length(before) == 1L && !is.na(before))
  out <- list(kind = "code", quote = "", escaped = FALSE)
  if (nzchar(before)) {
    spans <- .r_lex_spans(before)
    token <- substring(before, utils::tail(spans[, 1L], 1L))
    bounds <- .r_quote_bounds(token)
    if (startsWith(token, "#")) out$kind <- "comment" else
      if (!is.null(bounds) && !bounds$closed) {
        out <- list(kind = if (bounds$raw) "raw" else "quote",
          quote = bounds$quote, escaped = bounds$escaped)
      } else if (startsWith(token, "%") &&
                 (nchar(token) == 1L || !endsWith(token, "%"))) out$kind <- "operator"
  }
  stopifnot("cursor context must have a supported kind" =
              out$kind %in% c("code", "comment", "raw", "quote", "operator"),
            "cursor escape state must be logical" = .is_flag(out$escaped))
  out
}

#' Update Generated Pair Markers After a Grapheme Replacement
#' @param pairs Generated pair markers or NULL.
#' @param from,to Original zero-based replacement boundaries.
#' @param delta Change in the buffer's grapheme count.
#' @param chars Updated grapheme buffer.
#' @return Valid pair markers, or NULL when none remain.
#' @keywords internal
#' @noRd
.editor_pairs_update <- function(pairs, from, to, delta, chars) {
  stopifnot("pairs must be NULL or a list" = is.null(pairs) || is.list(pairs),
            "replacement boundaries must be ordered" = .is_count(from) && .is_count(to) && to >= from,
            "delta must be an integer count" = is.numeric(delta) && length(delta) == 1L &&
              is.finite(delta) && delta == trunc(delta),
            "chars must be nonmissing text" = is.character(chars) && !anyNA(chars))
  if (is.null(pairs) || !length(pairs$open)) return(NULL)
  keep <- !(pairs$open > from & pairs$open <= to) &
    !(pairs$close > from & pairs$close <= to)
  pairs <- lapply(pairs, `[`, keep)
  pairs$open <- pairs$open + as.integer(pairs$open > to) * delta
  pairs$close <- pairs$close + as.integer(pairs$close > to) * delta
  keep <- pairs$open >= 1L & pairs$close <= length(chars) & pairs$open < pairs$close
  pairs <- lapply(pairs, `[`, keep)
  keep <- vapply(seq_along(pairs$open), function(i)
    grepl(pairs$left[[i]], chars[[pairs$open[[i]]]], fixed = TRUE) &&
      grepl(pairs$right[[i]], chars[[pairs$close[[i]]]], fixed = TRUE), logical(1L))
  out <- if (any(keep)) lapply(pairs, `[`, keep) else NULL
  stopifnot("updated pairs must fit the buffer" = is.null(out) ||
              (all(out$open >= 1L) && all(out$open < out$close) &&
                 all(out$close <= length(chars))))
  out
}

#' Type a Character With Optional Context-Aware Pair Insertion
#' @param state Editor state.
#' @param text Typed text; multicharacter events remain literal.
#' @param config Editor configuration.
#' @return An updated editor state with a valid cursor.
#' @keywords internal
#' @noRd
.editor_type <- function(state, text, config) {
  stopifnot("state and config must be lists" = is.list(state) && is.list(config),
            "text must be scalar text" = is.character(text) && length(text) == 1L && !is.na(text))
  candidates <- c("(", "[", "{", ")", "]", "}", '"', "'", "`")
  if (identical(config$autopair, FALSE) || !text %in% candidates ||
      startsWith(trimws(paste0(state$chars, collapse = ""), which = "left"), ":"))
    return(.editor_replace_checked(state, config, text = text))
  before <- paste0(utils::head(state$chars, state$cursor), collapse = "")
  context <- .editor_pair_context(before)
  next_char <- if (state$cursor < length(state$chars)) state$chars[[state$cursor + 1L]] else ""
  pairs <- state$autopairs
  pending <- !is.null(pairs) && any(pairs$close == state$cursor + 1L & pairs$right == text)
  if (pending && grepl(text, next_char, fixed = TRUE) && !context$escaped &&
      (context$kind == "code" ||
         (context$kind %in% c("quote", "raw") && identical(context$quote, text)))) {
    state$cursor <- state$cursor + 1L
    state$completion <- NULL
    state$limit_rejected <- FALSE
  } else {
    closers <- c("(" = ")", "[" = "]", "{" = "}", '"' = '"', "'" = "'", "`" = "`")
    opening <- text %in% names(closers)
    safe_after <- !nzchar(next_char) || grepl("^[[:space:]),;\\]}]$", next_char, perl = TRUE)
    quote <- text %in% c('"', "'", "`")
    word_before <- quote && grepl("[\\p{L}\\p{M}\\p{N}_.]$", before, perl = TRUE)
    raw_prefix <- identical(text, '"') &&
      grepl("(?:^|[^\\p{L}\\p{M}\\p{N}_.])[rR]$", before, perl = TRUE)
    pair <- opening && context$kind == "code" && safe_after && (!word_before || raw_prefix)
    if (pair) {
      state <- .editor_replace_checked(state, config, text = paste0(text, closers[[text]]))
      if (!isTRUE(state$limit_rejected)) {
        close <- state$cursor
        state$cursor <- close - 1L
        add <- list(open = state$cursor, close = close, left = text, right = closers[[text]])
        state$autopairs <- if (is.null(state$autopairs)) add else Map(c, state$autopairs, add)
      }
    } else state <- .editor_replace_checked(state, config, text = text)
  }
  stopifnot("typed state must retain a valid cursor" =
              state$cursor >= 0L && state$cursor <= length(state$chars))
  state
}

#' Remove a Generated Empty Pair With One Backspace
#' @param state Editor state with a nonzero cursor.
#' @param config Editor configuration.
#' @return The edited state.
#' @keywords internal
#' @noRd
.editor_pair_backspace <- function(state, config) {
  stopifnot("state must have a character before the cursor" =
              is.list(state) && .is_count(state$cursor, 1L),
            "config must be a list" = is.list(config))
  pairs <- state$autopairs
  pair <- !identical(config$autopair, FALSE) && !is.null(pairs) &&
    any(pairs$open == state$cursor & pairs$close == state$cursor + 1L &
      state$chars[pairs$open] == pairs$left & state$chars[pairs$close] == pairs$right)
  if (pair) {
    before <- paste0(utils::head(state$chars, state$cursor - 1L), collapse = "")
    pair <- !startsWith(trimws(before, which = "left"), ":") &&
      .editor_pair_context(before)$kind == "code"
  }
  out <- .editor_replace(state, state$cursor - 1L,
    state$cursor + if (pair) 1L else 0L)
  stopifnot("backspace must retain a valid cursor" =
              out$cursor >= 0L && out$cursor <= length(out$chars))
  out
}

