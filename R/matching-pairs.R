#' Index Lexically Matching Delimiters Without Evaluating Input
#' @param text One UTF-8 source string, which may be incomplete.
#' @return A list of paired endpoint spans and a direct code-point lookup.
#' @keywords internal
#' @noRd
.r_matching_pairs <- function(text) {
  stopifnot("text must be one nonmissing string" = is.character(text) &&
              length(text) == 1L && !is.na(text))
  points <- utf8ToInt(text)
  spans <- .r_lex_spans(text)
  pairs <- matrix(0L, nrow = nrow(spans), ncol = 4L,
    dimnames = list(NULL, c("open_first", "open_last", "close_first", "close_last")))
  lookup <- integer(length(points))
  if (!nrow(spans)) return(list(pairs = pairs, lookup = lookup))
  starts <- spans[, 1L]
  ends <- spans[, 2L]
  kinds <- match(points[starts], c(40L, 91L, 123L, 41L, 93L, 125L), nomatch = 0L)
  kinds[starts != ends] <- 0L
  # Ordinary quotes are indexed in one vectorized pass. Only quotes ending in
  # a backslash run need an escape-parity check, so large pasted buffers avoid
  # a separate regex scan and validation call for every short string literal.
  quoted <- which(points[starts] %in% c(34L, 39L, 96L) & ends > starts &
    points[ends] == points[starts])
  escaped <- which(points[ends[quoted] - 1L] == 92L)
  if (length(escaped)) {
    closed <- vapply(quoted[escaped], function(i) {
      position <- ends[[i]] - 1L
      while (position > starts[[i]] && points[[position]] == 92L) position <- position - 1L
      (ends[[i]] - position - 1L) %% 2L == 0L
    }, logical(1))
    bad <- escaped[!closed]
    if (length(bad)) quoted <- quoted[-bad]
  }
  count <- length(quoted)
  if (count) {
    pairs[seq_len(count), ] <- cbind(starts[quoted], starts[quoted], ends[quoted], ends[quoted])
    lookup[c(starts[quoted], ends[quoted])] <- rep.int(seq_len(count), 2L)
  }
  raw <- which(points[starts] %in% c(114L, 82L) & starts < ends &
    points[pmin(starts + 1L, length(points))] == 34L)
  for (i in raw) {
    bounds <- .r_quote_bounds(intToUtf8(points[seq.int(starts[[i]], ends[[i]])]))
    if (is.null(bounds) || !isTRUE(bounds$closed)) next
    count <- count + 1L
    pair <- c(starts[[i]], starts[[i]] + bounds$open - 1L,
      ends[[i]] - bounds$close + 1L, ends[[i]])
    pairs[count, ] <- pair
    lookup[c(seq.int(pair[[1L]], pair[[2L]]), seq.int(pair[[3L]], pair[[4L]]))] <- count
  }
  candidates <- which(kinds > 0L)
  stack <- integer(length(candidates))
  top <- 0L
  for (i in candidates) {
    kind <- kinds[[i]]
    if (kind > 0L && kind <= 3L) {
      top <- top + 1L
      stack[[top]] <- i
    } else if (kind > 3L) {
      if (!top || kinds[[stack[[top]]]] != kind - 3L) next
      opening <- starts[[stack[[top]]]]
      top <- top - 1L
      count <- count + 1L
      pairs[count, ] <- c(opening, opening, starts[[i]], ends[[i]])
      lookup[c(opening, starts[[i]])] <- count
    }
  }
  out <- list(pairs = pairs[seq_len(count), , drop = FALSE], lookup = lookup)
  stopifnot("pair endpoints must be ordered and indexed" = ncol(out$pairs) == 4L &&
              all(out$pairs[, 1L] <= out$pairs[, 2L]) &&
              all(out$pairs[, 2L] < out$pairs[, 3L]) &&
              all(out$pairs[, 3L] <= out$pairs[, 4L]) &&
              length(out$lookup) == length(points) && all(out$lookup <= nrow(out$pairs)))
  out
}

#' Find the Pair Under or Immediately Before the Editor Cursor
#' @param state An editor state with graphemes and a zero-based cursor boundary.
#' @param source Source text corresponding to the state.
#' @param config Configuration containing an optional local highlight cache.
#' @return A two-column matrix of inclusive grapheme spans, or an empty matrix.
#' @keywords internal
#' @noRd
.editor_pair_focus <- function(state, source, config) {
  stopifnot("state must contain a valid grapheme cursor" = is.list(state) &&
              is.character(state$chars) && .is_count(state$cursor) && state$cursor <= length(state$chars),
            "source must be one string" = is.character(source) && length(source) == 1L && !is.na(source),
            "config must be a list" = is.list(config))
  empty <- matrix(integer(), ncol = 2L, dimnames = list(NULL, c("first", "last")))
  if (identical(config$match_pairs, FALSE) || !isTRUE(config$color) ||
      !"pair_match" %in% names(config$theme$colors) || !nzchar(config$theme$colors[["pair_match"]]) ||
      !nzchar(source)) return(empty)
  index <- .screen_cached(config$highlight_cache, "matching_pairs", source, {
    out <- if (startsWith(trimws(source, which = "left"), ":"))
      list(pairs = matrix(integer(), ncol = 4L), lookup = integer(nchar(source))) else
      .r_matching_pairs(source)
    boundaries <- c(0L, cumsum(nchar(state$chars, type = "chars")))
    if (nrow(out$pairs)) out$pairs[] <- findInterval(out$pairs - 1L, boundaries)
    # Store a direct lookup at both sides of each grapheme boundary. Cursor
    # movement then needs two array accesses, without rescanning source text.
    out$right <- c(out$lookup[utils::head(boundaries, -1L) + 1L], 0L)
    out$left <- c(0L, utils::head(out$right, -1L))
    out$lookup <- NULL
    out
  })
  id <- index$right[[state$cursor + 1L]]
  if (!id) id <- index$left[[state$cursor + 1L]]
  if (!id) return(empty)
  out <- matrix(index$pairs[id, ], ncol = 2L, byrow = TRUE,
    dimnames = list(NULL, c("first", "last")))
  stopifnot("focused endpoints must be valid grapheme spans" = nrow(out) == 2L &&
              all(out[, 1L] >= 1L) && all(out[, 2L] <= length(state$chars)) &&
              all(out[, 1L] <= out[, 2L]))
  out
}

#' Overlay a Pair Role on a Bounded Styled Editor Row
#' @param text One already highlighted visible row.
#' @param focus Paired inclusive source-grapheme spans.
#' @param first,last Inclusive source-grapheme positions displayed in this row.
#' @param prefix Number of printable graphemes preceding the source slice.
#' @param config Console configuration.
#' @param positions Optional cumulative safe-display grapheme positions.
#' @param chars Visible source graphemes, used only without prepared positions.
#' @return The same visible text with focused delimiters restyled.
#' @keywords internal
#' @noRd
.editor_pair_row <- function(text, focus, first, last, prefix, config,
                             positions = NULL, chars = NULL) {
  stopifnot("text must be one nonmissing string" = is.character(text) && length(text) == 1L && !is.na(text),
            "focus must contain endpoint spans" = is.matrix(focus) && ncol(focus) == 2L,
            "row bounds must be ordered" = .is_count(first, 1L) && .is_count(last) && last >= first - 1L,
            "prefix must be a grapheme count" = .is_count(prefix),
            "config must be a list" = is.list(config))
  if (!nrow(focus) || last < first) return(text)
  ranges <- focus[focus[, 1L] <= last & focus[, 2L] >= first, , drop = FALSE]
  if (!nrow(ranges)) return(text)
  ranges[, 1L] <- pmax(first, ranges[, 1L])
  ranges[, 2L] <- pmin(last, ranges[, 2L])
  if (!is.null(positions)) {
    ranges[, 1L] <- positions[ranges[, 1L]] - positions[[first]] + 1L
    ranges[, 2L] <- positions[ranges[, 2L] + 1L] - positions[[first]]
  } else {
    stopifnot("visible source graphemes must match the row" = is.character(chars) &&
                length(chars) == last - first + 1L)
    safe_count <- function(n) length(.editor_graphemes(.safe_text(paste0(utils::head(chars, n), collapse = ""))))
    ranges[, 1L] <- vapply(ranges[, 1L] - first, safe_count, integer(1)) + 1L
    ranges[, 2L] <- vapply(ranges[, 2L] - first + 1L, safe_count, integer(1))
  }
  ranges <- ranges + prefix
  out <- .screen_cached(config$highlight_cache, "matching_pair_row",
    list(text, ranges, config$theme$colors["pair_match"], config$color), {
      parts <- .screen_tokens(text)
      n <- length(parts$printable)
      chunks <- character(2L * nrow(ranges) + 1L)
      from <- 1L
      for (i in seq_len(nrow(ranges))) {
        lo <- max(from, ranges[i, 1L])
        hi <- min(n, ranges[i, 2L])
        if (hi < lo) next
        chunks[[2L * i - 1L]] <- .screen_token_slice(parts, from, lo - 1L)
        plain <- paste0(parts$tokens[parts$printable[seq.int(lo, hi)]], collapse = "")
        chunks[[2L * i]] <- style_text(plain, "pair_match", config$theme, config$color)
        from <- hi + 1L
      }
      chunks[[length(chunks)]] <- .screen_token_slice(parts, from, max(from - 1L, n))
      paste0(chunks, collapse = "")
    })
  stopifnot("focused row must be one string" = is.character(out) && length(out) == 1L && !is.na(out))
  out
}
