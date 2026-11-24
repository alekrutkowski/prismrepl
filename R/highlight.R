#' Highlight R source without evaluating it
#'
#' A tolerant lexical highlighter for complete or incomplete R expressions.
#' It recognizes comments, quoted and raw strings, backtick names, numbers,
#' reserved words, operators, and function calls. Highlighting does not parse,
#' evaluate, or look up names. Unknown or invalid syntax remains editable.
#'
#' @param text A character vector of R source. Embedded newlines are preserved.
#' @param theme A theme returned by [repl_theme()].
#' @param color Whether to add ANSI SGR attributes.
#' @param rainbow Color matching parentheses, brackets, braces, and quote
#'   delimiters by nesting depth. Literal contents retain their syntax colors.
#' @return A character vector with the same length as `text`. Terminal control
#'   characters are escaped before display; tabs become four spaces.
#' @export
#' @examples
#' highlight_r("mean(c(1, 2, NA), na.rm = TRUE)", color = FALSE)
highlight_r <- function(text, theme = repl_theme(), color = TRUE, rainbow = TRUE) {
  stopifnot("text must be a character vector without missing values" =
              is.character(text) && !anyNA(text),
            "theme must have named colors" = is.list(theme) &&
              is.character(theme$colors) && !is.null(names(theme$colors)),
            "color must be TRUE or FALSE" = is.logical(color) &&
              length(color) == 1L && !is.na(color),
            "rainbow must be TRUE or FALSE" = .is_flag(rainbow))
  text <- .safe_text(text)
  if (!color || identical(theme$name, "mono")) return(text)
  out <- vapply(text, function(line)
    .highlight_r_segment(line, 1L, nchar(line, type = "chars"), theme, color, rainbow),
    character(1), USE.NAMES = FALSE)
  stopifnot("highlighted text must preserve vector length" = length(out) == length(text),
            "highlighted text must have no missing values" = !anyNA(out))
  out
}


#' Highlight a visible range while retaining lexical context
#'
#' The complete buffer is scanned by a lexical regular expression, but only
#' tokens intersecting the requested logical lines are styled in R. Strings
#' and raw strings opened above the viewport retain their correct color.
#' @param text One full source buffer.
#' @param from_line,to_line Inclusive logical line numbers.
#' @param theme A theme list.
#' @param color Emit ANSI styles.
#' @param rainbow Apply nesting colors to matching delimiters.
#' @return One character string per selected logical line, including empty lines.
#' @keywords internal
#' @noRd
.highlight_r_window <- function(text, from_line, to_line,
                                theme = repl_theme(), color = TRUE, rainbow = TRUE) {
  stopifnot("text must be a scalar source string" = is.character(text) &&
              length(text) == 1L && !is.na(text),
            "line bounds must be positive ordered integers" =
              is.numeric(from_line) && length(from_line) == 1L &&
              is.finite(from_line) && from_line >= 1L && from_line == floor(from_line) &&
              is.numeric(to_line) && length(to_line) == 1L && is.finite(to_line) &&
              to_line >= from_line && to_line == floor(to_line),
            "theme must contain colors" = is.list(theme) && is.character(theme$colors),
            "color must be a logical scalar" = is.logical(color) &&
              length(color) == 1L && !is.na(color),
            "rainbow must be a logical scalar" = .is_flag(rainbow))
  text <- .safe_text(text)
  lines <- stringi::stri_split_fixed(text, "\n", omit_empty = FALSE)[[1L]]
  stopifnot("line bounds must be within the source" = to_line <= length(lines))
  sizes <- nchar(lines, type = "chars")
  starts <- c(1, head(cumsum(sizes + 1), -1L) + 1)
  first <- starts[[from_line]]
  last <- starts[[to_line]] + sizes[[to_line]] - 1
  rendered <- if (!color || identical(theme$name, "mono")) {
    substring(text, first, last)
  } else .highlight_r_segment(text, first, last, theme, color, rainbow)
  out <- stringi::stri_split_fixed(rendered, "\n", omit_empty = FALSE)[[1L]]
  stopifnot("highlighted window must preserve logical line count" =
              length(out) == to_line - from_line + 1L,
            "highlighted window must be nonmissing text" = is.character(out) && !anyNA(out))
  out
}

#' Build the tolerant R lexical pattern
#' @return One regular expression compatible with ICU and PCRE.
#' @keywords internal
#' @noRd
.r_lex_pattern <- function() {
  raw <- paste0("(?s:[rR]\"(-*)\\(.*?\\)\\1\"|",
                "[rR]\"(-*)\\[.*?\\]\\2\"|",
                "[rR]\"(-*)\\{.*?\\}\\3\")")
  out <- paste(c(
    raw,
    "(?s:[rR]\"-*(?:\\(|\\[|\\{).*$)",
    "#[^\\r\\n]*",
    "\"(?:\\\\[\\s\\S]|[^\"\\\\])*(?:\"|$)",
    "'(?:\\\\[\\s\\S]|[^'\\\\])*(?:'|$)",
    "`(?:\\\\[\\s\\S]|[^`\\\\])*(?:`|$)",
    "(?:0[xX][0-9A-Fa-f]+(?:\\.[0-9A-Fa-f]*)?(?:[pP][+-]?[0-9]+)?|(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?)(?:L|i)?",
    "(?:[\\p{L}_]|\\.(?![0-9]))[\\p{L}\\p{M}\\p{N}_.]*",
    "%[^%\\n]*(?:%|$)|<<-|->>|<-|->|:::|::|\\|>|&&|\\|\\||==|!=|<=|>=|[+*/^$@~:!<>=&|%-]",
    "\\s+", "(?s:.)"), collapse = "|")
  stopifnot("lexer pattern must be one string" = length(out) == 1L && nzchar(out))
  out
}

#' Locate R Tokens With Unicode Character Offsets in One Lexer Pass
#' @param text One UTF-8 source buffer.
#' @return A two-column integer matrix of inclusive token boundaries.
#' @keywords internal
#' @noRd
.r_lex_spans <- function(text) {
  stopifnot("text must be nonmissing scalar source" = is.character(text) &&
              length(text) == 1L && !is.na(text))
  out <- stringi::stri_locate_all_regex(text, .r_lex_pattern(), omit_no_match = TRUE)[[1L]]
  stopifnot("lexer spans must contain ordered character boundaries" =
              is.matrix(out) && ncol(out) == 2L && is.integer(out) && !anyNA(out) &&
              all(out[, 1L] >= 1L & out[, 1L] <= out[, 2L]),
            "lexer spans must cover the source" =
              sum(out[, 2L] - out[, 1L] + 1L) == nchar(text, type = "chars"))
  out
}

#' Style a source interval after scanning surrounding context
#' @param text Safe UTF-8 source text.
#' @param first,last Character interval to display.
#' @param theme Theme list.
#' @param color Emit color attributes.
#' @param rainbow Apply nesting colors to matching delimiters.
#' @return One source fragment, optionally styled.
#' @keywords internal
#' @noRd
.highlight_r_segment <- function(text, first, last, theme, color, rainbow = TRUE) {
  stopifnot("text must be one string" = is.character(text) && length(text) == 1L && !is.na(text),
            "first and last must be scalar positions" = is.numeric(first) &&
              length(first) == 1L && is.numeric(last) && length(last) == 1L &&
              is.finite(first) && is.finite(last),
            "theme must contain colors" = is.list(theme) && is.character(theme$colors),
            "color must be logical" = is.logical(color) && length(color) == 1L && !is.na(color),
            "rainbow must be logical" = .is_flag(rainbow))
  if (!nzchar(text) || first > last) return("")
  spans <- .r_lex_spans(text)
  hits <- spans[, 1L]
  ends <- spans[, 2L]
  use <- which(hits <= last & ends >= first)
  points <- utf8ToInt(text)
  levels <- if (rainbow) .r_rainbow_spans(points, hits, ends, max(c(0L, use))) else integer(length(hits))
  reserved <- c("if", "else", "repeat", "while", "function", "for", "in",
                "next", "break", "TRUE", "FALSE", "NULL", "NA", "Inf", "NaN",
                "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
  token_at <- function(i) .r_highlight_span(points, hits[[i]], ends[[i]])
  pieces <- vapply(use, function(i) {
    token <- token_at(i)
    role <- if (startsWith(token, "#")) "comment" else
      if (grepl("^[rR]\"-*[([{]", token) || startsWith(token, "\"") ||
          startsWith(token, "'")) "string" else
      if (token %in% reserved) "keyword" else
      if (grepl("^(?:[0-9]|\\.[0-9])", token, perl = TRUE)) "number" else
      if (startsWith(token, "`")) "symbol" else
      if (grepl("^[+*/^$@~:!<>=&|%-]", token)) "operator" else "input"
    if (identical(role, "input") && grepl("^[\\p{L}_.]", token, perl = TRUE)) {
      next_i <- i + 1L
      if (next_i <= length(hits) && grepl("^\\s+$", token_at(next_i), perl = TRUE))
        next_i <- next_i + 1L
      if (next_i <= length(hits) && identical(token_at(next_i), "(")) role <- "function_name"
    }
    if (levels[[i]] > 0L) {
      quote <- .r_quote_bounds(token)
      if (!is.null(quote)) return(.highlight_quote_segment(token,
        max(first, hits[[i]]) - hits[[i]] + 1L,
        min(last, ends[[i]]) - hits[[i]] + 1L, quote,
        role, .rainbow_role(levels[[i]], theme), theme, color))
      role <- .rainbow_role(levels[[i]], theme)
    }
    piece <- .r_highlight_span(points, max(first, hits[[i]]), min(last, ends[[i]]))
    style_text(piece, role, theme, color)
  }, character(1), USE.NAMES = FALSE)
  out <- paste0(pieces, collapse = "")
  stopifnot("styled fragment must be one string" = is.character(out) && length(out) == 1L)
  out
}

#' Extract a Visible Source Span From Already Decoded Unicode Code Points
#' @param points Unicode code points decoded once for the complete source.
#' @param first,last Inclusive character positions within that source.
#' @return One UTF-8 source fragment.
#' @keywords internal
#' @noRd
.r_highlight_span <- function(points, first, last) {
  stopifnot("points must be an integer vector" = is.integer(points),
            "span must lie within the decoded source" = .is_count(first, 1L) &&
              .is_count(last, first) && last <= length(points))
  out <- intToUtf8(points[seq.int(first, last)])
  stopifnot("source fragment must be nonmissing scalar text" =
              is.character(out) && length(out) == 1L && !is.na(out))
  out
}

#' Style Quote Delimiters Separately From Their Literal Contents
#' @param token Complete lexical quote token.
#' @param first,last Inclusive character positions within the token.
#' @param bounds Quote delimiter widths from .r_quote_bounds().
#' @param role Literal-content theme role.
#' @param pair_role Delimiter theme role.
#' @param theme Theme containing named colors.
#' @param color Emit ANSI styles.
#' @return One safely styled source fragment.
#' @keywords internal
#' @noRd
.highlight_quote_segment <- function(token, first, last, bounds,
                                     role, pair_role, theme, color) {
  stopifnot("token must be scalar text" = is.character(token) && length(token) == 1L && !is.na(token),
            "quote bounds and theme must be lists" = is.list(bounds) && is.list(theme),
            "interval must fit within the token" = .is_count(first, 1L) &&
              .is_count(last, first) && last <= nchar(token, type = "chars"),
            "roles must be text" = .is_text(role) && .is_text(pair_role),
            "color must be logical" = .is_flag(color))
  size <- nchar(token, type = "chars")
  starts <- c(1L, bounds$open + 1L, size - bounds$close + 1L)
  ends <- c(bounds$open, size - bounds$close, size)
  roles <- c(pair_role, role, pair_role)
  use <- which(starts <= ends & starts <= last & ends >= first)
  out <- paste0(vapply(use, function(i) style_text(substring(token,
    max(first, starts[[i]]), min(last, ends[[i]])), roles[[i]], theme, color),
    character(1L), USE.NAMES = FALSE), collapse = "")
  stopifnot("quote fragment must be scalar text" = is.character(out) && length(out) == 1L && !is.na(out))
  out
}
