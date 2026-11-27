#' Parse Literal Arguments for a Custom Console Command
#'
#' A function with exactly one ordinary formal parameter receives the complete
#' remainder, preserving internal spaces, tabs, newlines and backslashes. One
#' matching pair of quotes around that whole remainder is removed. With other
#' signatures, whitespace separates arguments and single, double or backtick
#' quotes group a complete argument. Quotes inside an unquoted word are literal,
#' so apostrophes in names do not need escaping. Backslashes are always literal;
#' use a different quote style to include quotation marks in a grouped argument.
#' For example, `"C:\\my folder\\" next` has two arguments and retains the final
#' path separator. A bare `NA` remains the string `"NA"`; `""` supplies an empty
#' string. No source code, escapes, variable references or shell syntax are
#' evaluated. Unclosed quotes and adjacent quoted fields without whitespace
#' are errors. The quoted fragments in `"one" "two"` remain literal when the
#' function has a single ordinary parameter.
#'
#' @param raw One character string containing the command remainder.
#' @param parameters Character vector of formal parameter names. An empty
#'   vector describes a zero-argument function; `"..."` retains token parsing.
#' @return An unnamed character vector of literal arguments with no missing
#'   elements. A blank remainder supplies no arguments, preserving R defaults.
#' @keywords internal
#' @noRd
.custom_command_arguments <- function(raw, parameters) {
  stopifnot("command remainder must be one nonmissing character string" =
              is.character(raw) && length(raw) == 1L && !is.na(raw),
            "formal parameter names must be nonmissing character strings" =
              is.character(parameters) && !anyNA(parameters) && all(nzchar(parameters)))
  text <- trimws(enc2utf8(raw))
  single <- length(parameters) == 1L && parameters[[1L]] != "..."
  out <- if (!nzchar(text)) character() else if (single &&
      !substr(text, 1L, 1L) %in% c('"', "'", "`")) text else {
    tokens <- .custom_command_tokens(text)
    if (single && length(tokens) > 1L) text else tokens
  }
  out <- unname(out)
  stopifnot("custom arguments must be unnamed nonmissing character strings" =
              is.character(out) && !anyNA(out) && is.null(names(out)),
            "a one-parameter command must receive at most one argument" =
              !single || length(out) <= 1L)
  out
}

#' Tokenize Whitespace-Separated Literal Command Arguments
#'
#' Matching uses whole quoted fields and never processes backslash escapes.
#' The anchored pattern must consume the complete input, so partial matches
#' cannot turn malformed quotation into a successful command invocation.
#'
#' @param text One nonempty command remainder, without surrounding whitespace.
#' @return An unnamed character vector of literal fields, with quoted wrappers
#'   removed and all inner characters preserved.
#' @keywords internal
#' @noRd
.custom_command_tokens <- function(text) {
  stopifnot("argument text must be one nonempty character string" =
              is.character(text) && length(text) == 1L && !is.na(text) && nzchar(text))
  pattern <- paste0("\\G(?:^|[ \\t\\r\\n]+)(?:\"[^\"]*\"|'[^']*'|`[^`]*`|",
                    "[^ \\t\\r\\n\"'`][^ \\t\\r\\n]*)")
  matches <- withCallingHandlers(gregexpr(pattern, text, perl = TRUE)[[1L]],
    warning = function(w) stop("Could not parse command arguments: ",
      conditionMessage(w), call. = FALSE))
  last <- length(matches)
  if (matches[[1L]] < 0L || matches[[last]] + attr(matches, "match.length")[[last]] - 1L != nchar(text))
    stop("Malformed command arguments: separate arguments with whitespace and close each quoted argument.",
         call. = FALSE)
  fields <- trimws(regmatches(text, list(matches))[[1L]])
  out <- vapply(fields, function(field) {
    if (substr(field, 1L, 1L) %in% c('"', "'", "`"))
      substr(field, 2L, nchar(field) - 1L) else field
  }, character(1L), USE.NAMES = FALSE)
  stopifnot("parsed fields must be unnamed nonmissing character strings" =
              is.character(out) && !anyNA(out) && is.null(names(out)))
  out
}
