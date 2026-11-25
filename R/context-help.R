#' Read a Help Topic from One Lexical R Name
#' @param token One token returned by the R lexer.
#' @return An unquoted name, or NULL for a non-name token.
#' @keywords internal
#' @noRd
.context_help_name <- function(token) {
  stopifnot("token must be one nonmissing string" = is.character(token) &&
              length(token) == 1L && !is.na(token))
  out <- if (startsWith(token, "`")) {
    parsed <- tryCatch(parse(text = token, keep.source = FALSE), error = function(e) NULL)
    if (length(parsed) == 1L && is.symbol(parsed[[1L]])) as.character(parsed[[1L]]) else NULL
  } else if (stringi::stri_detect_regex(token,
    "^(?:[\\p{L}_]|\\.(?![0-9]))[\\p{L}\\p{M}\\p{N}_.]*$")) token else NULL
  stopifnot("a help name must be NULL or one nonempty string" = is.null(out) || .is_text(out))
  out
}

#' Resolve Local Documentation at a Unicode Source Cursor
#'
#' Resolution is lexical: it never evaluates the source or reads a binding.
#' The character under the cursor takes priority, with the preceding name also
#' accepted at its right edge. Namespaces, backtick names and operators work in
#' incomplete and multiline input. Comments and string contents are excluded.
#' @param text One complete source buffer.
#' @param cursor Zero-based Unicode character offset, as in [complete_r()].
#' @return A list containing topic and optional package, or NULL.
#' @keywords internal
#' @noRd
.context_help_target <- function(text, cursor) {
  stopifnot("text must be one nonmissing string" = is.character(text) &&
              length(text) == 1L && !is.na(text),
            "cursor must be a valid character offset" = .is_count(cursor) &&
              cursor <= nchar(text, type = "chars"))
  if (!nzchar(text) || startsWith(trimws(text), ":")) return(NULL)
  spans <- .r_lex_spans(text)
  tokens <- stringi::stri_sub(text, spans[, 1L], spans[, 2L])
  whitespace <- stringi::stri_detect_regex(tokens, "^\\s+$")
  significant <- which(!whitespace & !startsWith(tokens, "#"))
  names <- lapply(tokens, .context_help_name)
  resolve <- function(index, operators = FALSE) {
    if (!length(index)) return(NULL)
    at <- match(index, significant)
    if (is.na(at)) return(NULL)
    namespace <- if (tokens[[index]] %in% c("::", ":::")) at else
      if (at < length(significant) && tokens[[significant[[at + 1L]]]] %in% c("::", ":::")) at + 1L else
      if (at > 1L && tokens[[significant[[at - 1L]]]] %in% c("::", ":::")) at - 1L else 0L
    if (namespace > 1L && namespace < length(significant)) {
      package <- names[[significant[[namespace - 1L]]]]
      topic <- names[[significant[[namespace + 1L]]]]
      if (!is.null(package) && !is.null(topic)) return(list(topic = topic, package = package))
    }
    topic <- names[[index]]
    if (is.null(topic) && operators && (stringi::stri_detect_regex(tokens[[index]], "^%[^%\\n]*%$") ||
        tokens[[index]] %in% c("+", "-", "*", "/", "^", "~", ":", "!", "<", ">", "=",
          "&", "|", "&&", "||", "==", "!=", "<=", ">=", "<-", "<<-", "->", "->>",
          "|>", "$", "@", "(", "{", "["))) topic <- tokens[[index]]
    if (is.null(topic)) return(NULL)
    if (identical(topic, "[") && ((index < length(tokens) && tokens[[index + 1L]] == "[") ||
        (index > 1L && tokens[[index - 1L]] == "["))) topic <- "[["
    list(topic = topic, package = NULL)
  }
  current <- which(spans[, 1L] <= cursor + 1L & spans[, 2L] >= cursor + 1L)
  previous <- which(spans[, 1L] <= cursor & spans[, 2L] >= cursor)
  out <- resolve(current)
  if (is.null(out) && (!length(current) || whitespace[[current]] ||
      tokens[[current]] %in% c("(", "[", "{", ",", ";", ")", "]", "}"))) out <- resolve(previous)
  if (is.null(out)) out <- resolve(current, operators = TRUE)
  if (is.null(out) && !length(current)) out <- resolve(previous, operators = TRUE)
  stopifnot("context help must contain a topic and optional package" = is.null(out) ||
              (is.list(out) && .is_text(out$topic) && (is.null(out$package) || .is_text(out$package))))
  out
}

#' Stage Help Without Changing the Pending Expression
#' @param state Editor state with a grapheme cursor.
#' @return Editor state with a documentation request or an explanatory hint.
#' @keywords internal
#' @noRd
.editor_request_help <- function(state) {
  stopifnot("state must contain a valid grapheme cursor" = is.list(state) &&
              is.character(state$chars) && !anyNA(state$chars) && .is_count(state$cursor) &&
              state$cursor <= length(state$chars))
  offset <- sum(nchar(utils::head(state$chars, state$cursor), type = "chars"))
  state$help_request <- .context_help_target(paste0(state$chars, collapse = ""), offset)
  state$overlay <- if (is.null(state$help_request))
    "Move the cursor onto a function name, namespace-qualified name or operator to open its help." else character()
  stopifnot("a help request must leave the expression editable" = identical(state$action, "edit"))
  state
}

#' Display Staged Help in the Current Input Session
#' @param state Editor state with a pending help request.
#' @param config Console configuration, or partial editor configuration.
#' @param handle The active terminal input handle, borrowed by the help viewer.
#' @param alternate Use a temporary alternate screen when editing inline.
#' @return Editor state with the request consumed and its draft preserved.
#' @keywords internal
#' @noRd
.editor_open_help <- function(state, config, handle, alternate = FALSE) {
  stopifnot("state and config must be lists" = is.list(state) && is.list(config),
            "alternate must be logical" = .is_flag(alternate),
            "a help request must be pending" = is.list(state$help_request))
  request <- state$help_request
  state$help_request <- NULL
  if (!inherits(config, "prismrepl_config")) {
    defaults <- repl_config(theme = if (is.null(config$theme)) "midnight" else config$theme,
      color = config$color, settings_file = NULL, persist = FALSE)
    config <- utils::modifyList(defaults, config)
    class(config) <- class(defaults)
  }
  config$viewer_handle <- handle
  config$viewer_alternate <- alternate
  problem <- tryCatch({ view_help(request$topic, request$package, config); NULL },
    error = function(e) conditionMessage(e))
  if (!is.null(problem)) state$overlay <- paste0("Cannot open help: ", gsub("[\r\n]", " ", .safe_text(problem)))
  state$clear <- isTRUE(state$clear) || !alternate
  stopifnot("help must leave a valid editable draft" = identical(state$action, "edit") &&
              .is_count(state$cursor) && state$cursor <= length(state$chars) && is.null(state$help_request))
  state
}
