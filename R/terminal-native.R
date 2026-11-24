#' Open the native terminal backend
#' @param mouse Whether to request click, drag, and wheel events for this handle.
#' @return An external pointer, or `NULL` when the terminal is unsupported.
#' @keywords internal
#' @noRd
term_open <- function(mouse = FALSE) {
  stopifnot("mouse must be one non-missing logical" =
              is.logical(mouse) && length(mouse) == 1L && !is.na(mouse))
  handle <- .Call(C_term_open, function(pointer) {
    try(term_close(pointer), silent = TRUE)
  }, mouse)
  stopifnot("terminal handle must be NULL or an external pointer" =
              is.null(handle) || typeof(handle) == "externalptr")
  handle
}

#' Restore terminal modes and release a native handle
#' @param handle A handle returned by `term_open()`.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
term_close <- function(handle) {
  stopifnot("handle must be NULL or an external pointer" =
              is.null(handle) || typeof(handle) == "externalptr")
  result <- .Call(C_term_close, handle)
  stopifnot("closing the terminal must return NULL" = is.null(result))
  invisible(result)
}

#' Read one terminal event without evaluating input
#' @param handle An open terminal handle.
#' @param timeout_ms Maximum idle wait in milliseconds, between zero and 1000.
#' @return A list with `type`, `key`, `text`, `modifiers`, `truncated`, `x`,
#'   `y`, `button`, and `action`. Mouse coordinates are one-based viewport
#'   columns and rows. Other events have missing coordinates and empty actions.
#' @keywords internal
#' @noRd
term_read <- function(handle, timeout_ms = 50L) {
  stopifnot("handle must be an external pointer" = typeof(handle) == "externalptr",
            "timeout_ms must be a finite scalar between zero and 1000" =
              is.numeric(timeout_ms) && length(timeout_ms) == 1L &&
              is.finite(timeout_ms) && timeout_ms >= 0 && timeout_ms <= 1000)
  event <- .Call(C_term_read, handle, as.integer(timeout_ms))
  if (identical(event$type, "timeout")) Sys.sleep(0.001)
  stopifnot("terminal event must have the expected fields" =
              is.list(event) && identical(names(event),
                c("type", "key", "text", "modifiers", "truncated", "x", "y",
                  "button", "action")),
            "terminal event must have a known type" =
              event$type %in% c("key", "text", "paste", "mouse", "timeout", "resize", "eof"))
  event
}

#' Write one assembled Unicode frame to the terminal
#' @param text One non-missing character string, with all cursor movements and
#'   styles already assembled. No newline is added.
#' @param synchronize Wrap the frame in optional synchronized-output sequences.
#'   Enable only when the terminal supports private mode 2026.
#' @param cursor NULL to retain the cursor color, an empty string to restore
#'   a cursor color managed by this session, or a six-digit hex color. Color
#'   changes are cached and emitted with the frame, under output-session ownership.
#' @return Invisibly, the number of UTF-8 payload bytes written. This internal
#'   renderer writes directly to terminal stdout and intentionally bypasses R
#'   output sinks and console callbacks. It requires real terminal output.
#' @keywords internal
#' @noRd
term_write <- function(text, synchronize = FALSE, cursor = NULL) {
  stopifnot("text must be one non-missing character string" =
              is.character(text) && length(text) == 1L && !is.na(text),
            "synchronize must be one non-missing logical" =
              is.logical(synchronize) && length(synchronize) == 1L && !is.na(synchronize),
            "cursor must be NULL, empty, or a six-digit hex color" = is.null(cursor) ||
              (is.character(cursor) && length(cursor) == 1L && !is.na(cursor) &&
                grepl("^$|^#[[:xdigit:]]{6}$", cursor)))
  result <- .Call(C_term_write, enc2utf8(text), synchronize,
                  if (is.null(cursor)) NULL else toupper(cursor))
  stopifnot("written byte count must be one finite non-negative number" =
              is.numeric(result) && length(result) == 1L && is.finite(result) && result >= 0)
  invisible(result)
}

#' Enable terminal output for a whole console session
#' @return An external pointer, or `NULL` when terminal output cannot be enabled.
#' @keywords internal
#' @noRd
term_output_open <- function() {
  handle <- .Call(C_term_output_open, function(pointer) {
    try(term_output_close(pointer), silent = TRUE)
  })
  stopifnot("output handle must be NULL or an external pointer" =
              is.null(handle) || typeof(handle) == "externalptr")
  handle
}

#' Restore output modes and the output code page
#' @param handle A handle returned by `term_output_open()`.
#' @return `NULL`, invisibly. Closing an already closed handle is harmless.
#' @keywords internal
#' @noRd
term_output_close <- function(handle) {
  stopifnot("handle must be NULL or an external pointer" =
              is.null(handle) || typeof(handle) == "externalptr")
  result <- .Call(C_term_output_close, handle)
  stopifnot("closing terminal output must return NULL" = is.null(result))
  invisible(result)
}

#' Inspect actual output settings without changing them
#' @return A named list. Windows mode and code page values are `NA` elsewhere.
#' @keywords internal
#' @noRd
term_output_state <- function() {
  result <- .Call(C_term_output_state)
  stopifnot("terminal output state must contain the expected fields" =
              is.list(result) && identical(names(result), c("supported", "active",
                "vt_stdout", "vt_stderr", "codepage", "stdout_mode", "stderr_mode")))
  result
}

#' Record whether the output session owns the alternate screen
#' @param active Whether the renderer is entering or owns the alternate screen.
#' @return Invisibly, whether a live output session received the ownership flag.
#'   Leaving the screen also restores a cursor color managed by this session.
#' @keywords internal
#' @noRd
term_output_screen <- function(active = TRUE) {
  stopifnot("active must be one non-missing logical" =
              is.logical(active) && length(active) == 1L && !is.na(active))
  result <- .Call(C_term_output_screen, active)
  stopifnot("screen ownership result must be one non-missing logical" =
              is.logical(result) && length(result) == 1L && !is.na(result))
  invisible(result)
}

#' Restore any active native input handle
#' @param output Also restore the session output guard, for final cleanup.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
term_reset <- function(output = FALSE) {
  stopifnot("output must be one non-missing logical" =
              is.logical(output) && length(output) == 1L && !is.na(output))
  result <- .Call(C_term_reset)
  if (output) result <- .Call(C_term_output_reset)
  stopifnot("terminal cleanup must return NULL" = is.null(result))
  invisible(result)
}

#' Get the terminal dimensions
#' @return Named integer vector with `width` and `height`.
#' @keywords internal
#' @noRd
term_size <- function() {
  size <- .Call(C_term_size)
  stopifnot("terminal dimensions must be two positive integers" =
              is.integer(size) && length(size) == 2L && all(size > 0L))
  size
}

#' Inspect terminal capabilities without changing modes
#' @return A list with `supported`, `platform`, `terminal`, and `truecolor`.
#' @keywords internal
#' @noRd
term_capabilities <- function() {
  result <- .Call(C_term_capabilities)
  stopifnot("terminal capabilities must have the expected fields" =
              is.list(result) && identical(names(result),
                c("supported", "platform", "terminal", "truecolor")))
  result
}

#' Ask the R parser whether code is complete
#' @param text One non-missing character string.
#' @return `"complete"`, `"null"`, `"incomplete"`, or `"error"`.
#' @keywords internal
#' @noRd
parse_status <- function(text) {
  stopifnot("text must be one non-missing character string" =
              is.character(text) && length(text) == 1L && !is.na(text))
  status <- .Call(C_parse_status, enc2utf8(text))
  stopifnot("parser status must be recognized" =
              is.character(status) && length(status) == 1L &&
              status %in% c("complete", "null", "incomplete", "error"))
  status
}

#' Inspect binding kinds without evaluating promises or active bindings
#' @param envir An environment.
#' @param names Non-missing object names.
#' @return A named character vector with `"active"`, `"promise"`, `"value"`,
#'   or `"missing"` for each name in the current environment frame.
#' @keywords internal
#' @noRd
binding_info <- function(envir, names) {
  stopifnot("envir must be an environment" = is.environment(envir),
            "names must be a character vector without missing entries" =
              is.character(names) && !anyNA(names))
  result <- .Call(C_binding_info, envir, names)
  stopifnot("binding metadata must match the requested names" =
              is.character(result) && length(result) == length(names) &&
              identical(names(result), names),
            "binding kinds must be recognized" =
              all(result %in% c("active", "promise", "value", "missing")))
  result
}
