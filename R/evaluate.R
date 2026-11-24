#' Evaluate R Code with Structured Conditions
#'
#' Runs expressions sequentially in the supplied environment. An error stops
#' the remaining expressions, and existing successful assignments are retained.
#' The console restores normal terminal input before calling this function, so
#' `readline()`, `menu()`, debuggers, and interactive plotting can use R's console.
#' All evaluation and display happen on R's main thread.
#' Warnings are recorded in the result and displayed immediately. They do not
#' populate base R's deferred `warnings()` buffer.
#'
#' @param code Nonmissing character source code.
#' @param envir Environment in which to evaluate the code.
#' @param config A [repl_config()] object.
#' @param display Display visible results and conditions.
#' @return A list containing `value`, `visible`, `error`, `warnings`, `messages`,
#'   `trace`, `elapsed`, `interrupted`, and `code`. Output from explicit `print()`
#'   and `cat()` calls goes to the normal R output connection, including when
#'   `display = FALSE`. No console-output sinks are installed.
#' @export
#' @examples
#' env <- new.env(parent = baseenv())
#' result <- evaluate_r("x <- 2; x + 3", env, display = FALSE)
#' result$value
#' result <- evaluate_r("stop('example')", env, display = FALSE)
#' conditionMessage(result$error)
evaluate_r <- function(code, envir = .GlobalEnv, config = repl_config(), display = TRUE) {
  stopifnot("code must be nonmissing character text" = is.character(code) && !anyNA(code),
            "envir must be an environment" = is.environment(envir),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "display must be a logical scalar" = .is_flag(display))
  source <- paste(code, collapse = "\n")
  started <- proc.time()[["elapsed"]]
  result <- list(value = NULL, visible = FALSE, error = NULL, warnings = list(),
    messages = list(), trace = character(), elapsed = 0, interrupted = FALSE, code = source)
  depth <- sys.nframe()
  active_depth <- depth
  record_error <- function(e) {
    result$trace <<- .trace_calls(sys.calls(), active_depth, config$trace_limit)
  }
  warning_handler <- function(w) {
    warn <- getOption("warn", 0L)
    if (warn >= 2L) stop(simpleError(paste0("(converted from warning) ", conditionMessage(w)),
                                  call = conditionCall(w)))
    if (warn >= 0L) {
      result$warnings[[length(result$warnings) + 1L]] <<- w
      if (display) .emit(conditionMessage(w), "warning", config)
    }
    restart <- findRestart("muffleWarning")
    if (!is.null(restart)) invokeRestart(restart)
  }
  message_handler <- function(m) {
    result$messages[[length(result$messages) + 1L]] <<- m
    if (display) .emit(sub("\n$", "", conditionMessage(m)), "message", config)
    restart <- findRestart("muffleMessage")
    if (!is.null(restart)) invokeRestart(restart)
  }
  tryCatch(withCallingHandlers({
    expressions <- parse(text = source, keep.source = TRUE)
    invisible(lapply(expressions, function(expr) {
      active_depth <<- sys.nframe()
      answer <- withVisible(eval(expr, envir = envir))
      result["value"] <<- list(answer$value)
      result$visible <<- answer$visible
      last_exists <- exists(".Last.value", envir, inherits = FALSE)
      if ((!last_exists && !environmentIsLocked(envir)) ||
          (last_exists && !bindingIsLocked(".Last.value", envir) &&
             !bindingIsActive(".Last.value", envir)))
        assign(".Last.value", answer$value, envir = envir)
      if (display && answer$visible) {
        bounded <- identical(config$preview, "auto") &&
          (!is.object(answer$value) || identical(class(answer$value), "data.frame") ||
             inherits(answer$value, c("factor", "Date", "POSIXt", "difftime")))
        if (bounded) {
          .emit(preview_object(answer$value, width = .display_width(config), rows = config$rows),
                "output", config)
        } else eval(quote(base::print(.prism_value)), list(.prism_value = answer$value), envir)
      }
      NULL
    }))
  }, warning = warning_handler, message = message_handler, error = record_error),
  error = function(e) { result$error <<- e },
  interrupt = function(e) { result$interrupted <<- TRUE })
  result$elapsed <- max(0, proc.time()[["elapsed"]] - started)
  if (display && result$interrupted) .emit("Interrupted.", "warning", config)
  if (display && !is.null(result$error)) .show_error(result, config)
  stopifnot("evaluation output must contain all result fields" = is.list(result) &&
              all(c("value", "visible", "error", "warnings", "messages", "trace", "elapsed",
                    "interrupted", "code") %in% names(result)),
            "result visibility and interruption must be logical" =
              .is_flag(result$visible) && .is_flag(result$interrupted),
            "elapsed time must be nonnegative" = is.finite(result$elapsed) && result$elapsed >= 0)
  result
}

#' Determine a Display Width
#' @param config Console configuration.
#' @return A positive integer width.
#' @keywords internal
#' @noRd
.display_width <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  automatic <- if (is.environment(config$screen) && isTRUE(config$screen$active))
    screen_content_width(config$screen, config) else term_size()[["width"]] - 2L
  out <- if (is.null(config$width)) max(20L, automatic) else config$width
  out <- min(1000L, as.integer(out))
  stopifnot("display width must be at least 20" = .is_count(out, 20L))
  out
}
