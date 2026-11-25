#' Construct a Native Shell Invocation Without Parsing Its Command Text
#'
#' POSIX system() invokes the system shell. On Windows, system2() invokes
#' the command processor explicitly; /s removes exactly the protective outer
#' quote pair around the payload, retaining the user's own shell quotes.
#' /d avoids command-processor AutoRun hooks. No output is captured and no
#' timeout redirects stdin, so foreground tools retain normal terminal I/O.
#'
#' @param command Literal shell command after the colon command and separator.
#' @param windows Use Windows command-processor syntax.
#' @param windows_shell Windows command processor path, normally COMSPEC.
#' @return A namespaced R call containing the literal command as data.
#' @noRd
.shell_call <- function(command, windows = .Platform$OS.type == "windows",
                        windows_shell = Sys.getenv("COMSPEC", unset = "cmd.exe")) {
  stopifnot("use :shell COMMAND (or :sh COMMAND)" = .is_text(command) && nzchar(trimws(command)),
            "windows must be logical" = .is_flag(windows),
            "Windows shell must be a nonmissing string" = is.character(windows_shell) &&
              length(windows_shell) == 1L && !is.na(windows_shell))
  if (!nzchar(windows_shell)) windows_shell <- "cmd.exe"
  out <- if (windows) substitute(base::system2(SHELL,
    args = base::c("/d", "/s", "/c", PAYLOAD), stdout = "", stderr = "", wait = TRUE),
    list(SHELL = windows_shell, PAYLOAD = paste0('"', command, '"'))) else
    substitute(base::system(COMMAND, intern = FALSE, wait = TRUE), list(COMMAND = command))
  stopifnot("shell invocation must be a call" = is.call(out))
  out
}

#' Describe the Result of an Explicit Shell Command
#' @param result Structured evaluation result.
#' @return A text and semantic-role pair for the exit status or interruption.
#' @noRd
.shell_summary <- function(result) {
  stopifnot("shell result must contain evaluation state" = is.list(result) &&
              all(c("value", "error", "interrupted") %in% names(result)) && .is_flag(result$interrupted))
  out <- if (result$interrupted) list(text = "Shell command interrupted.", role = "warning") else
    if (!is.null(result$error)) list(text = "Shell command failed.", role = "error") else {
      status <- result$value
      stopifnot("shell backend must return one finite exit status" = is.numeric(status) &&
                  length(status) == 1L && !is.na(status) && is.finite(status) && status == trunc(status))
      list(text = paste0("Shell exited with status ", format(status, scientific = FALSE, trim = TRUE), "."),
        role = if (status == 0) "message" else "error")
    }
  stopifnot("shell summary must have one text and role" = .is_text(out$text) &&
              out$role %in% c("message", "warning", "error"))
  out
}

#' Keep Foreground Shell Output Visible Before Restoring the Full Screen
#' @return NULL invisibly; an interrupt is handled by the caller's cleanup.
#' @noRd
.shell_pause <- function() {
  if (interactive()) readline("Press Enter to return to Prism (Ctrl+C also returns): ")
  invisible(NULL)
}
