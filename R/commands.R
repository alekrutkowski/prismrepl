#' Describe Console Commands from One Shared Registry
#' @return Command identifiers, syntax, aliases, groups, descriptions and staged text.
#' @noRd
.command_catalog <- function() {
  rows <- list(
    c("help", ":help [TEXT]", "h", "Discover", "Search commands and descriptions; choose one to place it in the input for approval.", ":help "),
    c("keys", ":keys", "k", "Discover", "Browse shortcuts; Enter places the selected :bind command in the input.", ":keys"),
    c("doc", ":doc TOPIC [PACKAGE]", "d", "Discover", "Read styled function documentation; also ?mean or ?stats::lm.", ":doc "),
    c("docs", ":docs PACKAGE", "ds", "Discover", "Browse an installed package's documentation index.", ":docs "),
    c("objects", ":objects [TEXT]", "o", "Workspace", "Explore workspace bindings, optionally filtering their names.", ":objects "),
    c("view", ":view NAME", "v", "Workspace", "Inspect an object's structure, values, attributes and slots.", ":view "),
    c("edit", ":edit NAME", "ed", "Workspace", "Edit a function in a small editor; validate and save explicitly with Ctrl+S.", ":edit "),
    c("rm", ":rm [NAME ...]", "r", "Workspace", "Remove named objects, or select a batch interactively.", ":rm "),
    c("packages", ":packages", "p", "Packages", "List attached packages and loaded namespaces.", ":packages"),
    c("attach", ":attach PACKAGE ...", "a", "Packages", "Attach installed packages in order with library(); stop at the first error. Tab completes each package name.", ":attach "),
    c("detach", ":detach [PACKAGE ...]", "de", "Packages", "Detach named packages, or select a batch; retain namespaces.", ":detach "),
    c("install", ":install PACKAGE ...", "i", "Packages", "Install packages from the configured repositories; Tab uses a cached CRAN index. --refresh refreshes that index.", ":install "),
    c("installgh", ":installgh USER/REPO", "ig", "Packages", "Install a GitHub package with remotes::install_github(); no quotes needed.", ":installgh "),
    c("files", ":files [PATH]", "f", "Files", "Browse directories and preview text files; stage a source command for an R script.", ":files "),
    c("source", ":source PATH", "so", "Files", "Run an R script in this workspace. Unquoted path spaces and single backslashes are preserved.", ":source "),
    c("cd", ":cd [PATH]", "c", "Files", "Change working directory; with no path, show the current directory like :pwd.", ":cd "),
    c("pwd", ":pwd", "pw", "Files", "Show the current working directory.", ":pwd"),
    c("git", ":git", "g", "Files", "Show tracked-file Git status for the current directory.", ":git"),
    c("shell", ":shell COMMAND", "sh", "Files", "Run a system shell command without an R string; preserve shell quotes, pipes and redirects.", ":shell "),
    c("history", ":history [TEXT]", "hi", "History and errors", "Browse submitted R expressions and colon commands; optionally filter literal text.", ":history "),
    c("recall", ":recall NUMBER", "re", "History and errors", "Place a numbered history entry in the input for editing and approval.", ":recall "),
    c("trace", ":trace", "tr", "History and errors", "Show the most recent error's captured call trace.", ":trace"),
    c("explain", ":explain [copy] [minimal]", "e", "History and errors", "Show an AI explanation link, or copy its prompt; minimal omits source and call trace.", ":explain "),
    c("theme", ":theme [NAME|auto]", "t", "Appearance", "Choose a theme or desktop appearance; Enter applies, saves, and repaints.", ":theme "),
    c("ui", ":ui [fullscreen|inline]", "ui", "Appearance", "Choose the full-screen interface or inline editing; Enter applies for this session.", ":ui "),
    c("sidebar", ":sidebar [on|off]", "si", "Appearance", "Choose whether the WORKSPACE, PACKAGES and FILES pane is shown in this session.", ":sidebar "),
    c("status", ":status [FIELD ...]", "s", "Appearance", "Select status fields with Space, then Enter; explicit fields or none are also accepted.", ":status "),
    c("clear", ":clear", "cl", "Appearance", "Clear the retained transcript or terminal display.", ":clear"),
    c("pipe", ":pipe [|> | %>%]", "pi", "Preferences", "Choose with Enter and remember the operator inserted by the pipe shortcut.", ":pipe "),
    c("bind", ":bind [ACTION [KEY ...]]", "b", "Preferences", "Choose an action to stage its remapping command; explicit keys save it, and reset restores defaults.", ":bind "),
    c("unbind", ":unbind [ACTION]", "u", "Preferences", "Choose an action with Enter to disable its shortcuts and save the preference.", ":unbind "),
    c("mouse", ":mouse [on|off]", "m", "Preferences", "Choose and remember mouse clicks, dragging, copying, pasting and scrolling.", ":mouse "),
    c("rainbow", ":rainbow [on|off|toggle]", "ra", "Preferences", "Choose and remember matching-parenthesis, bracket, brace and quote colors.", ":rainbow "),
    c("autopair", ":autopair [on|off|toggle]", "au", "Preferences", "Choose and remember automatic matching delimiters and generated-closer skipping.", ":autopair "),
    c("match", ":match [on|off|toggle]", "ma", "Preferences", "Choose and remember highlighting both matching brackets or quotes at the cursor, including raw strings.", ":match "),
    c("ai", ":ai [PROVIDER|custom URL]", "ai", "Preferences", "Choose and save the error-explanation provider with Enter; Custom can stage a URL for editing.", ":ai "),
    c("settings", ":settings [reset]", "se", "Preferences", "Choose a preference to edit; URL setup and reset commands are placed in the input for approval.", ":settings "),
    c("doctor", ":doctor", "dr", "Session", "Show terminal, color, clipboard and platform diagnostics.", ":doctor"),
    c("quit", ":quit", "q", "Session", "Return to the ordinary R prompt without quitting R.", ":quit")
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("id", "command", "alias", "group", "description", "insert")
  stopifnot("command names and aliases must be unique" = !anyDuplicated(out$id) &&
              !anyDuplicated(out$alias),
            "aliases must use one or two letters" = all(grepl("^[a-z]{1,2}$", out$alias)),
            "aliases must not shadow other commands" = all(!out$alias %in% out$id | out$alias == out$id),
            "every command must have descriptive staged text" = !anyNA(out) &&
              all(nzchar(out$description)) && all(startsWith(out$insert, ":")))
  out
}

#' Parse a Console Command Without Interpreting Path Escapes
#' @param text One command string.
#' @return The canonical identifier, argument tokens and literal remainder.
#' @noRd
.command_parse <- function(text) {
  stopifnot("text must be a nonempty command" = .is_text(text))
  match <- regexec("^[[:blank:]]*:([[:alpha:]][[:alnum:]]*)(?:[[:blank:]]+([\\s\\S]*))?$", text, perl = TRUE)
  parts <- regmatches(text, match)[[1L]]
  stopifnot("use a colon followed by a command name" = length(parts) >= 2L)
  name <- tolower(parts[[2L]])
  registry <- .command_catalog()
  index <- match(name, registry$id)
  if (is.na(index)) index <- match(name, registry$alias)
  cmd <- if (is.na(index)) name else registry$id[[index]]
  raw <- if (length(parts) >= 3L) parts[[3L]] else ""
  if (cmd != "shell") raw <- trimws(raw)
  path_command <- cmd %in% c("source", "cd", "files")
  args <- if (!nzchar(raw)) character() else if (cmd == "shell") raw else if (path_command) .command_path(raw) else
    scan(text = raw, what = character(), quiet = TRUE, quote = "\"'`", comment.char = "", allowEscapes = FALSE)
  out <- list(command = cmd, args = args, raw = raw)
  stopifnot("parsed command must contain literal arguments" = .is_text(out$command) &&
              is.character(out$args) && !anyNA(out$args))
  out
}

#' Keep Spaces and Backslashes in a Literal File Argument
#' @param text The entire command remainder, optionally enclosed in matching quotes.
#' @return One path without evaluating escapes or expressions.
#' @noRd
.command_path <- function(text) {
  stopifnot("path must be nonempty scalar text" = .is_text(text),
            "a file path must not contain a newline" = !grepl("[\r\n]", text))
  out <- trimws(text)
  first <- substr(out, 1L, 1L)
  if (nchar(out) >= 2L && first %in% c("'", '"', "`") && endsWith(out, first))
    out <- substr(out, 2L, nchar(out) - 1L)
  stopifnot("path must not be empty" = .is_text(out))
  out
}

#' Retain Submitted Expressions and Colon Commands Together
#' @param history Existing complete entries.
#' @param text Submitted text.
#' @param limit Maximum entries.
#' @return Deduplicated, bounded history, newest entry last.
#' @noRd
.history_submit <- function(history, text, limit) {
  stopifnot("history must be text" = is.character(history) && !anyNA(history),
            "submission must be text" = .is_text(text), "limit must be positive" = .is_count(limit, 1L))
  out <- utils::tail(c(history[history != text], text), limit)
  stopifnot("history must end in the submitted entry" = length(out) <= limit &&
              identical(utils::tail(out, 1L), text))
  out
}

#' Evaluate an Alias Through the Ordinary Condition and Output Pipeline
#' @param call A language object constructed from literal command arguments.
#' @param state Console session state.
#' @param envir Evaluation environment.
#' @param external Restore the ordinary terminal while installers or their child
#'   processes run, then return to the full-screen interface.
#' @param operation Package operation or shell command; shell commands report
#'   their exit status and pause before returning to the full-screen interface.
#' @return The evaluation result invisibly.
#' @noRd
.command_evaluate <- function(call, state, envir, external = FALSE, operation = "package") {
  stopifnot("command must be an R call" = is.call(call),
            "state and evaluation environment must be environments" = is.environment(state) && is.environment(envir),
            "external must be logical" = .is_flag(external),
            "operation must be package or shell" = .is_text(operation) && operation %in% c("package", "shell"))
  code <- paste(deparse(call, width.cutoff = 100L), collapse = "\n")
  config <- state$config
  result <- report <- NULL
  suspended <- external && is.environment(config$screen) && isTRUE(config$screen$active)
  if (suspended) {
    screen_leave(config$screen)
    on.exit({
      if (operation == "shell" && is.list(result)) {
        state$config$screen$mode <- if (isTRUE(result$interrupted)) "INTERRUPTED" else
          if (!is.null(result$error) || identical(report$role, "error")) "ERROR" else
            if (length(result$warnings)) "WARNING" else "READY"
        state$config$screen$elapsed <- result$elapsed
      }
      screen_enter(state$config$screen, state$config)
      screen_invalidate(state$config$screen)
      if (!is.null(result$error)) .show_error(result, state$config) else if (!is.null(report))
        .emit(report$text, report$role, state$config) else if (operation == "package")
          .emit("Package operation returned. Its full output is in the terminal scrollback.",
            if (length(result$warnings)) "warning" else "message", state$config)
    }, add = TRUE)
    config$screen <- NULL
    config$output_hook <- NULL
    .emit(if (operation == "shell") "Running the shell command in the ordinary terminal." else
      "Running the package installer; the full-screen console resumes when it returns.", "message", config)
  }
  result <- .evaluate_session(code, envir, config)
  state$last <- result
  if (!is.null(state$last$error)) state$last_error <- state$last
  if (operation == "shell") {
    report <- .shell_summary(result)
    if (is.null(result$error)) .emit(report$text, report$role, config)
    if (suspended && !isTRUE(result$interrupted))
      tryCatch(.shell_pause(), interrupt = function(e) invisible(NULL))
  }
  stopifnot("command evaluation must have a result" = is.list(state$last) && "error" %in% names(state$last))
  invisible(state$last)
}
