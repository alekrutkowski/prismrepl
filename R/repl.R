#' Start the Prism Interactive Console
#'
#' The console runs inside the current R process and evaluates code in `envir`.
#' Enter `:help` for commands, `:keys` or F2 for shortcuts, and `:quit` to return
#' to the ordinary R prompt. Native terminal mode is used only for interactive
#' terminal input; `mode = "basic"` uses the host console's line editor.
#' Supported terminals use a responsive full-screen layout by default. Set
#' `config = repl_config(ui = "inline")` to retain the terminal's own scrollback.
#'
#' @param envir Environment used for evaluation and workspace commands.
#' @param config A configuration returned by [repl_config()].
#' @param mode One of `"auto"`, `"terminal"`, or `"basic"`.
#' @return Invisibly, a list with session history, the last evaluation result,
#'   and the most recent error result.
#' @export
#' @examples
#' if (interactive()) repl()
#' if (interactive()) repl(config = repl_config(theme = "paper", pipe = "%>%"))
repl <- function(envir = .GlobalEnv, config = repl_config(), mode = "auto") {
  stopifnot("envir must be an environment" = is.environment(envir),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "mode must be auto, terminal, or basic" = .is_text(mode) &&
              mode %in% c("auto", "terminal", "basic"))
  if (!interactive()) stop("repl() needs interactive R. Use evaluate_r() to evaluate code in scripts.",
                           call. = FALSE)
  state <- new.env(parent = emptyenv())
  state$config <- config
  state$history <- if (is.null(config$history_file)) character() else
    repl_history(config$history_file, config$history_limit)
  state$last <- NULL
  state$last_error <- NULL
  state$initial <- ""
  state$running <- TRUE
  state$native <- FALSE
  output_handle <- NULL
  on.exit({
    if (is.environment(state$config$screen)) try(screen_leave(state$config$screen), silent = TRUE)
    try(term_reset(), silent = TRUE)
    try(term_output_close(output_handle), silent = TRUE)
    try(.cleanup_cran_workers(wait = TRUE), silent = TRUE)
    if (!is.null(state$config$history_file)) tryCatch(
      save_repl_history(state$history, state$config$history_file, state$config$history_limit),
      error = function(e) warning(paste("History was not saved:", conditionMessage(e)), call. = FALSE))
  }, add = TRUE)
  caps <- term_capabilities()
  if (isTRUE(caps$supported)) output_handle <- term_output_open()
  modern_windows <- nzchar(Sys.getenv("WT_SESSION")) || nzchar(Sys.getenv("WT_PROFILE_ID"))
  native <- mode != "basic" && isTRUE(caps$supported) && !is.null(output_handle) &&
    (!identical(Sys.getenv("TERM"), "dumb") || modern_windows) &&
    !identical(Sys.getenv("RSTUDIO"), "1")
  if ((mode == "terminal" || identical(config$ui, "fullscreen")) && !native)
    stop("Full terminal controls are unavailable. In Windows Terminal, start Rterm.exe; otherwise use mode = 'basic' and ui = 'inline'.",
         call. = FALSE)
  state$native <- native
  state$config <- .resolve_console_config(config, caps)
  state$config$hyperlinks <- isTRUE(config$hyperlinks) && isTRUE(caps$supported) &&
    (!identical(Sys.getenv("TERM"), "dumb") || modern_windows)
  if (native && !identical(config$ui, "inline")) .set_console_layout(state, "fullscreen")
  cfg <- state$config
  if (isTRUE(cfg$theme_initial)) {
    .remember_initial_theme(cfg)
    state$config$theme_initial <- FALSE
  }
  .emit(c("prismrepl 0.6.5", ":help commands  :keys shortcuts  :quit return to R"), "heading", cfg)
  if (!native) .emit("Basic console mode. Editing is provided by the host R console.", "muted", cfg)
  repeat {
    if (!state$running) break
    cfg <- state$config
    fullscreen <- is.environment(cfg$screen) && isTRUE(cfg$screen$active)
    inventory <- if ((fullscreen && isTRUE(cfg$sidebar)) ||
                     any(c("objects", "memory") %in% cfg$status))
      tryCatch(workspace_objects(envir, sizes = "memory" %in% cfg$status),
               error = function(e) NULL) else NULL
    process_memory <- if ("process_memory" %in% cfg$status) .process_memory() else NULL
    status <- tryCatch(.session_status(envir, cfg, inventory, process_memory), error = function(e) character())
    if (is.environment(cfg$screen) && isTRUE(cfg$screen$active)) {
      cfg$screen$status <- status
      cfg$screen$sidebar <- tryCatch(.session_sidebar(envir, cfg, inventory, process_memory), error = function(e)
        list(WORKSPACE = paste("Inspection unavailable:", conditionMessage(e))))
    }
    cfg$initial_text <- state$initial
    state$initial <- ""
    entry <- tryCatch(if (native) read_expression(history = state$history, envir = envir,
      config = cfg, status = status) else .read_basic(cfg, status),
      interrupt = function(e) list(action = "cancel", text = ""),
      error = function(e) {
        term_reset()
        .emit(paste0("Editor error: ", conditionMessage(e)), "error", cfg)
        list(action = "eof", text = "")
      })
    if (entry$action == "eof") break
    if (entry$action == "cancel" || !nzchar(trimws(entry$text))) next
    text <- entry$text
    if (is.environment(cfg$screen) && isTRUE(cfg$screen$active)) {
      code_lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
      colored <- .highlight_r_window(text, 1L, length(code_lines), theme = cfg$theme,
                                      color = cfg$color, rainbow = cfg$rainbow)
      prefixes <- c("r> ", rep("+  ", max(0L, length(colored) - 1L)))
      styled_input <- paste0(style_text(prefixes, "prompt", cfg$theme, cfg$color), colored)
      attr(styled_input, "prism_source") <- list(text = text, prefixes = prefixes)
      screen_append(cfg$screen, styled_input, "input", ansi = TRUE)
      cfg$screen$last_editor <- NULL
    }
    state$history <- .history_submit(state$history, text, cfg$history_limit)
    if (startsWith(trimws(text), ":")) {
      tryCatch(.repl_command(text, state, envir),
        error = function(e) .emit(paste0("Command error: ", conditionMessage(e)), "error", cfg),
        interrupt = function(e) .emit("Cancelled.", "muted", cfg))
      next
    }
    topic <- .help_query(text)
    if (!is.null(topic)) {
      tryCatch(view_help(topic, config = cfg), error = function(e)
        .emit(paste0("Help: ", conditionMessage(e)), "warning", cfg))
    } else {
      state$last <- .evaluate_session(text, envir, cfg)
      if (!is.null(state$last$error)) state$last_error <- state$last
    }
  }
  out <- list(history = state$history, last = state$last, last_error = state$last_error)
  stopifnot("session output must contain history and last result" = is.list(out) &&
              is.character(out$history) && all(c("history", "last") %in% names(out)))
  invisible(out)
}

#' Switch the Active Console Layout Without Restarting R
#' @param state Session environment.
#' @param ui Fullscreen or inline.
#' @return NULL invisibly.
#' @keywords internal
#' @noRd
.set_console_layout <- function(state, ui) {
  stopifnot("state must be an environment" = is.environment(state),
            "ui must be fullscreen or inline" = .is_text(ui) && ui %in% c("fullscreen", "inline"))
  if (ui == "fullscreen") {
    stopifnot("fullscreen requires a supported native terminal" = isTRUE(state$native))
    if (!is.environment(state$config$screen))
      state$config$screen <- screen_state(max_lines = state$config$transcript_limit)
    state$config$output_hook <- function(lines, role) {
      screen_append(state$config$screen, lines, role, ansi = TRUE)
      if (isTRUE(state$config$screen$active)) screen_render(state$config$screen, config = state$config)
      invisible(lines)
    }
    screen_enter(state$config$screen, state$config)
  } else {
    if (is.environment(state$config$screen)) screen_leave(state$config$screen)
    state$config$output_hook <- NULL
  }
  state$config$ui <- ui
  invisible(NULL)
}

#' Recognize a Single Literal Help Query
#' @param text Source text.
#' @return A help topic or NULL.
#' @keywords internal
#' @noRd
.help_query <- function(text) {
  stopifnot("text must be a character scalar" = is.character(text) && length(text) == 1L && !is.na(text))
  expressions <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) NULL)
  if (length(expressions) != 1L) return(NULL)
  expr <- expressions[[1L]]
  if (!is.call(expr) || length(expr) != 2L || !identical(expr[[1L]], as.name("?"))) return(NULL)
  target <- expr[[2L]]
  out <- if (is.name(target) || (is.character(target) && length(target) == 1L)) as.character(target)
  else if (is.call(target) && length(target) == 3L && identical(target[[1L]], as.name("::")) &&
           is.name(target[[2L]]) && is.name(target[[3L]]))
    paste0(as.character(target[[2L]]), "::", as.character(target[[3L]])) else NULL
  stopifnot("help query must be NULL or one topic" = is.null(out) || .is_text(out))
  out
}

#' Read an Expression with the Host Console
#' @param config Console configuration.
#' @param status Status lines.
#' @return A submission or cancellation record.
#' @keywords internal
#' @noRd
.read_basic <- function(config, status = character()) {
  stopifnot("config must be a configuration" = inherits(config, "prismrepl_config"),
            "status must be character" = is.character(status))
  if (length(status)) .emit(status, "muted", config)
  read_line <- function(prompt) {
    cat(prompt)
    flush.console()
    value <- readLines(stdin(), n = 1L, warn = FALSE)
    if (length(value)) value[[1L]] else NULL
  }
  lines <- if (is.null(config$initial_text)) "" else config$initial_text
  if (nzchar(lines)) {
    .emit(lines, "input", config)
    .emit("Recalled expression shown above. Enter to execute; Ctrl+C to cancel.", "muted", config)
    extra <- read_line("r> ")
    if (is.null(extra)) return(list(action = "eof", text = ""))
    if (nzchar(extra)) lines <- extra
  } else lines <- read_line("r> ")
  if (is.null(lines)) return(list(action = "eof", text = ""))
  repeat {
    if (startsWith(trimws(lines), ":") || parse_status(lines) != "incomplete") break
    extra <- read_line("+  ")
    if (is.null(extra)) return(list(action = "eof", text = lines))
    lines <- paste(lines, extra, sep = "\n")
  }
  out <- list(action = "submit", text = lines)
  stopifnot("basic input must be a submission" = identical(out$action, "submit") &&
              is.character(out$text) && length(out$text) == 1L)
  out
}

#' Collect Explicitly Enabled Session Status
#' @param envir Evaluation environment.
#' @param config Configuration.
#' @param inventory Optional shared workspace snapshot for this refresh.
#' @param process_memory Optional resident-byte snapshot shared with the sidebar.
#' @return Character lines, refreshed once per submitted expression.
#' @keywords internal
#' @noRd
.session_status <- function(envir, config, inventory = NULL, process_memory = NULL) {
  stopifnot("envir must be an environment" = is.environment(envir),
            "config must be a configuration" = inherits(config, "prismrepl_config"),
            "inventory must be NULL or a data frame" = is.null(inventory) || is.data.frame(inventory))
  fields <- config$status
  if (is.null(inventory) && any(c("objects", "memory") %in% fields))
    inventory <- workspace_objects(envir, sizes = "memory" %in% fields)
  out <- c(
    if ("process_memory" %in% fields) .process_memory_label(
      if (is.null(process_memory)) .process_memory() else process_memory),
    if ("directory" %in% fields) paste0("cwd: ", getwd()),
    if ("git" %in% fields) {
      info <- git_info()
      if (isTRUE(info$is_repo)) {
        if (nzchar(info$error)) "git: status unavailable" else paste0("git: ", info$branch,
          if (isTRUE(info$dirty)) " *" else "", "  +", info$ahead, "/", info$behind)
      }
    },
    if ("objects" %in% fields) paste0("objects: ", nrow(inventory)),
    if ("packages" %in% fields) paste0("packages: ", sum(startsWith(search(), "package:")), " attached"),
    if ("memory" %in% fields) paste0("objects ~", format(structure(
      sum(inventory$bytes, na.rm = TRUE), class = "object_size"), units = "auto"),
      if (anyNA(inventory$bytes)) " (partial)" else ""))
  out <- if (length(out)) paste(out, collapse = "  |  ") else character()
  stopifnot("status must be character" = is.character(out) && !anyNA(out))
  out
}

#' Console Command Reference
#' @param details Include aliases, groups, identifiers and staged command text.
#' @param commands_file Custom registry path, or NULL for session-only commands.
#' @return A data frame of commands and descriptions. With `details = TRUE`,
#'   returns the complete command registry used by the palette and completion.
#' @export
#' @examples
#' repl_commands()
#' repl_commands(details = TRUE)
repl_commands <- function(details = FALSE, commands_file = repl_commands_file()) {
  stopifnot("details must be logical" = .is_flag(details))
  catalog <- .command_catalog(commands_file)
  out <- if (details) catalog else catalog[c("command", "description")]
  stopifnot("command reference must contain matching descriptions" = is.data.frame(out) &&
              nrow(out) > 0L && !anyNA(out$description))
  out
}

#' Format a Plain Data Frame for a Panel
#' @param x A data frame.
#' @return Text lines.
#' @keywords internal
#' @noRd
.table_lines <- function(x) {
  stopifnot("x must be a data frame" = is.data.frame(x))
  out <- .table_format(x, width = max(20L, term_size()[["width"]] - 4L))$lines
  stopifnot("table text must be character" = is.character(out))
  .safe_text(out)
}

#' Handle a Local Console Command
#' @param text The command text.
#' @param state Session state.
#' @param envir Evaluation environment.
#' @return NULL invisibly.
#' @keywords internal
#' @noRd
.repl_command <- function(text, state, envir) {
  stopifnot("text must be one command" = .is_text(text),
            "state and envir must be environments" = is.environment(state) && is.environment(envir))
  parsed <- .command_parse(text, state$config$commands_file)
  cmd <- parsed$command
  args <- parsed$args
  cfg <- state$config
  need <- function(n) stopifnot("wrong number of command arguments; see :help" = length(args) == n)
  panel <- function(lines, title) .tui_panel(lines, title, cfg)
  switch(cmd,
    help = {
      staged <- .command_help(cfg, query = paste(args, collapse = " "))
      if (.is_text(staged)) state$initial <- staged
    },
    keys = .binding_menu(state, title = "Keyboard shortcuts"),
    commands = {
      catalog <- .custom_command_catalog(cfg$commands_file)
      if (!nrow(catalog)) .emit(c("No custom commands registered for this configuration.",
        "Add one with prismrepl::repl_add_command(\"name\", function(text) ...)."), "message", cfg)
      else {
        staged <- .command_palette(cfg, query = paste(args, collapse = " "), catalog = catalog)
        if (.is_text(staged)) state$initial <- staged
      }
    },
    doc = {
      stopifnot("use :doc TOPIC [PACKAGE]" = length(args) %in% 1:2)
      view_help(args[1L], if (length(args) == 2L) args[2L] else NULL, cfg)
    },
    docs = { need(1L); view_help(NULL, args[1L], cfg) },
    objects = {
      view_object(envir, cfg, label = "Workspace", filter = paste(args, collapse = " "))
    },
    view = {
      need(1L)
      stopifnot("object must be an ordinary local binding" = identical(unname(binding_info(envir, args)), "value"))
      view_object(get(args[1L], envir, inherits = FALSE), cfg, label = args[1L])
    },
    edit = { need(1L); .edit_function_command(args[[1L]], envir, cfg) },
    rm = .batch_action("objects", args, state, envir),
    packages = .tui_table(loaded_packages(), "Packages and namespaces", cfg),
    detach = .batch_action("packages", args, state, envir),
    attach = {
      stopifnot("use :attach PACKAGE ... with package names separated by spaces" =
                  length(args) >= 1L && all(nzchar(args)))
      calls <- lapply(unique(args), function(package)
        substitute(base::library(PACKAGE, character.only = TRUE), list(PACKAGE = package)))
      call <- if (length(calls) == 1L) calls[[1L]] else
        as.call(c(list(quote(base::`{`)), calls))
      .command_evaluate(call, state, envir)
    },
    install = {
      stopifnot("use :install PACKAGE ... or :install --refresh" = length(args) >= 1L)
      if (identical(args, "--refresh")) {
        index <- .cran_package_names(refresh = TRUE)
        .emit(index$status, "message", cfg)
      } else {
        stopifnot("supply package names separated by spaces" = all(grepl("^[A-Za-z][A-Za-z0-9.]*$", args)))
        .command_evaluate(substitute(utils::install.packages(PACKAGES, repos = REPOS),
          list(PACKAGES = unique(args), REPOS = .installation_repositories())), state, envir, external = TRUE)
      }
    },
    installgh = {
      need(1L)
      stopifnot("use :installgh user/repo (optionally with @ref or a subdirectory)" =
        grepl("^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+(?:[/@#][^[:space:]]+)?$", args[[1L]], perl = TRUE))
      if (!requireNamespace("remotes", quietly = TRUE))
        stop("The remotes package is needed. Run :install remotes, then retry :installgh.", call. = FALSE)
      .command_evaluate(substitute(remotes::install_github(REPO),
        list(REPO = args[[1L]])), state, envir, external = TRUE)
    },
    status = {
      if (!length(args)) args <- .status_choice(cfg)
      if (is.null(args)) return(invisible(NULL))
      if (identical(args, "none")) args <- character()
      stopifnot("unknown status field" = all(args %in% c("process_memory", "directory", "git", "objects", "packages", "memory")))
      state$config$status <- unique(args)
    },
    theme = {
      selected <- if (!length(args)) .theme_picker(cfg) else {
        need(1L)
        if (identical(args[[1L]], "auto")) .initial_theme(refresh = TRUE) else args[[1L]]
      }
      if (!is.null(selected)) {
        if (identical(selected, "auto")) selected <- .initial_theme(refresh = TRUE)
        state$config$theme <- repl_theme(selected, color_depth = cfg$color_depth)
        state$config$theme_initial <- FALSE
        if (is.environment(cfg$screen)) screen_invalidate(cfg$screen)
        saved <- .persist_preferences(state$config, "theme")
        .emit(paste0("Theme: ", selected, if (saved) " (saved)" else " (this session)"),
              "message", state$config)
        if (is.environment(cfg$screen) && isTRUE(cfg$screen$active))
          screen_render(cfg$screen, config = state$config)
      }
    },
    pipe = {
      if (!length(args)) args <- .pipe_choice(cfg)
      if (is.null(args)) return(invisible(NULL))
      need(1L); stopifnot("pipe must be |> or %>%" = args[1L] %in% c("|>", "%>%"))
      state$config$pipe <- args[1L]
      .persist_preferences(state$config, "pipe")
    },
    bind = .bind_command(args, state),
    unbind = {
      if (!length(args)) args <- .binding_choice(cfg, title = "Disable a shortcut action", reset = FALSE)
      if (is.null(args)) return(invisible(NULL))
      need(1L)
      .bind_command(c(args, "none"), state)
    },
    settings = {
      stopifnot("use :settings or :settings reset" = !length(args) || identical(args, "reset"))
      if (length(args)) {
        defaults <- list(theme = repl_theme(.initial_theme(refresh = TRUE), color_depth = cfg$color_depth),
          pipe = "|>", mouse = TRUE, rainbow = TRUE, autopair = TRUE, match_pairs = TRUE,
          ai_provider = "mistral", ai_url = NULL, keybindings = repl_keybindings(), theme_initial = FALSE)
        state$config[names(defaults)] <- defaults
        state$config$key_lookup <- .key_lookup(state$config$keybindings)
        if (is.environment(cfg$screen)) screen_invalidate(cfg$screen)
        .persist_preferences(state$config, c("theme", "pipe", "mouse", "keybindings",
          "rainbow", "autopair", "match_pairs", "ai_provider", "ai_url"), reset = TRUE)
        .emit("Appearance, editing, AI provider, shortcuts, pipe and mouse preferences reset.", "message", state$config)
        if (is.environment(cfg$screen) && isTRUE(cfg$screen$active))
          screen_render(cfg$screen, config = state$config)
      } else .preferences_menu(state, envir)
    },
    mouse = {
      if (!length(args)) args <- .toggle_choice("mouse", "Mouse interaction", cfg$mouse, cfg)
      if (is.null(args)) return(invisible(NULL))
      need(1L)
      stopifnot("mouse must be on or off" = args[1L] %in% c("on", "off"))
      state$config$mouse <- identical(args[1L], "on")
      .persist_preferences(state$config, "mouse")
      .emit(paste("Mouse", args[1L]), "message", state$config)
    },
    rainbow = .editing_preference_command("rainbow", args, state),
    autopair = .editing_preference_command("autopair", args, state),
    match = .editing_preference_command("match_pairs", args, state),
    ai = .ai_command(args, state),
    history = {
      entries <- state$history
      ids <- seq_along(entries)
      if (length(args)) ids <- ids[grepl(paste(args, collapse = " "), entries, fixed = TRUE)]
      panel(unlist(lapply(ids, function(i) c(paste0("[", i, "]"), strsplit(entries[i], "\n", fixed = TRUE)[[1L]], "")),
                   use.names = FALSE), "Command history")
    },
    recall = {
      need(1L); n <- suppressWarnings(as.integer(args[1L]))
      stopifnot("history number is out of range" = .is_count(n, 1L) && n <= length(state$history))
      state$initial <- state$history[n]
    },
    trace = panel(if (length(state$last_error$trace)) state$last_error$trace else "No captured trace.", "Last error trace"),
    explain = .explain_command(args, state),
    files = {
      stopifnot("use :files or :files PATH" = length(args) <= 1L)
      staged <- .file_explorer(if (length(args)) args[[1L]] else getwd(), cfg)
      if (.is_text(staged)) state$initial <- staged
    },
    source = {
      need(1L)
      .command_evaluate(substitute(base::source(PATH, local = TRUE),
        list(PATH = path.expand(args[[1L]]))), state, envir)
    },
    cd = {
      stopifnot("use :cd or :cd PATH" = length(args) <= 1L)
      if (!length(args)) .emit(getwd(), "output", cfg) else setwd(path.expand(args[[1L]]))
    },
    pwd = .emit(getwd(), "output", cfg),
    git = panel(capture.output(utils::str(git_info())), "Git repository"),
    shell = .command_evaluate(.shell_call(parsed$raw), state, envir,
      external = TRUE, operation = "shell"),
    clear = {
      if (is.environment(cfg$screen) && isTRUE(cfg$screen$active)) screen_clear(cfg$screen)
      else if (state$native) cat("\033[2J\033[H")
    },
    doctor = .tui_table(repl_diagnostics(), "Terminal diagnostics", cfg),
    ui = {
      if (!length(args)) args <- .layout_choice(cfg)
      if (is.null(args)) return(invisible(NULL))
      need(1L); .set_console_layout(state, args[1L])
    },
    sidebar = {
      if (!length(args)) args <- .toggle_choice("sidebar", "Session pane", cfg$sidebar, cfg)
      if (is.null(args)) return(invisible(NULL))
      need(1L)
      stopifnot("sidebar must be on or off" = args[1L] %in% c("on", "off"))
      state$config$sidebar <- identical(args[1L], "on")
    },
    quit = { state$running <- FALSE },
    {
      definition <- .custom_command_lookup(cmd, cfg$commands_file, load = TRUE)
      if (is.null(definition)) stop("Unknown command. Use :help.", call. = FALSE)
      call <- as.call(c(list(definition$fun), unname(as.list(args))))
      .command_evaluate(call, state, envir, external = definition$external,
        operation = "custom", source = text)
    })
  invisible(NULL)
}

#' Change and Persist a Console Key Binding
#' @param args Arguments after the bind command.
#' @param state Session environment.
#' @return NULL invisibly.
#' @keywords internal
#' @noRd
.bind_command <- function(args, state) {
  stopifnot("arguments must be text" = is.character(args) && !anyNA(args),
            "state must be an environment" = is.environment(state))
  cfg <- state$config
  if (!length(args)) .binding_menu(state)
  else if (identical(args, "reset")) {
    state$config$keybindings <- repl_keybindings()
    state$config$key_lookup <- .key_lookup(state$config$keybindings)
    .persist_preferences(state$config, "keybindings")
    .emit("Default shortcuts restored.", "message", state$config)
  } else {
    action <- args[[1L]]
    stopifnot("unknown action; use :bind to list action identifiers" = action %in% names(cfg$keybindings))
    if (length(args) == 1L) .binding_menu(state, action, paste("Binding:", action))
    else {
      chords <- if (identical(args[-1L], "none")) character() else args[-1L]
      bindings <- .merge_keybindings(cfg$keybindings, stats::setNames(list(chords), action))
      state$config$keybindings <- bindings
      state$config$key_lookup <- .key_lookup(bindings)
      saved <- .persist_preferences(state$config, "keybindings")
      .emit(paste0(action, ": ", if (length(bindings[[action]])) paste(bindings[[action]], collapse = " / ") else "unbound",
                   if (saved) " (saved)" else " (this session)"), "message", state$config)
    }
  }
  invisible(NULL)
}

#' Select and Apply an Explicit Batch Operation
#' @param kind Objects or packages.
#' @param names Explicit names or an empty vector for a selector.
#' @param state Session state.
#' @param envir Evaluation environment.
#' @return NULL invisibly.
#' @keywords internal
#' @noRd
.batch_action <- function(kind, names, state, envir) {
  stopifnot("kind must be objects or packages" = kind %in% c("objects", "packages"),
            "names must be character" = is.character(names),
            "state and envir must be environments" = is.environment(state) && is.environment(envir))
  cfg <- state$config
  if (!length(names)) {
    if (kind == "objects") {
      objects <- workspace_objects(envir, sizes = FALSE)
      objects <- objects[!objects$locked, , drop = FALSE]
      candidates <- objects$name
      labels <- paste(objects$name, objects$class, objects$dimensions, sep = "  ")
    } else {
      packages <- loaded_packages()
      candidates <- packages$package[packages$attached & !packages$package %in%
        c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "prismrepl")]
      labels <- candidates
    }
    if (!length(candidates)) { .emit("No eligible entries.", "muted", cfg); return(invisible(NULL)) }
    chosen <- .tui_panel(labels, paste("Select", kind), cfg, rep(FALSE, length(labels)))
    if (!length(chosen)) return(invisible(NULL))
    names <- candidates[chosen]
  }
  .emit(paste0(if (kind == "objects") "Remove: " else "Detach: ", paste(.safe_text(names), collapse = ", ")),
        "warning", cfg)
  if (cfg$confirm && !.confirm_console(cfg))
    return(invisible(NULL))
  if (kind == "objects") {
    remove_objects(names, envir)
    .emit(paste("Removed", length(unique(names)), "objects."), "message", cfg)
  } else .emit(.table_lines(detach_packages(names)), "output", cfg)
  invisible(NULL)
}

#' Report Local Terminal Capabilities
#' @return A two-column data frame of local diagnostics. No network is accessed.
#' @export
#' @examples
#' repl_diagnostics()
repl_diagnostics <- function() {
  clipboard <- function(action) {
    if (.Platform$OS.type == "windows") return("CF_UNICODETEXT (Win32)")
    provider <- .clipboard_program(action)
    if (!is.null(provider)) return(provider$backend)
    if (action == "read") "Host paste shortcut or last Prism selection" else
      "OSC 52 request where accepted; Prism session fallback"
  }
  info <- c(list(R = as.character(getRversion()), platform = R.version$platform,
    locale = Sys.getlocale("LC_CTYPE"), colors = cli::num_ansi_colors(),
    hyperlinks = cli::ansi_has_hyperlink_support(),
    resolved_colors = .console_color_depth(),
    windows_terminal = nzchar(Sys.getenv("WT_SESSION")) || nzchar(Sys.getenv("WT_PROFILE_ID")),
    no_color = nzchar(Sys.getenv("NO_COLOR")),
    clipboard_read = clipboard("read"), clipboard_write = clipboard("write")), term_capabilities(),
    term_output_state(), as.list(term_size()))
  out <- data.frame(setting = names(info), value = vapply(info, function(x)
    paste(x, collapse = ", "), character(1L)), row.names = NULL, stringsAsFactors = FALSE)
  stopifnot("diagnostics must have two columns" = is.data.frame(out) && ncol(out) == 2L)
  out
}
