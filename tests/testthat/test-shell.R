test_that("shell commands preserve native syntax without R or token parsing", {
  commands <- c(
    "printf '%s\\n' 'two  spaces' | cat > \"out file.txt\"",
    '"C:\\Program Files\\tool.exe" "C:\\folder  name\\file.txt" & echo %TEMP%',
    "printf '%s' x\\ ",
    "echo `date` $(printf literal) $HOME # shell comment",
    "echo \"a quote remains open",
    "printf '%s\\n' 'caf\u00e9'\nprintf '%s' done")
  invisible(lapply(commands, function(command) {
    invisible(lapply(c("shell", "sh", "SHELL"), function(alias) {
      parsed <- .command_parse(paste0(" \t:", alias, "\t", command))
      expect_identical(parsed$command, "shell")
      expect_identical(parsed$raw, command)
      expect_identical(parsed$args, command)
    }))
  }))
  expect_error(.shell_call(""), "use :shell COMMAND", fixed = TRUE)
  expect_error(.shell_call(" \t"), "use :shell COMMAND", fixed = TRUE)
  expect_error(.shell_call(NA_character_), "use :shell COMMAND", fixed = TRUE)
})

test_that("shell invocations keep user syntax as literal data across R parsing", {
  command <- 'echo "literal"; assign("unexpected", TRUE); # C:\\path with spaces\\file'
  posix <- .shell_call(command, windows = FALSE)
  windows <- .shell_call(command, windows = TRUE, windows_shell = "C:/Windows/System32/cmd.exe")
  expect_identical(posix[[1L]], quote(base::system))
  expect_identical(posix[[2L]], command)
  expect_false(posix$intern)
  expect_true(posix$wait)
  expect_identical(windows[[1L]], quote(base::system2))
  expect_identical(windows[[2L]], "C:/Windows/System32/cmd.exe")
  expect_identical(eval(windows$args, baseenv()), c("/d", "/s", "/c", paste0('"', command, '"')))
  expect_identical(windows$stdout, "")
  expect_identical(windows$stderr, "")
  expect_true(windows$wait)
  expect_identical(.shell_call("echo ok", windows = TRUE, windows_shell = "")[[2L]], "cmd.exe")
  invisible(lapply(list(posix, windows), function(call) {
    text <- paste(deparse(call, width.cutoff = 100L), collapse = "\n")
    parsed <- parse(text = text)
    expect_length(parsed, 1L)
    expect_identical(parsed[[1L]], call)
  }))
})

test_that("shell dispatch uses the external console and preserves raw commands", {
  calls <- list()
  local_mocked_bindings(.command_evaluate = function(call, state, envir,
      external = FALSE, operation = "package") {
    calls[[length(calls) + 1L]] <<- list(call = call, external = external, operation = operation)
    invisible(NULL)
  })
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  env <- new.env(parent = baseenv())
  command <- 'echo "literal"; assign("unexpected", TRUE); # not R code'
  invisible(lapply(c(":shell ", ":sh "), function(prefix)
    .repl_command(paste0(prefix, command), state, env)))
  expect_length(calls, 2L)
  expect_identical(calls[[1L]], calls[[2L]])
  expect_true(calls[[1L]]$external)
  expect_identical(calls[[1L]]$operation, "shell")
  expect_identical(calls[[1L]]$call, .shell_call(command))
  expect_false(exists("unexpected", envir = env, inherits = FALSE))
  expect_error(.repl_command(":sh", state, env), "use :shell COMMAND", fixed = TRUE)
})

test_that("shell commands join completion, description search and submitted history", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  catalog <- .command_catalog()
  expect_identical(catalog$alias[catalog$id == "shell"], "sh")
  expect_true(all(c(":shell", ":sh") %in% complete_r(":sh")$candidates))
  expect_length(complete_r(":sh echo ")$candidates, 0L)
  expect_identical(.palette_data(catalog, "shell pipes redirects")$id, "shell")
  selected <- .palette_apply(.palette_state("shell pipes redirects"),
    list(type = "key", key = "enter", text = "", modifiers = character()), cfg)
  expect_identical(selected$chosen, ":shell ")
  history <- .history_submit("x <- 1", ":sh echo 'two  spaces' ", 10L)
  expect_identical(tail(history, 1L), ":sh echo 'two  spaces' ")
  path <- withr::local_tempfile()
  save_repl_history(history, path)
  expect_identical(repl_history(path), history)
})

test_that("shell summaries distinguish exit failures and R conditions", {
  result <- list(value = 0L, error = NULL, interrupted = FALSE)
  expect_identical(.shell_summary(result)$role, "message")
  result$value <- 7L
  expect_identical(.shell_summary(result), list(text = "Shell exited with status 7.", role = "error"))
  result$interrupted <- TRUE
  expect_identical(.shell_summary(result)$role, "warning")
  result$interrupted <- FALSE
  result$error <- simpleError("could not start")
  expect_identical(.shell_summary(result)$role, "error")
  result["error"] <- list(NULL)
  result$value <- NA_integer_
  expect_error(.shell_summary(result), "finite exit status")
})

test_that("external shell execution restores the screen on every exit path", {
  run_case <- function(status = 0L, fullscreen = TRUE, interrupted = FALSE,
      evaluation_error = FALSE, unexpected_error = FALSE, pause_interrupt = FALSE,
      pause_error = FALSE, operation = "shell", warning = FALSE) {
    state <- new.env(parent = emptyenv())
    state$config <- repl_config(color = FALSE, settings_file = NULL)
    state$config$screen <- new.env(parent = emptyenv())
    state$config$screen$active <- fullscreen
    state$config$screen$mode <- "WARNING"
    state$config$screen$elapsed <- 9.25
    state$config$output_hook <- function(...) stop("the active hook must not run")
    original_hook <- state$config$output_hook
    previous_error <- list(error = simpleError("previous R error"))
    state$last_error <- previous_error
    state$last <- NULL
    events <- character()
    messages <- list()
    entry <- NULL
    result <- list(value = status, error = if (evaluation_error) simpleError("could not start") else NULL,
      interrupted = interrupted, warnings = if (warning) list(simpleWarning("shell warning")) else list(),
      elapsed = 0.125)
    mark <- function(event) events <<- c(events, event)
    invoke <- .command_evaluate
    environment(invoke) <- list2env(list(
      screen_leave = function(screen) { mark("leave"); screen$active <- FALSE },
      screen_enter = function(screen, config) {
        mark("enter")
        entry <<- list(mode = screen$mode, elapsed = screen$elapsed)
        screen$active <- TRUE
      },
      screen_invalidate = function(screen) mark("invalidate"),
      .evaluate_session = function(code, envir, config) {
        mark("evaluate")
        expect_false(isTRUE(state$config$screen$active))
        if (fullscreen) { expect_null(config$screen); expect_null(config$output_hook) }
        if (unexpected_error) stop("unexpected evaluation failure")
        result
      },
      .shell_pause = function() {
        mark("pause")
        expect_false(state$config$screen$active)
        if (pause_interrupt) stop(structure(list(message = "interrupt", call = NULL),
          class = c("interrupt", "condition")))
        if (pause_error) stop("pause failed")
        invisible(NULL)
      },
      .emit = function(lines, role, config) {
        messages[[length(messages) + 1L]] <<- list(text = lines, role = role)
        invisible(lines)
      },
      .show_error = function(result, config) mark("show_error")),
      parent = environment(invoke))
    answer <- tryCatch(invoke(quote(base::system("fixture")), state, new.env(parent = baseenv()),
      external = TRUE, operation = operation), error = function(e) e)
    expect_identical(state$config$output_hook, original_hook)
    expect_identical(state$config$screen$active, fullscreen)
    if (!unexpected_error) expect_identical(state$last, result)
    expect_identical(state$last_error, if (evaluation_error) result else previous_error)
    list(events = events, messages = messages, answer = answer, entry = entry,
      screen = list(mode = state$config$screen$mode, elapsed = state$config$screen$elapsed))
  }
  success <- run_case()
  expect_identical(success$events, c("leave", "evaluate", "pause", "enter", "invalidate"))
  expect_identical(success$entry, list(mode = "READY", elapsed = 0.125))
  expect_identical(success$screen, success$entry)
  failed <- run_case(status = 7L)
  expect_identical(tail(failed$messages, 1L)[[1L]]$role, "error")
  expect_identical(failed$events, success$events)
  expect_identical(failed$entry, list(mode = "ERROR", elapsed = 0.125))
  paused <- run_case(pause_interrupt = TRUE)
  expect_identical(paused$events, success$events)
  expect_false(inherits(paused$answer, "condition"))
  expect_identical(paused$entry, success$entry)
  interrupted <- run_case(interrupted = TRUE)
  expect_identical(interrupted$events, c("leave", "evaluate", "enter", "invalidate"))
  expect_identical(tail(interrupted$messages, 1L)[[1L]]$role, "warning")
  expect_identical(interrupted$entry, list(mode = "INTERRUPTED", elapsed = 0.125))
  error <- run_case(evaluation_error = TRUE)
  expect_identical(error$events, c(success$events, "show_error"))
  expect_identical(error$entry, list(mode = "ERROR", elapsed = 0.125))
  warned <- run_case(warning = TRUE)
  expect_identical(warned$entry, list(mode = "WARNING", elapsed = 0.125))
  expect_identical(warned$screen, warned$entry)
  unexpected <- run_case(unexpected_error = TRUE)
  expect_s3_class(unexpected$answer, "error")
  expect_identical(unexpected$events, c("leave", "evaluate", "enter", "invalidate"))
  expect_identical(unexpected$entry, list(mode = "WARNING", elapsed = 9.25))
  pause_failure <- run_case(pause_error = TRUE)
  expect_s3_class(pause_failure$answer, "error")
  expect_identical(pause_failure$events, success$events)
  expect_identical(pause_failure$entry, success$entry)
  inline <- run_case(fullscreen = FALSE)
  expect_identical(inline$events, "evaluate")
  expect_null(inline$entry)
  expect_identical(inline$screen, list(mode = "WARNING", elapsed = 9.25))
  installer <- run_case(operation = "package")
  expect_identical(installer$events, c("leave", "evaluate", "enter", "invalidate"))
  expect_match(tail(installer$messages, 1L)[[1L]]$text, "Package operation returned", fixed = TRUE)
  expect_identical(installer$entry, list(mode = "WARNING", elapsed = 9.25))
})

test_that("native shell commands handle quoted scripts, arguments and nonzero status", {
  directory <- withr::local_tempdir(pattern = "prism shell ")
  withr::local_dir(directory)
  windows <- .Platform$OS.type == "windows"
  script <- file.path(directory, if (windows) "run tool.cmd" else "run tool.sh")
  output <- file.path(directory, "result file.txt")
  writeLines(if (windows) c("@echo off", '> "%~2" echo %~1', "exit /b 0") else
    c("#!/bin/sh", 'printf \'%s\\n\' "$1" > "$2"'), script, useBytes = TRUE)
  if (!windows) Sys.chmod(script, "0700")
  quote_path <- function(path) if (windows) paste0('"', gsub("/", "\\", path, fixed = TRUE), '"') else shQuote(path)
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  state$last <- state$last_error <- NULL
  envir <- new.env(parent = baseenv())
  command <- paste(":shell", quote_path(script), '"value with two  spaces"', quote_path(output))
  displayed <- capture.output(.repl_command(command, state, envir))
  expect_identical(readLines(output, warn = FALSE), "value with two  spaces")
  expect_identical(state$last$value, 0L)
  expect_identical(envir$.Last.value, 0L)
  expect_null(state$last$error)
  expect_true(any(grepl("Shell exited with status 0.", displayed, fixed = TRUE)))
  displayed <- capture.output(.repl_command(if (windows) ":sh exit /b 7" else ":sh exit 7", state, envir))
  expect_identical(state$last$value, 7L)
  expect_identical(envir$.Last.value, 7L)
  expect_null(state$last$error)
  expect_true(any(grepl("[ERROR]", displayed, fixed = TRUE)))
  expect_true(any(grepl("Shell exited with status 7.", displayed, fixed = TRUE)))
})

test_that("native shell pipes and child environment changes remain local", {
  directory <- withr::local_tempdir(pattern = "prism shell pipe ")
  withr::local_dir(directory)
  withr::local_envvar(c(PRISM_SHELL_TEST_LITERAL = "parent value"))
  dir.create("child")
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  state$last <- state$last_error <- NULL
  envir <- new.env(parent = baseenv())
  windows <- .Platform$OS.type == "windows"
  command <- if (windows) ':sh echo pipe ok| findstr /c:"pipe ok" > "piped file.txt"' else
    ':sh printf \'%s\\n\' \'pipe ok\' | cat > "piped file.txt"'
  invisible(capture.output(.repl_command(command, state, envir)))
  expect_identical(trimws(readLines("piped file.txt", warn = FALSE)), "pipe ok")
  expect_identical(state$last$value, 0L)
  parent <- getwd()
  command <- if (windows) ":sh cd child & set PRISM_SHELL_TEST_LITERAL=child & exit /b 0" else
    ":sh cd child; PRISM_SHELL_TEST_LITERAL=child; export PRISM_SHELL_TEST_LITERAL; exit 0"
  invisible(capture.output(.repl_command(command, state, envir)))
  expect_identical(getwd(), parent)
  expect_identical(Sys.getenv("PRISM_SHELL_TEST_LITERAL"), "parent value")
  expect_identical(state$last$value, 0L)
})
