custom_integration_state <- function(file) {
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL,
    commands_file = file, persist = FALSE)
  state$last <- state$last_error <- NULL
  state
}

custom_integration_function <- function(fun) {
  environment(fun) <- baseenv()
  fun
}

test_that("custom execution preserves closures, visibility and Last.value", {
  directory <- withr::local_tempdir()
  file <- file.path(directory, "commands.rds")
  captured <- new.env(parent = baseenv())
  captured$prefix <- "saved prefix"
  fun <- eval(quote(function(text) paste(prefix, text)), captured)
  repl_add_command("zzcustom-visible", fun, file = file)
  repl_add_command("zzcustom-hidden", custom_integration_function(
    function(text) invisible(paste0("hidden:", text))), file = file)
  state <- custom_integration_state(file)
  env <- new.env(parent = baseenv())
  env[["zzcustom-visible"]] <- function(...) stop("workspace mask must not run")

  output <- capture.output(.repl_command(":zzcustom-visible two  words", state, env))
  expect_null(state$last$error)
  expect_identical(state$last$value, "saved prefix two  words")
  expect_identical(env$.Last.value, state$last$value)
  expect_true(state$last$visible)
  expect_true(any(grepl("saved prefix two  words", output, fixed = TRUE)))
  expect_identical(state$last$code, ":zzcustom-visible two  words")

  output <- capture.output(.repl_command(":zzcustom-hidden quiet text", state, env))
  expect_null(state$last$error)
  expect_identical(state$last$value, "hidden:quiet text")
  expect_identical(env$.Last.value, "hidden:quiet text")
  expect_false(state$last$visible)
  expect_length(output, 0L)
})

test_that("custom conditions use the normal labeled output and error state", {
  withr::local_options(warn = 0L)
  directory <- withr::local_tempdir()
  file <- file.path(directory, "commands.rds")
  repl_add_command("zzcustom-condition", custom_integration_function(function(text) {
    message("message first\nmessage second")
    warning("warning first\nwarning second", call. = FALSE)
    paste("answer", text)
  }), file = file)
  repl_add_command("zzcustom-failure", custom_integration_function(function(text)
    stop(paste0("failure first\n", text), call. = FALSE)), file = file)
  state <- custom_integration_state(file)
  env <- new.env(parent = baseenv())

  output <- capture.output(.repl_command(":zzcustom-condition accepted", state, env))
  expect_null(state$last$error)
  expect_length(state$last$messages, 1L)
  expect_length(state$last$warnings, 1L)
  expect_identical(conditionMessage(state$last$warnings[[1L]]),
    "warning first\nwarning second")
  expect_equal(sum(grepl("[MESSAGE]", output, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("[WARNING]", output, fixed = TRUE)), 1L)
  expect_identical(env$.Last.value, "answer accepted")

  output <- capture.output(.repl_command(":zzcustom-failure failure second", state, env))
  expect_s3_class(state$last$error, "error")
  expect_identical(conditionMessage(state$last$error), "failure first\nfailure second")
  expect_identical(state$last_error, state$last)
  expect_identical(env$.Last.value, "answer accepted")
  expect_equal(sum(grepl("[ERROR]", output, fixed = TRUE)), 1L)
})

test_that("custom functions receive literal scalar strings for every arity", {
  directory <- withr::local_tempdir()
  file <- file.path(directory, "commands.rds")
  repl_add_command("zzcustom-zero", custom_integration_function(function() "zero"), file = file)
  repl_add_command("zzcustom-pair", custom_integration_function(function(left, right)
    list(left, right)), file = file)
  repl_add_command("zzcustom-dots", custom_integration_function(function(first, ...)
    c(list(first), list(...))), file = file)
  state <- custom_integration_state(file)
  env <- new.env(parent = baseenv())
  env$changed <- FALSE

  invisible(capture.output(.repl_command(":zzcustom-zero", state, env)))
  expect_identical(state$last$value, "zero")
  invisible(capture.output(.repl_command(':zzcustom-pair "two words" NA', state, env)))
  expect_identical(state$last$value, list("two words", "NA"))
  invisible(capture.output(.repl_command(':zzcustom-dots 123 "two words" "" x=y', state, env)))
  expect_identical(state$last$value, list("123", "two words", "", "x=y"))
  expect_true(all(vapply(state$last$value, function(x)
    is.character(x) && length(x) == 1L && !is.na(x), logical(1L))))

  command <- ':zzcustom-pair "changed <- TRUE; stop(123)" "$HOME"'
  invisible(capture.output(.repl_command(command, state, env)))
  expect_null(state$last$error)
  expect_identical(state$last$value, list("changed <- TRUE; stop(123)", "$HOME"))
  expect_false(env$changed)
  expect_identical(.history_submit(character(), command, 10L), command)
})

test_that("palette and completion share the selected custom registry", {
  directory <- withr::local_tempdir()
  first <- file.path(directory, "first.rds")
  second <- file.path(directory, "second.rds")
  body <- custom_integration_function(function(text)
    stop("Listing or completion must not run a command"))
  repl_add_command("zzcustom-first", body, alias = "zx",
    description = "Find the cobalt notebook", file = first)
  repl_add_command("zzcustom-second", body,
    description = "Find the amber notebook", file = second)
  config <- repl_config(color = FALSE, settings_file = NULL, commands_file = first)
  catalog <- .command_catalog(commands_file = first)

  expect_identical(.palette_data(catalog, "cobalt notebook")$id, "zzcustom-first")
  expect_false("zzcustom-second" %in% catalog$id)
  expect_true(":zzcustom-first" %in% complete_r(":zzcustom", commands_file = first)$candidates)
  expect_false(":zzcustom-second" %in% complete_r(":zzcustom", commands_file = first)$candidates)
  expect_identical(complete_r(":zx", commands_file = first)$candidates, ":zx")
  expect_identical(.command_parse(":zx notebook", commands_file = first)$command,
    "zzcustom-first")
  event <- list(type = "key", key = "enter", text = "", modifiers = character())
  selected <- .palette_apply(.palette_state("cobalt notebook"), event, config)
  expect_identical(selected$chosen, ":zzcustom-first ")
  expect_null(.palette_apply(.palette_state("amber notebook"), event, config)$chosen)
  expect_true("zzcustom-first" %in% repl_commands(details = TRUE, commands_file = first)$id)
  lines <- .palette_lines(.custom_command_catalog(second), 100L)$lines
  expect_false(any(grepl("(:)", lines, fixed = TRUE)))

  seen <- NULL
  local_mocked_bindings(.command_palette = function(config, query, catalog) {
    seen <<- list(query = query, ids = catalog$id)
    ":zzcustom-first "
  })
  state <- custom_integration_state(first)
  .repl_command(":co cobalt", state, new.env(parent = baseenv()))
  expect_identical(seen, list(query = "cobalt", ids = "zzcustom-first"))
  expect_identical(state$initial, ":zzcustom-first ")
  expect_null(state$last)

  repl_remove_command("zzcustom-first", file = first)
  expect_false(":zzcustom-first" %in% complete_r(":zzcustom", commands_file = first)$candidates)
  expect_true(":zzcustom-second" %in% complete_r(":zzcustom", commands_file = second)$candidates)
})

test_that("saved command closures work in a fresh matching R process", {
  withr::local_envvar(c(R_TESTS = ""))
  directory <- withr::local_tempdir("prism saved commands ")
  file <- file.path(directory, "commands.rds")
  captured <- new.env(parent = baseenv())
  captured$prefix <- "retained local value"
  repl_add_command("zzcustom-captured", eval(quote(function(text)
    paste(prefix, text)), captured), file = file)
  global <- function(text) paste(prism_custom_session_label, text)
  environment(global) <- .GlobalEnv
  repl_add_command("zzcustom-global", global, file = file)
  result_file <- file.path(directory, "result.rds")
  context <- file.path(directory, "context.rds")
  saveRDS(list(registry = file, result = result_file, libraries = .libPaths()), context)
  script <- file.path(directory, "fresh-session.R")
  writeLines(c(
    "context <- readRDS(commandArgs(trailingOnly = TRUE)[[1L]])",
    ".libPaths(context$libraries)",
    "prism_custom_session_label <- 'fresh global value'",
    "state <- new.env(parent = emptyenv())",
    "state$config <- prismrepl::repl_config(color = FALSE, settings_file = NULL, commands_file = context$registry)",
    "state$last <- state$last_error <- NULL",
    "workspace <- new.env(parent = baseenv())",
    "dispatch <- getFromNamespace('.repl_command', 'prismrepl')",
    "invisible(capture.output(dispatch(':zzcustom-captured two  words', state, workspace)))",
    "captured <- state$last$value",
    "captured_error <- state$last$error",
    "invisible(capture.output(dispatch(':zzcustom-global argument', state, workspace)))",
    "saveRDS(list(captured = captured, captured_error = captured_error, global = state$last$value,",
    "  global_error = state$last$error, last_value = workspace$.Last.value,",
    "  registered = prismrepl::repl_custom_commands(file = context$registry)$name), context$result)"
  ), script)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  result <- processx::run(rscript, c("--vanilla", script, context),
    timeout = 30000L, error_on_status = FALSE)
  expect_identical(result$status, 0L, info = paste(result$stdout, result$stderr))
  if (result$status == 0L) {
    answer <- readRDS(result_file)
    expect_null(answer$captured_error)
    expect_null(answer$global_error)
    expect_identical(answer$captured, "retained local value two  words")
    expect_identical(answer$global, "fresh global value argument")
    expect_identical(answer$last_value, answer$global)
    expect_setequal(answer$registered, c("zzcustom-captured", "zzcustom-global"))
  }
})

test_that("external custom commands restore the screen after success error and interruption", {
  directory <- withr::local_tempdir()
  file <- file.path(directory, "commands.rds")
  captured <- new.env(parent = baseenv())
  captured$prefix <- "external captured value"
  fun <- eval(quote(function(mode) {
    if (identical(mode, "error")) stop("external custom failure", call. = FALSE)
    if (identical(mode, "interrupt")) stop(structure(
      list(message = "custom interrupt", call = NULL), class = c("interrupt", "condition")))
    paste(prefix, mode)
  }), captured)
  repl_add_command("zzcustom-external", fun, external = TRUE, file = file)
  state <- custom_integration_state(file)
  state$config$screen <- new.env(parent = emptyenv())
  state$config$screen$active <- TRUE
  state$config$screen$mode <- "READY"
  hook <- function(...) stop("the screen output hook must be suspended")
  state$config$output_hook <- hook
  env <- new.env(parent = baseenv())
  events <- character()
  error_scopes <- logical()
  mark <- function(event) events <<- c(events, event)
  local_mocked_bindings(
    screen_leave = function(screen) {
      expect_true(screen$active)
      mark("leave")
      screen$active <- FALSE
    },
    screen_enter = function(screen, config) {
      expect_false(screen$active)
      mark("enter")
      screen$active <- TRUE
    },
    screen_invalidate = function(screen) mark("invalidate"),
    .shell_pause = function() {
      expect_false(state$config$screen$active)
      mark("pause")
      invisible(NULL)
    },
    .emit = function(lines, role, config) {
      if (is.null(config$screen)) expect_null(config$output_hook)
      invisible(lines)
    },
    .show_error = function(result, config) {
      error_scopes <<- c(error_scopes, is.null(config$screen))
      invisible(NULL)
    }
  )
  cases <- lapply(c("ok", "error", "interrupt"), function(mode) {
    events <<- character()
    .repl_command(paste(":zzcustom-external", mode), state, env)
    list(events = events, result = state$last, active = state$config$screen$active,
      mode = state$config$screen$mode, last_value = env$.Last.value)
  })
  expect_identical(cases[[1L]]$events, c("leave", "pause", "enter", "invalidate"))
  expect_identical(cases[[1L]]$result$value, "external captured value ok")
  expect_identical(cases[[1L]]$last_value, cases[[1L]]$result$value)
  expect_identical(cases[[1L]]$mode, "READY")
  expect_identical(cases[[2L]]$events, cases[[1L]]$events)
  expect_s3_class(cases[[2L]]$result$error, "error")
  expect_identical(conditionMessage(cases[[2L]]$result$error), "external custom failure")
  expect_identical(cases[[2L]]$mode, "ERROR")
  expect_identical(error_scopes, c(TRUE, FALSE))
  expect_identical(cases[[3L]]$events, c("leave", "enter", "invalidate"))
  expect_true(cases[[3L]]$result$interrupted)
  expect_identical(cases[[3L]]$mode, "INTERRUPTED")
  expect_identical(cases[[3L]]$last_value, cases[[1L]]$last_value)
  expect_true(all(vapply(cases, `[[`, logical(1L), "active")))
  expect_identical(state$last, cases[[3L]]$result)
  expect_identical(state$last_error, cases[[2L]]$result)
  expect_identical(state$config$output_hook, hook)
})

test_that("relative registry configuration stays bound to its original directory", {
  directory <- withr::local_tempdir()
  first <- file.path(directory, "first folder")
  second <- file.path(directory, "second folder")
  dir.create(first)
  dir.create(second)
  withr::local_dir(first)
  state <- custom_integration_state("commands.rds")
  repl_add_command("zzcustom-relative", custom_integration_function(function(text)
    paste("original", text)), description = "Original registry selection", file = "commands.rds")
  repl_add_command("zzcustom-relative", custom_integration_function(function(text)
    paste("other", text)), description = "Other registry selection",
    file = file.path(second, "commands.rds"))
  env <- new.env(parent = baseenv())

  .repl_command(paste(":cd", second), state, env)
  expect_identical(normalizePath(getwd(), winslash = "/"),
    normalizePath(second, winslash = "/"))
  invisible(capture.output(.repl_command(":zzcustom-relative selected text", state, env)))
  expect_null(state$last$error)
  expect_identical(state$last$value, "original selected text")
  catalog <- .command_catalog(commands_file = state$config$commands_file)
  expect_identical(.palette_data(catalog, "original registry")$id, "zzcustom-relative")
  expect_length(.palette_data(catalog, "other registry")$id, 0L)
})
