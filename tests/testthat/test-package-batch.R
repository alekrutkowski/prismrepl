test_that("batch attach builds ordered literal calls and preserves the single-package call", {
  calls <- list()
  local_mocked_bindings(.command_evaluate = function(call, state, envir, ...) {
    calls[[length(calls) + 1L]] <<- call
    invisible(NULL)
  })
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  env <- new.env(parent = baseenv())
  .repl_command(":attach stats utils stats datasets", state, env)
  .repl_command(":a stats utils datasets", state, env)
  .repl_command(":attach stats stats", state, env)
  .repl_command(":attach 'stats; stop(\"must not run\")' utils", state, env)
  expect_identical(calls[[1L]], calls[[2L]])
  expect_identical(calls[[1L]][[1L]], quote(base::`{`))
  expect_identical(as.list(calls[[1L]])[-1L], list(
    quote(base::library("stats", character.only = TRUE)),
    quote(base::library("utils", character.only = TRUE)),
    quote(base::library("datasets", character.only = TRUE))))
  expect_identical(calls[[3L]], quote(base::library("stats", character.only = TRUE)))
  expect_identical(calls[[4L]][[2L]][[2L]], 'stats; stop("must not run")')
  invisible(lapply(calls, function(call)
    expect_identical(parse(text = paste(deparse(call), collapse = "\n"))[[1L]], call)))
  expect_error(.repl_command(":attach", state, env), "use :attach", fixed = TRUE)
  expect_error(.repl_command(":a '' utils", state, env), "use :attach", fixed = TRUE)
  expect_error(.repl_command(":detatch stats", state, env), "Unknown command", fixed = TRUE)
})

test_that("batch attach evaluates in order and retains the last invisible result", {
  attached <- character()
  local_mocked_bindings(library = function(package, character.only) {
    expect_true(character.only)
    attached <<- c(attached, package)
    invisible(paste0("result:", package))
  }, .package = "base")
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  state$last <- state$last_error <- NULL
  env <- new.env(parent = baseenv())
  env$library <- function(...) stop("masked library must not run")
  env[["{"]] <- function(...) stop("masked braces must not run")
  output <- capture.output(.repl_command(":a first second first third", state, env))
  expect_identical(attached, c("first", "second", "third"))
  expect_null(state$last$error)
  expect_identical(state$last$value, "result:third")
  expect_false(state$last$visible)
  expect_identical(env$.Last.value, "result:third")
  expect_false(any(grepl("result:third", output, fixed = TRUE)))
})

test_that("batch attach stops at its first failure and preserves preceding effects", {
  attempted <- attached <- character()
  local_mocked_bindings(library = function(package, character.only) {
    attempted <<- c(attempted, package)
    if (identical(package, "broken")) stop("missing package: broken", call. = FALSE)
    attached <<- c(attached, package)
    invisible(package)
  }, .package = "base")
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  state$last <- state$last_error <- NULL
  env <- new.env(parent = baseenv())
  env$.Last.value <- 73L
  output <- capture.output(.repl_command(":attach first broken later", state, env))
  expect_identical(attempted, c("first", "broken"))
  expect_identical(attached, "first")
  expect_s3_class(state$last$error, "error")
  expect_match(conditionMessage(state$last$error), "missing package: broken", fixed = TRUE)
  expect_identical(state$last_error, state$last)
  expect_identical(env$.Last.value, 73L)
  expect_true(any(grepl("[ERROR]", output, fixed = TRUE)))
})

test_that("batch attach also runs the ordinary base library implementation", {
  before <- search()
  on.exit(invisible(lapply(setdiff(search(), before), function(entry)
    base::detach(entry, character.only = TRUE, unload = FALSE))), add = TRUE)
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  state$last <- state$last_error <- NULL
  env <- new.env(parent = baseenv())
  invisible(capture.output(.repl_command(":attach stats utils datasets stats", state, env)))
  expect_null(state$last$error)
  expect_true(all(paste0("package:", c("stats", "utils", "datasets")) %in% search()))
  expect_identical(state$last$value, .packages())
  expect_false(state$last$visible)
  expect_identical(env$.Last.value, .packages())
})

test_that("batch detach retains selection, confirmation and per-package results", {
  received <- list()
  local_mocked_bindings(.batch_action = function(kind, names, state, envir) {
    received[[length(received) + 1L]] <<- list(kind = kind, names = names,
      confirm = state$config$confirm)
    invisible(NULL)
  })
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  env <- new.env(parent = baseenv())
  .repl_command(":detach first second", state, env)
  .repl_command(":de first second", state, env)
  .repl_command(":detach", state, env)
  expect_identical(received[[1L]], list(kind = "packages",
    names = c("first", "second"), confirm = TRUE))
  expect_identical(received[[2L]], received[[1L]])
  expect_identical(received[[3L]]$names, character())
  names <- c("prismrepl-test-absent-one", "prismrepl-test-absent-two")
  result <- detach_packages(c(names, names[[1L]]))
  expect_identical(result$package, names)
  expect_identical(result$status, rep("not attached", 2L))
  expect_error(detach_packages(c(names, "base")), "core default packages", fixed = TRUE)
})

test_that("package commands complete every argument and retain complete history entries", {
  invisible(lapply(c(":attach stats uti", ":a stats uti", ":detach stats uti", ":de stats uti"),
    function(text) {
      result <- complete_r(text)
      expect_true("utils" %in% result$candidates)
      expect_identical(substr(text, result$start + 1L, result$end), "uti")
      expect_identical(result$end, nchar(text))
    }))
  text <- ":attach stats utils data"
  result <- complete_r(text)
  expect_true("datasets" %in% result$candidates)
  expect_identical(substr(text, result$start + 1L, result$end), "data")
  catalog <- .command_catalog()
  expect_identical(catalog$command[catalog$id == "attach"], ":attach PACKAGE ...")
  expect_identical(catalog$alias[catalog$id == "attach"], "a")
  expect_identical(catalog$alias[catalog$id == "detach"], "de")
  expect_false(any(catalog$id == "detatch"))
  commands <- c(":attach stats utils", ":detach first second")
  history <- Reduce(function(previous, command) .history_submit(previous, command, 10L),
    commands, init = character())
  expect_identical(history, commands)
})
