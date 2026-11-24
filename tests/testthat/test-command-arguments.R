test_that("every command has an unambiguous short alias and shared metadata", {
  catalog <- .command_catalog()
  expect_false(anyDuplicated(catalog$id) > 0L)
  expect_false(anyDuplicated(catalog$alias) > 0L)
  expect_true(all(nchar(catalog$alias) <= 2L))
  expect_equal(vapply(paste0(":", catalog$alias), function(x) .command_parse(x)$command,
                      character(1L), USE.NAMES = FALSE), catalog$id)
  expect_equal(.command_parse(":h")$command, "help")
  expect_equal(repl_commands(details = TRUE), catalog)
  expect_error(repl_commands(details = NA), "logical")
})

test_that("file command parsing preserves spaces and Windows backslashes literally", {
  paths <- c("my/path/folder with  spaces/my file.R", "C:\\Users\\Alek\\R files\\my script.R",
             "\\\\server\\share name\\script.R", "données/分析.R", "dir/it's a file.R")
  lapply(paths, function(path) {
    expect_identical(.command_parse(paste(":source", path))$args, path)
    expect_identical(.command_parse(paste(":so", path))$args, path)
    expect_identical(.command_parse(paste(":cd", path))$args, path)
    expect_identical(.command_parse(paste0(":source `", path, "`"))$args, path)
  })
  expect_identical(.command_parse(':source "folder with  spaces/file.R"')$args,
                   "folder with  spaces/file.R")
  expect_identical(.command_parse(":cd")$args, character())
  expect_error(.command_parse(":source file.R\nstop('bad')"), "newline")
})

test_that("source aliases use the supplied workspace and cd without a path shows it", {
  directory <- tempfile("source files ")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "my  script.R")
  writeLines("from_source <- 73L\nmessage('first\\nsecond')", path)
  env <- new.env(parent = baseenv())
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = NULL)
  state$last <- state$last_error <- NULL
  output <- capture.output(.repl_command(paste(":so", path), state, env))
  expect_identical(env$from_source, 73L)
  expect_null(state$last$error)
  expect_equal(sum(grepl("[MESSAGE]", output, fixed = TRUE)), 1L)
  expect_true(any(grepl("first", output)))
  expect_true(any(grepl("second", output)))
  expect_output(.repl_command(":cd", state, env), getwd(), fixed = TRUE)
  expect_output(.repl_command(":pw", state, env), getwd(), fixed = TRUE)
})

test_that("package aliases build quoted literal calls instead of executable strings", {
  calls <- list()
  local_mocked_bindings(.command_evaluate = function(call, state, envir, ...) {
    calls[[length(calls) + 1L]] <<- call
    invisible(NULL)
  })
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(settings_file = NULL)
  env <- new.env(parent = baseenv())
  .repl_command(":a stats", state, env)
  .repl_command(":i data.table cli utf8", state, env)
  expect_equal(calls[[1L]], quote(base::library("stats", character.only = TRUE)))
  expect_equal(calls[[2L]][[2L]], c("data.table", "cli", "utf8"))
  expect_error(.repl_command(":install cli;stop('bad')", state, env), "package names")
  expect_error(.repl_command(":installgh ../bad", state, env), "user/repo")
  expect_error(.repl_command(":attach", state, env), "number")
})

test_that("commands join multiline expression history only when submitted", {
  old <- c("x <- 2", "f <- function(x) {\n  x + 1\n}")
  expect_identical(.history_submit(old, ":h", 10L), c(old, ":h"))
  expect_identical(.history_submit(c(old, ":h"), ":h", 3L), c(old, ":h"))
  expect_identical(.history_submit(c(old, ":h"), ":source my file.R", 2L),
                   c(":h", ":source my file.R"))
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  history <- .history_submit(old, ":theme white", 10L)
  save_repl_history(history, path)
  expect_identical(repl_history(path), history)
})

test_that("command completion knows aliases packages and space-containing paths", {
  local_mocked_bindings(.cran_package_names = function(...) list(
    names = c("cli", "data.table", "digest", "utf8"), status = ""))
  expect_true(":source" %in% complete_r(":so")$candidates)
  expect_true(":so" %in% complete_r(":so")$candidates)
  expect_true("stats" %in% complete_r(":a sta")$candidates)
  expect_true(all(startsWith(complete_r(":a sta")$candidates, "sta")))
  result <- complete_r(":i cli data.t")
  expect_identical(result$candidates, "data.table")
  expect_identical(substr(":i cli data.t", result$start + 1L, result$end), "data.t")
  directory <- tempfile("command paths ")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "a  script.R")
  writeLines("1", path)
  result <- complete_r(paste0(":source ", directory, "/a"))
  expect_identical(result$candidates, path)
  expect_identical(result$start, nchar(":source "))
  expect_null(.complete_command("x <- 1", 6L, .GlobalEnv, 20L))
})

test_that("CRAN cache reads bounded data and rejects another repository", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines(c("Repository: https://example.invalid", "data.table", "cli"), path)
  expect_identical(.read_cran_index(path, "https://example.invalid")$names, c("cli", "data.table"))
  expect_null(.read_cran_index(path, "https://cloud.r-project.org"))
  writeLines(c("Repository: https://example.invalid", "system('bad')"), path)
  expect_null(.read_cran_index(path, "https://example.invalid"))
  withr::local_options(repos = c(CRAN = "@CRAN@", custom = "https://example.invalid"))
  expect_identical(.installation_repositories(), c(CRAN = "https://cloud.r-project.org", custom = "https://example.invalid"))
})
