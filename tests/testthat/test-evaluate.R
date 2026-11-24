test_that("evaluation preserves visibility and stops at the first error", {
  env <- new.env(parent = baseenv())
  out <- evaluate_r("x <- 3; x + 4", env, display = FALSE)
  expect_identical(out$value, 7)
  expect_true(out$visible)
  expect_null(out$error)
  expect_identical(env$.Last.value, 7)
  expect_false(evaluate_r("y <- 4", env, display = FALSE)$visible)
  out <- evaluate_r("a <- 1; stop('broken'); b <- 2", env, display = FALSE)
  expect_s3_class(out$error, "error")
  expect_identical(env$a, 1)
  expect_false(exists("b", env, inherits = FALSE))
})

test_that("warning policies, messages, and interrupts are respected", {
  env <- new.env(parent = baseenv())
  out <- evaluate_r("message('notice'); warning('careful'); 9", env, display = FALSE)
  expect_length(out$warnings, 1L)
  expect_length(out$messages, 1L)
  expect_equal(out$value, 9)
  withr::local_options(warn = 2)
  out <- evaluate_r("warning('fatal'); x <- 1", env, display = FALSE)
  expect_match(conditionMessage(out$error), "converted from warning", fixed = TRUE)
  expect_false(exists("x", env, inherits = FALSE))
  options(warn = -1L)
  expect_length(evaluate_r("warning('hidden')", env, display = FALSE)$warnings, 0L)
  out <- evaluate_r("signalCondition(structure(list(message='stop'), class=c('interrupt','condition')))",
                    env, display = FALSE)
  expect_true(out$interrupted)
})

test_that("traces capture nested user calls before unwinding", {
  env <- new.env(parent = baseenv())
  out <- evaluate_r("outer <- function() inner(); inner <- function() stop('failure'); outer()",
                    env, display = FALSE)
  expect_s3_class(out$error, "error")
  expect_true(any(grepl("outer\\(", out$trace)))
  expect_true(any(grepl("inner\\(", out$trace)))
  expect_false(any(grepl("tryCatchList", out$trace)))
  expect_false(any(grepl("lapply|FUN\\(X", out$trace)))
})

test_that("implicit printing respects the supplied environment and custom classes", {
  env <- new.env(parent = baseenv())
  env$print.localmethod <- function(x, ...) { cat("CUSTOM_PRINT\n"); invisible(x) }
  env$x <- structure(list(a = 1), class = "localmethod")
  expect_output(out <- evaluate_r("x", env, repl_config(preview = "print", color = FALSE)), "CUSTOM_PRINT")
  expect_null(out$error)
  expect_output(out <- evaluate_r("x", env, repl_config(preview = "auto", color = FALSE)), "CUSTOM_PRINT")
  expect_null(out$error)
  locked <- new.env(parent = baseenv())
  lockEnvironment(locked)
  expect_null(evaluate_r("2 + 3", locked, display = FALSE)$error)
  active <- new.env(parent = baseenv())
  makeActiveBinding(".Last.value", function(value) stop("must not invoke"), active)
  expect_null(evaluate_r("2 + 3", active, display = FALSE)$error)
})

test_that("parsing and print-method errors are reported", {
  expect_s3_class(evaluate_r("x <- )", new.env(), display = FALSE)$error, "error")
  env <- new.env(parent = baseenv())
  env$print.bad <- function(x, ...) stop("print failed")
  env$x <- structure(1, class = "bad")
  cfg <- repl_config(preview = "print", color = FALSE)
  # S3 print dispatch sees methods defined in the evaluation environment.
  out <- evaluate_r("print(x)", env, cfg, display = FALSE)
  expect_match(conditionMessage(out$error), "print failed", fixed = TRUE)
})

test_that("evaluation does not replace global handlers or output sinks", {
  old <- options("error", "warn")
  sinks <- c(sink.number(), sink.number(type = "message"))
  out <- evaluate_r("stop('caught')", new.env(), display = FALSE)
  expect_s3_class(out$error, "error")
  expect_identical(options("error", "warn"), old)
  expect_identical(c(sink.number(), sink.number(type = "message")), sinks)
})

test_that("NULL results retain the structured result schema", {
  env <- new.env(parent = baseenv())
  out <- evaluate_r("NULL", env, display = FALSE)
  expect_true("value" %in% names(out))
  expect_null(out$value)
  expect_true(out$visible)
  expect_output(out <- evaluate_r("cat('printed')", env, display = FALSE), "printed")
  expect_true("value" %in% names(out))
  expect_null(out$value)
  expect_false(out$visible)
})
