test_that("the native parser distinguishes complete, incomplete and invalid input", {
  expect_identical(parse_status("1 + 2"), "complete")
  expect_identical(parse_status("function(x) {"), "incomplete")
  expect_identical(parse_status("1 + )"), "error")
  expect_identical(parse_status(""), "null")
  expect_identical(parse_status("# only a comment\n"), "null")
  expect_identical(parse_status("\u03b1 <- '\U0001f642'"), "complete")
  expect_identical(parse_status('r"(a\\b)"'), "complete")
  expect_error(parse_status(NA_character_), "non-missing")
  expect_error(parse_status(c("a", "b")), "one")
})

test_that("native binding inspection never runs active or delayed bindings", {
  env <- new.env(parent = baseenv())
  env$value <- c(1L, 2L)
  delayedAssign("deferred", stop("promise was evaluated"), assign.env = env)
  makeActiveBinding("active", function() stop("active binding was evaluated"), env)
  expect_identical(binding_info(env, c("value", "deferred", "active", "absent")),
    c(value = "value", deferred = "promise", active = "active", absent = "missing"))
  expect_identical(binding_info(env, character()), stats::setNames(character(), character()))
  expect_error(binding_info(1, "value"), "environment")
  expect_error(binding_info(env, NA_character_), "missing")
  delayedAssign("cached", 42, assign.env = env)
  expect_identical(binding_info(env, "cached"), c(cached = "promise"))
  expect_equal(env$cached, 42)
  expect_identical(binding_info(env, "cached"), c(cached = "value"))
})

test_that("binding inspection distinguishes missing arguments from cached defaults", {
  env <- (function(required, deferred = stop("default was evaluated"), cached = 42, ...) {
    cached
    environment()
  })()
  expect_identical(binding_info(env, c("required", "deferred", "cached", "...")),
    c(required = "missing", deferred = "promise", cached = "value", "..." = "missing"))
  with_dots <- (function(...) environment())(stop("dots were evaluated"))
  expect_identical(binding_info(with_dots, "..."), c("..." = "value"))

  rlang::env_bind(env, explicit_missing = )
  delayedAssign("forced_missing", rlang::missing_arg(), assign.env = env)
  expect_identical(binding_info(env, "forced_missing"), c(forced_missing = "promise"))
  invisible(rlang::env_get_list(env, "forced_missing"))
  expect_identical(binding_info(env, c("explicit_missing", "forced_missing")),
    c(explicit_missing = "missing", forced_missing = "value"))
})

test_that("binding inspection preserves frame scope, Unicode names and duplicate requests", {
  parent <- new.env(parent = emptyenv())
  parent$inherited <- 1L
  lapply(c(FALSE, TRUE), function(hashed) {
    env <- new.env(hash = hashed, parent = parent)
    env$null <- NULL
    env$expression <- quote(stop("language value was evaluated"))
    env$symbol <- as.name("inherited")
    env[["\u03b1 with spaces"]] <- list(data = 1:3)
    delayedAssign("recursive", recursive, eval.env = env, assign.env = env)
    makeActiveBinding("active", function() stop("active binding was evaluated"), env)
    lockBinding("active", env)
    requested <- c("null", "inherited", "", "\u03b1 with spaces", "recursive",
                   "active", "expression", "symbol", "null")
    expect_identical(binding_info(env, requested), stats::setNames(
      c("value", "missing", "missing", "value", "promise", "active", "value", "value", "value"),
      requested))
    expect_identical(binding_info(env, c("recursive", "active", "absent")),
      stats::setNames(c("promise", "active", "missing"), c("recursive", "active", "absent")))
    expect_identical(binding_info(env, c("active", "absent")),
      c(active = "active", absent = "missing"))
  })
})

test_that("global missing bindings and forced missing values retain their distinct kinds", {
  names <- paste0(".prismrepl_binding_test_", Sys.getpid(), c("_missing", "_forced", "_active", "_lazy"))
  stopifnot("temporary test names must be unused" =
              !any(vapply(names, exists, logical(1L), envir = globalenv(), inherits = FALSE)))
  on.exit(rm(list = names, envir = globalenv()), add = TRUE)
  do.call(rlang::env_bind, c(list(globalenv()), stats::setNames(list(rlang::missing_arg()), names[1L])))
  delayedAssign(names[2L], rlang::missing_arg(), assign.env = globalenv())
  invisible(rlang::env_get_list(globalenv(), names[2L]))
  # No active/lazy fixture is present yet. A clone of a plain environment may
  # collapse a forced promise to its cached missing value; inspection must not
  # depend on clone implementation, binding order or the process identifier.
  expect_identical(binding_info(globalenv(), names[1:2]),
    stats::setNames(c("missing", "value"), names[1:2]))
  makeActiveBinding(names[3L], function() stop("global active binding was evaluated"), globalenv())
  delayedAssign(names[4L], stop("global promise was evaluated"), assign.env = globalenv())
  expect_identical(binding_info(globalenv(), names),
    stats::setNames(c("missing", "value", "active", "promise"), names))
})

test_that("global cached missing values are independent of binding names and order", {
  names <- paste0(".prismrepl_cached_missing_", Sys.getpid(), "_", seq_len(64L))
  stopifnot("temporary test names must be unused" =
    !any(vapply(names, exists, logical(1L), envir = globalenv(), inherits = FALSE)))
  on.exit(rm(list = names, envir = globalenv()), add = TRUE)
  invisible(lapply(names, function(name)
    delayedAssign(name, rlang::missing_arg(), assign.env = globalenv())))
  expect_identical(binding_info(globalenv(), names),
    stats::setNames(rep("promise", length(names)), names))
  invisible(rlang::env_get_list(globalenv(), names))
  requested <- c(rev(names), names[c(1L, 32L, 64L)])
  expect_identical(binding_info(globalenv(), requested),
    stats::setNames(rep("value", length(requested)), requested))
})

test_that("forced defaults containing missing remain values without evaluating others", {
  env <- (function(required, deferred = stop("default was evaluated"),
                   cached = rlang::missing_arg()) {
    invisible(rlang::env_get_list(environment(), "cached"))
    environment()
  })()
  expect_identical(binding_info(env, c("required", "deferred", "cached", "...")),
    c(required = "missing", deferred = "promise", cached = "value", "..." = "missing"))
})

test_that("terminal inspection has no side effects outside a native terminal", {
  dimensions <- term_size()
  expect_identical(names(dimensions), c("width", "height"))
  expect_true(all(dimensions > 0))
  capabilities <- term_capabilities()
  expect_type(capabilities$supported, "logical")
  expect_length(capabilities$supported, 1L)
  expect_true(capabilities$platform %in% c("unix", "windows", "macos"))
  expect_null(term_close(NULL))
  if (!capabilities$supported) expect_null(term_open())
  if (!capabilities$supported) expect_null(term_open(mouse = TRUE))
  expect_error(term_open(mouse = NA), "non-missing")
  expect_error(term_open(mouse = 1), "logical")
})

test_that("output inspection and cleanup are safe without a real terminal", {
  before <- term_output_state()
  expect_identical(names(before), c("supported", "active", "vt_stdout", "vt_stderr",
                                    "codepage", "stdout_mode", "stderr_mode"))
  expect_type(before$supported, "logical")
  expect_type(before$active, "logical")
  expect_null(term_output_close(NULL))
  if (!before$supported) expect_null(term_output_open())
  if (!before$active) {
    expect_false(term_output_screen(TRUE))
    expect_false(term_output_screen(FALSE))
    expect_null(term_reset())
    expect_null(term_reset(output = TRUE))
    expect_identical(term_output_state(), before)
  }
  expect_error(term_reset(output = NA), "non-missing")
  expect_error(term_output_screen(active = NA), "non-missing")
})

test_that("buffered output validates frames without touching redirected stdout", {
  expect_error(term_write(NA_character_), "non-missing")
  expect_error(term_write(c("first", "second")), "one")
  expect_error(term_write("frame", synchronize = NA), "non-missing")
  expect_error(term_write("frame", synchronize = 1), "logical")
  expect_error(term_write("frame", cursor = NA_character_), "cursor")
  expect_error(term_write("frame", cursor = c("#FFFFFF", "#000000")), "cursor")
  expect_error(term_write("frame", cursor = "#FFF"), "cursor")
  expect_error(term_write("frame", cursor = "#GGGGGG"), "cursor")
  expect_error(term_write("frame", cursor = "red"), "cursor")
  expect_error(.Call(C_term_write, "frame", FALSE, "#GGGGGG"), "[Cc]ursor")
  expect_error(.Call(C_term_write, "frame", FALSE, NA_character_), "[Cc]ursor")
  if (!term_output_state()$supported)
    expect_error(term_write("frame"), "real terminal")
})

test_that("native clipboard calls reject invalid inputs before system access", {
  expect_error(.Call(C_clipboard_read, 0L), "limit")
  expect_error(.Call(C_clipboard_read, NA_integer_), "limit")
  expect_error(.Call(C_clipboard_read, 8388609L), "limit")
  expect_error(.Call(C_clipboard_read, 100), "limit")
  expect_error(.Call(C_clipboard_write, NA_character_), "non-missing")
  expect_error(.Call(C_clipboard_write, c("a", "b")), "one")
})
