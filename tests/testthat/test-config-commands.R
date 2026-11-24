test_that("configuration validates values without startup effects", {
  cfg <- repl_config(color = FALSE, status = character(), history_file = NULL)
  expect_s3_class(cfg, "prismrepl_config")
  expect_null(cfg$history_file)
  expect_error(repl_config(pipe = "oops"))
  expect_error(repl_config(status = "unknown"))
  expect_error(repl_config(rows = 1.5))
  expect_error(repl_config(color = NA))
  expect_error(repl_config(max_paste = Inf))
  expect_error(repl_config(ui = "unknown"))
  expect_error(repl_config(editor_height = 1L))
  expect_error(repl_config(transcript_limit = 10L))
  expect_error(repl_config(color_depth = 42L))
  expect_identical(cfg$ui, "auto")
  expect_true(cfg$sidebar)
})

test_that("Windows Terminal color detection works before VT is enabled", {
  caps <- list(supported = TRUE, truecolor = FALSE)
  env <- c(WT_SESSION = "test-session", TERM = "", NO_COLOR = "", COLORTERM = "")
  expect_identical(.console_color_depth(caps, env, detected = 1L), 16777216L)
  expect_identical(.console_color_depth(caps, env, detected = 1L, color = FALSE), 0L)
  env[["NO_COLOR"]] <- "1"
  expect_identical(.console_color_depth(caps, env, detected = 256L), 0L)
  expect_identical(.console_color_depth(caps, env, detected = 1L, color = TRUE), 16777216L)
  env[["NO_COLOR"]] <- ""
  env[["WT_SESSION"]] <- ""
  env[["TERM"]] <- "xterm-256color"
  expect_identical(.console_color_depth(caps, env, detected = 1L), 256L)
  expect_identical(.console_color_depth(caps, env, detected = 1L, requested = 16L), 16L)
  env[["TERM"]] <- "dumb"
  expect_identical(.console_color_depth(list(supported = FALSE), env, detected = 1L), 0L)
})

test_that("commands mutate only explicitly requested state", {
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(confirm = FALSE, color = FALSE, status = character(), settings_file = NULL)
  state$history <- c("x <- 1", "x + 2")
  state$native <- FALSE
  state$running <- TRUE
  state$last <- NULL
  state$initial <- ""
  env <- list2env(list(x = 1, y = 2, "two words" = 3))
  dispatch <- function(text) prismrepl:::.repl_command(text, state, env)
  expect_output(dispatch(':rm "two words"'), "Removed 1 objects.")
  expect_false(exists("two words", env, inherits = FALSE))
  expect_true(exists("x", env, inherits = FALSE))
  expect_output(dispatch(":theme paper"), "Theme: paper")
  expect_identical(state$config$theme$name, "paper")
  dispatch(":pipe %>%")
  expect_identical(state$config$pipe, "%>%")
  dispatch(":sidebar off")
  expect_false(state$config$sidebar)
  expect_error(dispatch(":sidebar sometimes"))
  dispatch(":recall 2")
  expect_identical(state$initial, "x + 2")
  dispatch(":status memory objects")
  expect_setequal(state$config$status, c("memory", "objects"))
  expect_error(dispatch(":recall 99"))
  expect_error(dispatch(":doesnotexist"))
  state$last <- list(error = NULL)
  state$last_error <- list(error = simpleError("remembered"), code = "stop('remembered')", trace = "outer()")
  expect_output(dispatch(":explain minimal"), "remembered")
  dispatch(":quit")
  expect_false(state$running)
})

test_that("help shorthand does not swallow surrounding R expressions", {
  expect_identical(prismrepl:::.help_query("?mean"), "mean")
  expect_identical(prismrepl:::.help_query("?stats::lm"), "stats::lm")
  expect_null(prismrepl:::.help_query("?mean; x <- 1"))
  expect_null(prismrepl:::.help_query("??mean"))
  expect_null(prismrepl:::.help_query("?mean + x"))
})

test_that("panel panning preserves whole graphemes and empty selection is safe", {
  expect_identical(prismrepl:::.panel_slice("a\u6f22b", 1L, 2L), "\u6f22")
  expect_identical(prismrepl:::.panel_slice("e\u0301xyz", 0L, 1L), "e\u0301")
  expect_identical(prismrepl:::.panel_slice("abc", 100L, 5L), "")
  expect_identical(prismrepl:::.tui_panel(character(), "empty", repl_config(), logical()), integer())
})

test_that("diagnostics and command lists have stable useful schemas", {
  expect_named(repl_commands(), c("command", "description"))
  expect_named(repl_diagnostics(), c("setting", "value"))
  expect_true(any(repl_diagnostics()$setting == "platform"))
  expect_identical(prismrepl:::.session_status(new.env(), repl_config(status = character())), character())
})
