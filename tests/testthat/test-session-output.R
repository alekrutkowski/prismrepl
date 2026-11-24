test_that("fullscreen captures print output and preserves condition ordering", {
  cfg <- repl_config(color = TRUE, color_depth = 16777216L, hyperlinks = FALSE)
  cfg$screen <- screen_state(max_lines = 200L)
  before <- sink.number()
  printed <- capture.output(result <- .with_screen_output(function(local) evaluate_r(
    'cat("FIRST\\n"); message("NOTICE"); cat("SECOND\\n"); warning("CAREFUL"); stop("BROKEN")',
    new.env(parent = baseenv()), local), cfg, paint = FALSE))
  expect_length(printed, 0L)
  expect_identical(sink.number(), before)
  expect_s3_class(result$error, "error")
  lines <- cli::ansi_strip(unlist(lapply(cfg$screen$entries, `[[`, "lines")))
  locate <- function(text) which(grepl(text, lines, fixed = TRUE))[1L]
  positions <- vapply(c("FIRST", "NOTICE", "SECOND", "CAREFUL", "BROKEN"), locate, integer(1L))
  expect_true(all(diff(positions) > 0L))
  expect_true(any(grepl("[MESSAGE]", lines, fixed = TRUE)))
  expect_true(any(grepl("[WARNING]", lines, fixed = TRUE)))
  expect_true(any(grepl("[ERROR]", lines, fixed = TRUE)))
})

test_that("fullscreen capture restores sinks after errors and bounds retained output", {
  cfg <- repl_config(color = FALSE)
  cfg$screen <- screen_state(max_lines = 30L)
  before <- sink.number()
  expect_error(.with_screen_output(function(local) stop("unexpected"), cfg, paint = FALSE), "unexpected")
  expect_identical(sink.number(), before)
  .with_screen_output(function(local) cat(paste(rep("large output line", 1000L), collapse = "\n")),
                      cfg, paint = FALSE, max_bytes = 1024L)
  lines <- unlist(lapply(cfg$screen$entries, `[[`, "lines"))
  expect_lte(length(lines), 30L)
  expect_lt(sum(nchar(lines, type = "bytes")), 1200L)
  expect_identical(sink.number(), before)
})

test_that("captured terminal controls cannot clear or overwrite the TUI", {
  cfg <- repl_config(color = TRUE)
  cfg$screen <- screen_state()
  .with_screen_output(function(local) cat("hello\033[2J\033]52;c;payload\007world\n"),
                      cfg, paint = FALSE)
  lines <- unlist(lapply(cfg$screen$entries, `[[`, "lines"))
  expect_false(any(grepl("\033", lines, fixed = TRUE)))
  expect_true(any(grepl("hello", lines, fixed = TRUE)))
})

test_that("sidebar inventory never forces deferred or active bindings", {
  env <- new.env(parent = baseenv())
  env$table <- data.frame(x = 1:3, y = 4:6)
  delayedAssign("later", stop("must not force"), assign.env = env)
  makeActiveBinding("active", function() stop("must not invoke"), env)
  cfg <- repl_config(sidebar = TRUE)
  sidebar <- .session_sidebar(env, cfg)
  expect_true(any(grepl("3 x 2", sidebar$WORKSPACE, fixed = TRUE)))
  expect_true(any(grepl("promise", sidebar$WORKSPACE, fixed = TRUE)))
  expect_true(any(grepl("active", sidebar$WORKSPACE, fixed = TRUE)))
  cfg$sidebar <- FALSE
  expect_identical(.session_sidebar(env, cfg), list())
})
