.selection_test_menu <- function(answers, selected, lines = c("Alpha", "Beta", "Gamma")) {
  frames <- list()
  asked <- 0L
  result <- .panel_plain_selection(lines, "Options", selected,
    ask = function(choices, graphics, title) {
      asked <<- asked + 1L
      if (asked > length(answers)) stop("Unexpected extra menu prompt")
      frames[[asked]] <<- list(choices = choices, graphics = graphics, title = title)
      answers[[asked]]
    })
  list(value = result, frames = frames, asked = asked)
}

test_that("plain batch menus preserve the current selection until Apply", {
  current <- c(TRUE, FALSE, TRUE)
  out <- .selection_test_menu(1L, current)
  expect_identical(out$value, c(1L, 3L))
  expect_identical(out$asked, 1L)
  expect_identical(out$frames[[1L]]$choices,
    c("Apply selection", "[x] Alpha", "[ ] Beta", "[x] Gamma"))
  expect_false(out$frames[[1L]]$graphics)
  expect_match(out$frames[[1L]]$title, "0 cancels", fixed = TRUE)
  expect_identical(current, c(TRUE, FALSE, TRUE))
})

test_that("plain batch rows toggle without accepting and zero cancels", {
  current <- c(TRUE, FALSE, TRUE)
  out <- .selection_test_menu(c(2L, 3L, 0L), current)
  expect_null(out$value)
  expect_identical(out$asked, 3L)
  expect_identical(out$frames[[2L]]$choices[[2L]], "[ ] Alpha")
  expect_identical(out$frames[[3L]]$choices[[3L]], "[x] Beta")
  expect_identical(current, c(TRUE, FALSE, TRUE))
  expect_null(.selection_test_menu(0L, current)$value)
  out <- .selection_test_menu(c(2L, 3L, 4L, 1L), rep(FALSE, 3L))
  expect_identical(out$value, 1:3)
  expect_identical(out$asked, 4L)
  out <- .selection_test_menu(c(2L, 3L, 4L, 1L), rep(TRUE, 3L))
  expect_identical(out$value, integer())
  out <- .selection_test_menu(c(3L, 3L, 1L), current)
  expect_identical(out$value, c(1L, 3L))
})

test_that("plain batch menus accept empty selections and sanitize labels", {
  out <- .selection_test_menu(1L, logical(), character())
  expect_identical(out$value, integer())
  expect_identical(out$frames[[1L]]$choices, "Apply selection")
  expect_null(.selection_test_menu(0L, logical(), character())$value)
  out <- .selection_test_menu(1L, c(FALSE, TRUE),
    c("\033[31mAlpha\033[0m", "Beta\ncontinued"))
  expect_identical(out$value, 2L)
  expect_identical(out$frames[[1L]]$choices,
    c("Apply selection", "[ ] Alpha", "[x] Beta continued"))
  expect_error(.panel_plain_selection("Alpha", "Options", logical(), ask = function(...) 1L),
    "one nonmissing logical per line", fixed = TRUE)
  expect_error(.panel_plain_selection("Alpha", "Options", NA, ask = function(...) 1L),
    "one nonmissing logical per line", fixed = TRUE)
  expect_error(.panel_plain_selection("Alpha", "Options", TRUE, ask = function(...) 3L),
    "valid item", fixed = TRUE)
  expect_error(.panel_plain_selection("Alpha", "Options", TRUE, ask = function(...) 1.5),
    "valid item", fixed = TRUE)
})

test_that("interactive batch panels use numeric selection when no terminal is available", {
  cfg <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  received <- list()
  finished <- FALSE
  env <- new.env(parent = environment(.tui_panel))
  env$interactive <- function() TRUE
  env$.viewer_start <- function(config) list(handle = NULL, guard = NULL, shared = FALSE)
  env$.viewer_finish <- function(...) { finished <<- TRUE; invisible(NULL) }
  env$term_size <- function() c(width = 80L, height = 24L)
  env$.panel_plain_selection <- function(lines, title, selected) {
    received <<- list(lines = lines, title = title, selected = selected)
    2L
  }
  env$.emit <- function(...) stop("An interactive menu must not fall back to plain output")
  panel <- .tui_panel
  environment(panel) <- env
  expect_identical(panel(c("Alpha", "Beta"), "Options", cfg, selected = c(TRUE, FALSE)), 2L)
  expect_identical(received, list(lines = c("Alpha", "Beta"), title = "Options",
    selected = c(TRUE, FALSE)))
  expect_true(finished)
})

test_that("noninteractive batch panels print generic guidance without changing selection", {
  cfg <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  current <- c(TRUE, FALSE)
  text <- capture.output(result <- .tui_panel(c("Alpha", "Beta"), "Options", cfg, selected = current))
  expect_null(result)
  expect_identical(current, c(TRUE, FALSE))
  expect_true(any(grepl("Use explicit command arguments in a plain console.", text, fixed = TRUE)))
  expect_false(any(grepl(":rm", text, fixed = TRUE)))
})
