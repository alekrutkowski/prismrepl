.theme_test_event <- function(key = "", text = "", type = if (nzchar(text)) "text" else "key", ...) {
  c(list(type = type, key = key, text = text, modifiers = character()), list(...))
}

.theme_test_panel <- function(events, initial = 1L, width = 80L, resize_width = width,
                              config = repl_config(color = FALSE, settings_file = NULL)) {
  data <- data.frame(theme = c("midnight", "white", "black"),
    mode = c("dark", "light", "dark"), background = c("#0B1020", "#FFFFFF", "#000000"))
  frames <- list()
  read <- 0L
  sized <- 0L
  env <- new.env(parent = environment(.tui_panel))
  env$.viewer_start <- function(config) list(handle = TRUE, guard = NULL, shared = FALSE)
  env$.viewer_finish <- function(...) invisible(NULL)
  env$term_size <- function() {
    sized <<- sized + 1L
    c(width = if (sized == 1L) width else resize_width, height = 12L)
  }
  env$term_read <- function(...) {
    read <<- read + 1L
    if (read > length(events)) stop("Unexpected extra terminal read")
    events[[read]]
  }
  env$.panel_draw <- function(lines, previous, size, config) {
    frames[[length(frames) + 1L]] <<- gsub(.screen_ansi_pattern(), "", lines, perl = TRUE)
    list(lines = lines)
  }
  panel <- .tui_panel
  environment(panel) <- env
  out <- panel(character(), "Theme picker", config,
    formatter = function(width) .table_format(data, width), choose = TRUE, initial_record = initial)
  list(value = out, frames = frames, reads = read)
}

test_that("single-choice panels initially focus and accept the current record", {
  result <- .theme_test_panel(list(.theme_test_event("enter")), initial = 2L)
  expect_identical(result$value, 2L)
  expect_identical(result$reads, 1L)
  expect_true(any(grepl("Record 2/3", result$frames[[1L]], fixed = TRUE)))
  expect_true(any(grepl("Enter apply", result$frames[[1L]], fixed = TRUE)))
  expect_true(any(grepl("Escape/q cancel", result$frames[[1L]], fixed = TRUE)))
})

test_that("single-choice navigation and mouse focus do not accept early", {
  result <- .theme_test_panel(list(.theme_test_event("down"), .theme_test_event("escape")))
  expect_null(result$value)
  expect_identical(result$reads, 2L)
  expect_true(any(grepl("Record 2/3", result$frames[[2L]], fixed = TRUE)))
  expect_null(.theme_test_panel(list(.theme_test_event(text = "q")))$value)
  click <- function(action, y) .theme_test_event(type = "mouse", key = "left",
    button = "left", action = action, x = 8L, y = y)
  result <- .theme_test_panel(list(click("press", 4L), click("release", 4L), .theme_test_event("enter")))
  expect_identical(result$value, 3L)
  expect_identical(result$reads, 3L)
  blank <- .theme_test_panel(list(click("press", 9L), click("release", 9L), .theme_test_event("enter")))
  expect_identical(blank$value, 1L)
})

test_that("narrow formatting and resizing preserve the chosen record", {
  result <- .theme_test_panel(list(.theme_test_event("down"), .theme_test_event("enter")),
    initial = 2L, width = 24L)
  expect_identical(result$value, 3L)
  result <- .theme_test_panel(list(.theme_test_event(type = "resize"), .theme_test_event("enter")),
    initial = 2L, width = 80L, resize_width = 24L)
  expect_identical(result$value, 2L)
  expect_true(all(.panel_cells(result$frames[[2L]]) <= 23L))
})

test_that("theme choices retain viewer search and remapped acceptance", {
  result <- .theme_test_panel(list(.theme_test_event(text = "/"), .theme_test_event(text = "black"),
    .theme_test_event("enter"), .theme_test_event("enter")))
  expect_identical(result$value, 3L)
  expect_identical(result$reads, 4L)
  cfg <- repl_config(color = FALSE, settings_file = NULL,
    keybindings = list(down = "F3", submit = "F4", escape = "F6"))
  result <- .theme_test_panel(list(.theme_test_event("down"), .theme_test_event("f3"),
    .theme_test_event("f4")), config = cfg)
  expect_identical(result$value, 2L)
  expect_true(any(grepl("F4 apply", result$frames[[1L]], fixed = TRUE)))
  expect_true(any(grepl("F6/q cancel", result$frames[[1L]], fixed = TRUE)))
  expect_true(any(grepl("Apply the focused record", .panel_help(cfg, 100L, FALSE, TRUE, TRUE), fixed = TRUE)))
})

test_that("theme picker maps labels to names and marks the current theme", {
  cfg <- repl_config(theme = "white", color = FALSE, settings_file = NULL)
  received <- list()
  local_mocked_bindings(.tui_panel = function(lines, title, config, formatter, choose, initial_record) {
    received <<- list(title = title, choose = choose, initial = initial_record, formatted = formatter(24L))
    match("black", repl_themes())
  })
  expect_identical(.theme_picker(cfg), "black")
  expect_true(received$choose)
  expect_identical(received$initial, match("white", repl_themes()))
  expect_match(received$title, "current: white", fixed = TRUE)
  marker <- which(grepl("white (current)", received$formatted$lines, fixed = TRUE))
  expect_length(marker, 1L)
  expect_identical(received$formatted$records[[marker]], match("white", repl_themes()))
  expect_true(all(.panel_cells(received$formatted$lines) <= 24L))
})

test_that("plain choices accept menu numbers and noninteractive themes print a command", {
  data <- data.frame(theme = c("paper", "black"), mode = c("light", "dark"))
  formatted <- .table_format(data, 20L)
  received <- list()
  ask <- function(choices, graphics, title) {
    received <<- list(choices = choices, graphics = graphics, title = title)
    2L
  }
  expect_identical(.panel_plain_choice(formatted$lines, "Themes", formatted$records, ask), 2L)
  expect_length(received$choices, 2L)
  expect_false(received$graphics)
  expect_match(received$title, "0 cancels", fixed = TRUE)
  expect_null(.panel_plain_choice(formatted$lines, "Themes", formatted$records, function(...) 0L))
  cfg <- repl_config(theme = "white", color = FALSE, settings_file = NULL)
  text <- capture.output(result <- .theme_picker(cfg))
  expect_null(result)
  expect_true(any(grepl("white (current)", text, fixed = TRUE)))
  expect_true(any(grepl(":theme NAME", text, fixed = TRUE)))
})

test_that("accepted theme choices share explicit-command saving and full redraw", {
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  state$config$screen <- new.env(parent = emptyenv())
  state$config$screen$active <- TRUE
  state$native <- TRUE
  state$running <- TRUE
  chosen <- NULL
  picks <- invalidated <- rendered <- 0L
  saved <- list()
  local_mocked_bindings(
    .theme_picker = function(...) { picks <<- picks + 1L; chosen },
    .persist_preferences = function(config, fields, ...) {
      saved[[length(saved) + 1L]] <<- list(theme = config$theme$name, fields = fields)
      TRUE
    },
    screen_invalidate = function(...) { invalidated <<- invalidated + 1L; invisible(NULL) },
    screen_render = function(...) { rendered <<- rendered + 1L; invisible(NULL) },
    .emit = function(...) invisible(NULL))
  .repl_command(":theme", state, new.env())
  expect_identical(state$config$theme$name, "midnight")
  expect_length(saved, 0L)
  expect_equal(invalidated, 0L)
  expect_equal(rendered, 0L)
  chosen <- "white"
  .repl_command(":theme", state, new.env())
  expect_identical(state$config$theme$name, "white")
  expect_identical(saved[[1L]], list(theme = "white", fields = "theme"))
  expect_equal(invalidated, 1L)
  expect_equal(rendered, 1L)
  .repl_command(":theme black", state, new.env())
  expect_equal(picks, 2L)
  expect_identical(state$config$theme$name, "black")
  expect_identical(saved[[2L]], list(theme = "black", fields = "theme"))
  expect_equal(invalidated, 2L)
  expect_equal(rendered, 2L)
})
