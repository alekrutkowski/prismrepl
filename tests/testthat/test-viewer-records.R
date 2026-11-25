test_that("command descriptions remain paired with their commands", {
  data <- data.frame(command = c(":view NAME", ":theme NAME"), description = c(
    "Open the named object. Enter opens a child, and Left returns to the parent.",
    "Switch the current theme and repaint the terminal."), stringsAsFactors = FALSE)
  wide <- .table_format(data, 90L)
  expect_match(wide$lines[[1L]], ":view NAME +Open the named object")
  expect_match(wide$lines[which(wide$records == 2L)[[1L]]], ":theme NAME +Switch the current theme")
  narrow <- .table_format(data, 30L)
  expect_identical(narrow$lines[[1L]], ":view NAME")
  expect_match(narrow$lines[[2L]], "^  Open the named object")
  expect_true(all(diff(narrow$records) >= 0L))
  expect_true(all(narrow$records[grepl("parent", narrow$lines)] == 1L))
  expect_true(all(narrow$records[grepl("repaint", narrow$lines)] == 2L))
})

test_that("record formatting preserves Unicode and fits tiny through wide windows", {
  data <- data.frame(command = c(":view \u754c", ":doc caf\u00e9"),
    description = c("Inspect e\u0301 and \U0001f469\u200d\U0001f4bb safely.",
                    "Read a function's documentation without losing its description."))
  invisible(lapply(c(1L, 3L, 5L, 8L, 20L, 40L, 80L, 132L), function(width) {
    formatted <- .table_format(data, width)
    expect_true(all(.panel_cells(formatted$lines) <= width))
    expect_identical(unique(formatted$records), 1:2)
  }))
  formatted <- .table_format(data, 40L)
  expect_true(any(grepl("e\u0301", formatted$lines, fixed = TRUE)))
  expect_true(any(grepl("\U0001f469\u200d\U0001f4bb", formatted$lines, fixed = TRUE)))
})

test_that("cached viewer moves change only selected rows and retain identical output", {
  cfg <- repl_config(color = TRUE, color_depth = 16777216L, settings_file = NULL)
  lines <- paste("Record", seq_len(1000L), strrep("detail ", 8L))
  size <- c(width = 100L, height = 24L)
  cache <- new.env(parent = emptyenv())
  first <- .panel_frame(lines, "Viewer", cfg, size, cursor = 1L, cache = cache)
  saved <- cache$normal
  second <- .panel_frame(lines, "Viewer", cfg, size, cursor = 2L, cache = cache)
  expect_identical(cache$normal, saved)
  expect_identical(second, .panel_frame(lines, "Viewer", cfg, size, cursor = 2L))
  expect_identical(which(first != second), 2:3)
  old <- list(lines = first, cursor_row = 0L, cursor_column = 0L,
              layout = list(width = 100L, height = 24L))
  new <- old
  new$lines <- second
  update <- .screen_build_update(new, old, cfg, show_cursor = FALSE)
  expect_match(update, "\033[2;1H", fixed = TRUE)
  expect_match(update, "\033[3;1H", fixed = TRUE)
  expect_false(grepl("\033[4;1H", update, fixed = TRUE))
  expect_identical(.screen_build_update(new, new, cfg, show_cursor = FALSE), "")
})

test_that("viewer drawing writes one assembled update and skips unchanged frames", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  lines <- .panel_frame(c("one", "two"), "Viewer", cfg, c(width = 40L, height = 8L))
  calls <- list()
  env <- new.env(parent = environment(.panel_draw))
  env$term_write <- function(text, synchronize = FALSE, cursor = NULL) {
    calls[[length(calls) + 1L]] <<- text
    invisible(NULL)
  }
  draw <- .panel_draw
  environment(draw) <- env
  frame <- draw(lines, NULL, c(width = 40L, height = 8L), cfg)
  expect_length(calls, 1L)
  expect_false(grepl("\n", calls[[1L]], fixed = TRUE))
  draw(lines, frame, c(width = 40L, height = 8L), cfg)
  expect_length(calls, 1L)
})

test_that("viewer navigation honors remapped and disabled common bindings", {
  cfg <- repl_config(color = FALSE, settings_file = NULL,
    keybindings = list(down = "F4", up = character()))
  event <- function(key) list(type = "key", key = key, text = "", modifiers = character())
  expect_identical(.viewer_action(event("f4"), cfg), "down")
  expect_identical(.viewer_action(event("down"), cfg), "")
  expect_identical(.viewer_action(event("up"), cfg), "")
  expect_identical(.viewer_action(event("tab"), cfg), "tab")
  help <- .panel_help(cfg, 90L, FALSE, TRUE)
  expect_true(any(grepl("F4", help, fixed = TRUE)))
  expect_false(any(grepl("Up/Down", help, fixed = TRUE)))
  expect_match(.viewer_caption(c("very long optional detail", "F2 keys", "q close"), 20L),
               "q close$", perl = TRUE)
})
test_that("blank-space clicks do not select a last batch row", {
  events <- list(
    list(type = "mouse", key = "left", button = "left", action = "press", x = 3L, y = 8L,
         text = "", modifiers = character()),
    list(type = "mouse", key = "left", button = "left", action = "release", x = 3L, y = 8L,
         text = "", modifiers = character()),
    list(type = "key", key = "enter", text = "", modifiers = character()))
  i <- 0L
  local_mocked_bindings(
    .viewer_start = function(config) list(handle = TRUE, guard = NULL, shared = FALSE),
    .viewer_finish = function(...) invisible(NULL),
    .panel_draw = function(...) NULL,
    term_size = function() c(width = 80L, height = 12L),
    term_read = function(...) { i <<- i + 1L; events[[i]] })
  out <- .tui_panel("sole record", "Batch selection", repl_config(settings_file = NULL), selected = FALSE)
  expect_identical(out, integer())
})

