test_that("console shortcuts open the complete effective keymap and preserve the draft", {
  cfg <- repl_config(theme = "black", color = FALSE, settings_file = NULL)
  cfg$keybindings$help <- "F3"
  cfg$keybindings$newline <- "Alt+N"
  state <- .editor_state("x <- '\u6f22\u5b57'\nbase::mean(c(1, 2))", history = "1 + 1")
  state$cursor <- 14L
  state$completion <- list(candidates = c("mean", "median"), index = 1L)
  state$overlay <- "A previous completion hint"
  state$search <- list(query = "mean", index = 2L)
  original <- state
  handle <- methods::new("externalptr")
  seen <- NULL
  local_mocked_bindings(.tui_panel = function(lines, title, config, formatter, ...) {
    seen <<- list(title = title, config = config, prepared = formatter(76L))
    invisible(NULL)
  })
  result <- .editor_open_shortcuts(state, cfg, handle)
  expect_identical(seen$title, "Keyboard shortcuts")
  expect_identical(seen$config$viewer_handle, handle)
  expect_false(seen$config$viewer_alternate)
  expect_equal(seen$prepared$count, nrow(keymap(cfg)))
  expect_identical(unique(seen$prepared$records), seq_len(nrow(keymap(cfg))))
  shown <- paste(seen$prepared$lines, collapse = "\n")
  expect_match(shown, "F3", fixed = TRUE)
  expect_match(shown, "Alt+N", fixed = TRUE)
  expect_match(shown, "Scroll it down by one line.", fixed = TRUE)
  expect_false(grepl("More: :keys", shown, fixed = TRUE))
  expect_true(result$clear)
  result$clear <- original$clear
  expect_identical(result, original)
})

test_that("inline shortcut help borrows input and accepts partial configurations", {
  state <- .editor_state("mean(1:3)")
  state$cursor <- 2L
  seen <- NULL
  local_mocked_bindings(.tui_table = function(data, title, config) {
    seen <<- list(data = data, title = title, config = config)
    invisible(NULL)
  })
  result <- .editor_open_shortcuts(state, list(color = FALSE), handle = TRUE, alternate = TRUE)
  expect_s3_class(seen$config, "prismrepl_config")
  expect_true(seen$config$viewer_handle)
  expect_true(seen$config$viewer_alternate)
  expect_identical(seen$data, keymap(seen$config))
  expect_identical(result, state)
  local_mocked_bindings(.tui_table = function(...) stop("Viewer\nunavailable"))
  failed <- .editor_open_shortcuts(state, list(color = FALSE), handle = TRUE)
  expect_identical(failed$chars, state$chars)
  expect_identical(failed$cursor, state$cursor)
  expect_match(failed$overlay, "Cannot open shortcuts: Viewer unavailable", fixed = TRUE)
  expect_true(failed$clear)
})

test_that("the live console dispatches F2 and a remapped help key directly to the viewer", {
  run <- function(full, key) {
    cfg <- repl_config(theme = "black", color = FALSE, settings_file = NULL)
    cfg$initial_text <- "x <- 1\nmean(1:3)"
    cfg$keybindings$help <- toupper(key)
    cfg$key_lookup <- .key_lookup(cfg$keybindings)
    if (full) {
      cfg$screen <- new.env(parent = emptyenv())
      cfg$screen$active <- TRUE
    }
    events <- list(list(type = "key", key = key, text = "", modifiers = character()),
      list(type = "key", key = "enter", text = "", modifiers = character()))
    index <- 0L
    opened <- closed <- viewed <- 0L
    seen <- NULL
    local_mocked_bindings(
      term_open = function(...) { opened <<- opened + 1L; TRUE },
      term_close = function(...) { closed <<- closed + 1L; invisible(NULL) },
      term_size = function(...) c(width = 80L, height = 24L),
      term_read = function(...) { index <<- index + 1L; events[[index]] },
      term_write = function(...) invisible(NULL),
      .editor_display_frame = function(state, ...) list(lines = "draft", cursor_row = 0L,
        cursor_column = 0L),
      .editor_draw = function(...) invisible(NULL),
      screen_render = function(...) invisible(NULL),
      screen_invalidate = function(...) invisible(NULL),
      .mouse_dispatch = function(event, frame, selection, config) list(event = event,
        selection = selection, status = "", redraw = FALSE),
      .editor_open_shortcuts = function(state, config, handle, alternate) {
        viewed <<- viewed + 1L
        seen <<- list(state = state, handle = handle, alternate = alternate)
        state
      })
    result <- read_expression(config = cfg)
    expect_identical(result, list(action = "submit", text = cfg$initial_text))
    expect_identical(viewed, 1L)
    expect_identical(opened, 1L)
    expect_identical(closed, 1L)
    expect_true(seen$handle)
    expect_identical(seen$alternate, !full)
    expect_identical(paste0(seen$state$chars, collapse = ""), cfg$initial_text)
    expect_identical(seen$state$action, "edit")
    expect_length(seen$state$overlay, 0L)
  }
  invisible(lapply(c(TRUE, FALSE), function(full)
    lapply(c("f2", "f3"), function(key) run(full, key))))
})
