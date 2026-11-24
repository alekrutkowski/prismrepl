mouse_event <- function(x = 1L, y = 1L, button = "left", action = "press", modifiers = character())
  list(type = "mouse", key = button, text = "", x = as.integer(x), y = as.integer(y),
       button = button, action = action, modifiers = modifiers, truncated = FALSE)

test_that("mouse clicks are staged on release and drags never activate a row", {
  config <- repl_config(settings_file = NULL, color = FALSE)
  frame <- c("first row", "second row")
  press <- mouse_event(3, 1)
  started <- .mouse_dispatch(press, frame, .mouse_state(), config)
  expect_null(started$event)
  expect_true(started$selection$down)
  clicked <- .mouse_dispatch(mouse_event(3, 1, action = "release"), frame, started$selection, config)
  expect_identical(clicked$event, press)
  expect_false(clicked$selection$down)
  copied <- character()
  local_mocked_bindings(.clipboard_write = function(text) {
    copied <<- text
    list(ok = TRUE, reason = "Copied test selection")
  })
  moved <- .mouse_dispatch(mouse_event(5, 2, action = "move"), frame, started$selection, config)
  expect_null(moved$event)
  expect_true(moved$redraw)
  expect_identical(.mouse_text(moved$selection), "rst row\nsecon")
  released <- .mouse_dispatch(mouse_event(5, 2, action = "release"), frame, moved$selection, config)
  expect_null(released$event)
  expect_identical(copied, "rst row\nsecon")
  expect_false(released$selection$down)
  expect_identical(released$status, "Copied test selection")
})

test_that("Ctrl drag selects a rectangle in both directions and preserves spaces", {
  config <- repl_config(settings_file = NULL, color = FALSE)
  frame <- c("abcdef", "uvwxyz", "12  56")
  selected <- .mouse_dispatch(mouse_event(4, 3, modifiers = "ctrl"), frame, .mouse_state(), config)$selection
  selected <- .mouse_dispatch(mouse_event(2, 1, action = "move"), frame, selected, config)$selection
  expect_true(selected$rectangle)
  expect_identical(.mouse_text(selected), "bcd\nvwx\n2  ")
  expect_equal(.mouse_ranges(selected), data.frame(row = 1:3, first = 2, last = 4))
})

test_that("Unicode selection keeps combining marks and wide characters whole", {
  config <- repl_config(settings_file = NULL, color = TRUE, color_depth = 16777216L)
  line <- paste0(style_text("A\u754ce\u0301\U0001f642Z", "string", config$theme, TRUE), "  ")
  frame <- structure(c(line, "second"), special = "retained")
  selection <- .mouse_dispatch(mouse_event(3, 1), frame, .mouse_state(), config)$selection
  selection <- .mouse_dispatch(mouse_event(5, 1, action = "move"), frame, selection, config)$selection
  expect_identical(.mouse_text(selection), "\u754ce\u0301\U0001f642")
  painted <- .mouse_paint(frame, selection, config)
  expect_identical(attr(painted, "special"), "retained")
  expect_identical(vapply(painted, .screen_width, numeric(1L)), vapply(frame, .screen_width, numeric(1L)))
  expect_identical(gsub(.screen_ansi_pattern(), "", painted, perl = TRUE),
                   gsub(.screen_ansi_pattern(), "", frame, perl = TRUE))
  expect_match(painted[[1L]], config$theme$colors[["selection"]], fixed = TRUE)
  wrapped <- list(lines = frame, layout = list(width = 30L, height = 2L), arbitrary = 42)
  expect_identical(.mouse_paint(wrapped, selection, config)$arbitrary, 42)
  expect_identical(.mouse_paint(wrapped, NULL, config), wrapped)
})

test_that("new actions and resizes clear selection without changing their event", {
  config <- repl_config(settings_file = NULL)
  frame <- c("abc", "def")
  selected <- .mouse_dispatch(mouse_event(1, 1), frame, .mouse_state(), config)$selection
  selected <- .mouse_dispatch(mouse_event(2, 2, action = "move"), frame, selected, config)$selection
  event <- list(type = "resize")
  result <- .mouse_dispatch(event, frame, selected, config)
  expect_identical(result$event, event)
  expect_true(result$redraw)
  expect_false(result$selection$moved)
  idle <- .mouse_dispatch(list(type = "timeout"), frame, selected, config)
  expect_identical(idle$selection, selected)
  config$mouse <- FALSE
  expect_false(.mouse_dispatch(mouse_event(), frame, selected, config)$selection$moved)
})

test_that("selection never contaminates the cached unpainted screen", {
  config <- repl_config(settings_file = NULL, color = TRUE, color_depth = 16777216L)
  screen <- screen_state()
  screen$active <- TRUE
  config$screen <- screen
  local_mocked_bindings(term_write = function(text, synchronize = FALSE, cursor = NULL) invisible(nchar(text)))
  editor <- list(lines = "r> sample", cursor_row = 0L, cursor_column = 9L)
  original <- screen_render(screen, editor, config, c(width = 90L, height = 24L))
  selected <- .mouse_dispatch(mouse_event(2, 1), original, .mouse_state(), config)$selection
  selected <- .mouse_dispatch(mouse_event(6, 1, action = "move"), original, selected, config)$selection
  screen$mouse_selection <- selected
  painted <- screen_render(screen, editor, config, c(width = 90L, height = 24L))
  expect_false(identical(painted$lines, original$lines))
  expect_identical(screen$last_base_frame$lines, original$lines)
  screen$mouse_selection <- .mouse_state()
  restored <- screen_render(screen, editor, config, c(width = 90L, height = 24L))
  expect_identical(restored$lines, original$lines)
  expect_identical(screen$last_frame$lines, original$lines)
})

test_that("right click produces only a paste event and respects failure status", {
  config <- repl_config(settings_file = NULL)
  local_mocked_bindings(.clipboard_read = function(limit)
    list(ok = TRUE, text = "1 + 1\nstop('not executed')", reason = ""))
  result <- .mouse_dispatch(mouse_event(button = "right"), "body", .mouse_state(), config)
  expect_identical(result$event$type, "paste")
  expect_identical(result$event$text, "1 + 1\nstop('not executed')")
  state <- .editor_step(.editor_state(), result$event, character(), new.env(), config)
  expect_identical(state$action, "edit")
  expect_identical(paste(state$chars, collapse = ""), result$event$text)
  local_mocked_bindings(.clipboard_read = function(limit)
    list(ok = FALSE, text = "", reason = "Clipboard unavailable"))
  result <- .mouse_dispatch(mouse_event(button = "right"), "body", .mouse_state(), config)
  expect_null(result$event)
  expect_identical(result$status, "Clipboard unavailable")
})

test_that("OSC 52 encoding is UTF-8 and padding correct", {
  expect_identical(vapply(c("", "f", "fo", "foo", "foob", "fooba", "foobar"),
    .clipboard_base64, character(1L), USE.NAMES = FALSE),
    c("", "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy"))
  expect_identical(.clipboard_base64("\u754ce\u0301\U0001f642"), "55WMZcyB8J+Zgg==")
})

test_that("clipboard providers are consulted only on explicit copy or paste", {
  old <- list(text = .clipboard_state$text, has_text = .clipboard_state$has_text)
  on.exit({ .clipboard_state$text <- old$text; .clipboard_state$has_text <- old$has_text }, add = TRUE)
  local_mocked_bindings(.clipboard_program = function(action) NULL,
    term_output_state = function() list(supported = FALSE))
  skip_on_os("windows")
  .clipboard_state$has_text <- FALSE
  expect_false(.clipboard_read()$ok)
  expect_identical(.clipboard_write("\u03b1\nhello")$backend, "session")
  expect_identical(.clipboard_read()$text, "\u03b1\nhello")
  expect_match(.clipboard_read()$reason, "Prism selection", fixed = TRUE)
  expect_false(.clipboard_read(limit = 2L)$ok)
})
