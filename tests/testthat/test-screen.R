screen_fixture <- function(color = FALSE) {
  config <- repl_config(color = color)
  config$sidebar <- TRUE
  config$editor_height <- 8L
  screen <- screen_state()
  screen$status <- "cwd: /project  |  objects: 3  |  git: main"
  screen$sidebar <- list(Objects = c("iris  data.frame  150 x 5", "x  numeric  1"),
                         Packages = c("base", "stats"), Project = "/project")
  list(screen = screen, config = config)
}

screen_plain <- function(text) gsub(.screen_ansi_pattern(), "", text, perl = TRUE)

screen_test_renderer <- function() {
  calls <- new.env(parent = emptyenv())
  calls$text <- character()
  calls$cursor <- list()
  render <- screen_render
  env <- new.env(parent = environment(render))
  env$term_write <- function(text, synchronize = FALSE, cursor = NULL) {
    calls$text <- c(calls$text, text)
    calls$cursor <- c(calls$cursor, list(cursor))
    invisible(NULL)
  }
  environment(render) <- env
  list(render = render, calls = calls)
}

test_that("fullscreen state is local and transcript retention is bounded", {
  screen <- screen_state(max_lines = 5L)
  expect_false(screen$active)
  screen_append(screen, c("one", "two", "three"))
  screen_append(screen, c("four", "five", "six", "seven"))
  expect_identical(unlist(lapply(screen$entries, `[[`, "lines"), use.names = FALSE),
                   c("three", "four", "five", "six", "seven"))
  screen$scroll <- 2L
  screen_append(screen, "eight")
  expect_equal(screen$scroll, 0L)
  screen_clear(screen)
  expect_length(screen$entries, 0L)
  expect_length(ls(screen$cache, all.names = TRUE), 0L)
})

test_that("only SGR and web OSC8 sequences survive transcript sanitization", {
  source <- paste0("\033[31mred\033[0m", "\033[2J", "\033]52;c;clipboard\007")
  safe <- .screen_safe_ansi(source)
  expect_true(grepl("\033[31m", safe, fixed = TRUE))
  expect_false(grepl("\033[2J", safe, fixed = TRUE))
  expect_false(grepl("\033]52", safe, fixed = TRUE))
  expect_match(safe, "clipboard", fixed = TRUE)
  link <- "\033]8;;https://chat.mistral.ai/chat?q=example\033\\Explain this error\033]8;;\033\\"
  expect_identical(.screen_safe_ansi(link), link)
  rows <- .screen_wrap(link, 8L)
  expect_true(all(grepl("\033]8;;https://", rows, fixed = TRUE)))
  expect_identical(paste0(screen_plain(rows), collapse = ""), "Explain this error")
})

test_that("native ANSI clipping and wrapping retain color regardless of cli settings", {
  withr::local_envvar(NO_COLOR = "1")
  withr::local_options(cli.num_colors = 1L)
  text <- paste0("\033[38;2;0;180;255m", "\u754ce\u0301abc", "\033[0m")
  rows <- .screen_wrap(text, 3L)
  expect_true(all(vapply(rows, .screen_width, numeric(1)) <= 3L))
  expect_identical(paste0(screen_plain(rows), collapse = ""), "\u754ce\u0301abc")
  expect_true(all(grepl("\033[38;2;0;180;255m", rows, fixed = TRUE)))
  selected <- .screen_grapheme_slice(text, 2L, 3L)
  expect_identical(screen_plain(selected), "e\u0301a")
  expect_true(grepl("\033[38;2;0;180;255m", selected, fixed = TRUE))
  expect_equal(.screen_width(.screen_fit(text, 4L)), 4L)
  many <- paste0(rep("\033[31mx\033[0m", 1000L), collapse = "")
  clipped <- .screen_grapheme_slice(many, 1000L, 1000L)
  expect_identical(screen_plain(clipped), "x")
  expect_lt(nchar(clipped), 100L)
})

test_that("wide layouts show the sidebar and narrow or short layouts hide it", {
  fixture <- screen_fixture()
  screen <- fixture$screen
  config <- fixture$config
  expect_true(.screen_layout(screen, config, 132L, 30L)$sidebar)
  expect_false(.screen_layout(screen, config, 80L, 30L)$sidebar)
  expect_false(.screen_layout(screen, config, 132L, 10L)$sidebar)
  config$sidebar <- FALSE
  expect_false(.screen_layout(screen, config, 132L, 30L)$sidebar)
  expect_equal(screen_content_width(screen, config, c(width = 80L, height = 24L)), 77L)
})

test_that("the compositor is pure and every tested size retains a valid editor cursor", {
  fixture <- screen_fixture()
  screen_append(fixture$screen, paste("Transcript", seq_len(40L)))
  editor <- list(lines = c("r> {", "+    mean(iris$Sepal.Length)", "+  }"),
                 cursor_row = 2L, cursor_column = 4L)
  previous <- as.list.environment(fixture$screen, all.names = TRUE)
  sizes <- expand.grid(width = c(1L, 8L, 23L, 40L, 80L, 110L, 132L),
                       height = c(1L, 3L, 6L, 8L, 14L, 30L))
  invisible(lapply(seq_len(nrow(sizes)), function(i) {
    frame <- .screen_compose(fixture$screen, editor, fixture$config,
                             sizes$width[[i]], sizes$height[[i]])
    expect_length(frame$lines, sizes$height[[i]])
    expect_true(all(vapply(frame$lines, .screen_width, numeric(1)) <= max(1L, sizes$width[[i]] - 1L)))
    expect_true(frame$cursor_row >= 0L && frame$cursor_row < sizes$height[[i]])
    expect_true(frame$cursor_column >= 0L && frame$cursor_column < sizes$width[[i]])
  }))
  expect_identical(as.list.environment(fixture$screen, all.names = TRUE), previous)
})

test_that("transcript scrolling changes output without changing the editor", {
  fixture <- screen_fixture()
  screen_append(fixture$screen, sprintf("OUTPUT_%02d", seq_len(50L)))
  editor <- list(lines = "r> unfinished", cursor_row = 0L, cursor_column = 13L)
  live <- .screen_compose(fixture$screen, editor, fixture$config, 80L, 20L)
  fixture$screen$scroll <- 15L
  scrolled <- .screen_compose(fixture$screen, editor, fixture$config, 80L, 20L)
  expect_true(any(grepl("OUTPUT_50", screen_plain(live$lines), fixed = TRUE)))
  expect_false(any(grepl("OUTPUT_50", screen_plain(scrolled$lines), fixed = TRUE)))
  expect_true(any(grepl("lines above live", screen_plain(scrolled$lines), fixed = TRUE)))
  expect_identical(live$lines[[live$layout$editor_start + 1L]],
                   scrolled$lines[[scrolled$layout$editor_start + 1L]])
  expect_identical(live$cursor_row, scrolled$cursor_row)
  expect_identical(live$cursor_column, scrolled$cursor_column)
})

test_that("fullscreen repaint emits absolute cursor addressing and diffs unchanged rows", {
  fixture <- screen_fixture(color = TRUE)
  fixture$screen$active <- TRUE
  editor <- list(lines = "r> 1 + 1", cursor_row = 0L, cursor_column = 8L)
  renderer <- screen_test_renderer()
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  first <- renderer$calls$text[[1L]]
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  expect_length(renderer$calls$text, 1L)
  editor$cursor_column <- editor$cursor_column - 1L
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  expect_length(renderer$calls$text, 2L)
  second <- renderer$calls$text[[2L]]
  expect_true(grepl("\033[1;1H", first, fixed = TRUE))
  expect_true(grepl("\033[?25h", first, fixed = TRUE))
  expect_lt(nchar(second), nchar(first) / 4L)
  expect_false(grepl("\033[?1049l", first, fixed = TRUE))
  expect_false(grepl("[\r\n]", first))
  expect_false(grepl("\033[K", second, fixed = TRUE))
})

test_that("theme changes update a contrasting cursor at the same insertion position", {
  fixture <- screen_fixture(color = TRUE)
  fixture$config$theme <- repl_theme("black")
  fixture$screen$active <- TRUE
  renderer <- screen_test_renderer()
  editor <- list(lines = "r> ", cursor_row = 0L, cursor_column = 3L)
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  expect_identical(renderer$calls$cursor[[1L]], "#FFFFFF")
  expect_true(fixture$screen$last_frame$cursor_visible)
  fixture$config$theme <- repl_theme("white")
  screen_invalidate(fixture$screen)
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  expect_identical(renderer$calls$cursor[[2L]], "#000000")
  expect_match(renderer$calls$text[[2L]], "\033[2J", fixed = TRUE)
  fixture$config$color <- FALSE
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  expect_identical(renderer$calls$cursor[[3L]], "")
})

test_that("cursor visibility changes survive an otherwise unchanged frame", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  frame <- list(lines = c("same", "rows"), cursor_row = 0L, cursor_column = 2L,
    layout = list(width = 20L, height = 2L), cursor_visible = FALSE, cursor_color = NULL)
  shown <- frame
  shown$cursor_visible <- TRUE
  update <- .screen_build_update(shown, frame, cfg, show_cursor = TRUE)
  expect_match(update, "\033[?25h", fixed = TRUE)
  expect_false(grepl("\033[K", update, fixed = TRUE))
  expect_match(.screen_build_update(frame, shown, cfg, show_cursor = FALSE), "\033[?25l", fixed = TRUE)
  expect_identical(.screen_build_update(shown, shown, cfg, show_cursor = TRUE), "")
  expect_identical(.screen_build_update(frame, frame, cfg, show_cursor = FALSE), "")
  recolored <- shown
  recolored$cursor_color <- "#FFFFFF"
  expect_match(.screen_build_update(recolored, shown, cfg), "\033[?25h", fixed = TRUE)
})

test_that("lazy transcript preparation is bounded and matches full formatting", {
  fixture <- screen_fixture(color = TRUE)
  screen_append(fixture$screen, sprintf("Line%04d %s", seq_len(4000L), strrep("x", 70L)))
  viewport <- .screen_viewport(fixture$screen, fixture$config, 33L, 25L)
  expect_lte(length(fixture$screen$cache$tail_rows), 64L * 3L)
  expect_false(viewport$complete)
  expect_match(screen_plain(tail(viewport$rows, 1L)), "x", fixed = TRUE)
  complete <- .screen_transcript(fixture$screen, fixture$config, 33L)
  invisible(lapply(c(0L, 17L, 69L, 150L, 500L, 0L), function(offset) {
    fixture$screen$scroll <- offset
    found <- .screen_viewport(fixture$screen, fixture$config, 33L, 25L)
    bottom <- length(complete) - offset
    expect_identical(found$rows, complete[seq.int(bottom - 24L, bottom)])
  }))
})

test_that("theme invalidation retains session data and repaints every row", {
  fixture <- screen_fixture(color = TRUE)
  fixture$screen$active <- TRUE
  screen_append(fixture$screen, "Retained output")
  editor <- list(lines = "r> retained", cursor_row = 0L, cursor_column = 12L)
  renderer <- screen_test_renderer()
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  entries <- fixture$screen$entries
  screen_invalidate(fixture$screen)
  expect_identical(fixture$screen$entries, entries)
  expect_identical(fixture$screen$last_editor, editor)
  expect_null(fixture$screen$last_frame)
  fixture$config$theme <- repl_theme("paper")
  renderer$render(fixture$screen, editor, fixture$config, c(width = 80L, height = 20L))
  expect_length(renderer$calls$text, 2L)
  expect_true(grepl("\033[2J", renderer$calls$text[[2L]], fixed = TRUE))
  expect_false(identical(renderer$calls$text[[1L]], renderer$calls$text[[2L]]))
})

test_that("styled newline splitting carries links without scanning graphemes", {
  link <- "\033]8;;https://example.org\033\\"
  text <- paste0("\033[31m", link, "first\nsecond\n")
  rows <- .screen_split_lines(text)
  expect_identical(screen_plain(rows), c("first", "second", ""))
  expect_true(all(grepl(link, rows, fixed = TRUE)))
  expect_identical(.screen_split_lines(""), "")
  expect_identical(.screen_split_lines("a\n\n"), c("a", "", ""))
})

test_that("resizing the fullscreen editor preserves every grapheme in its buffer", {
  fixture <- screen_fixture()
  fixture$screen$active <- TRUE
  fixture$config$screen <- fixture$screen
  state <- .editor_state('x <- "\u754c e\u0301"\nmean(iris$Sepal.Length)')
  original <- state
  frames <- lapply(list(c(width = 132L, height = 30L), c(width = 45L, height = 12L),
                      c(width = 20L, height = 6L), c(width = 132L, height = 30L)), function(size)
    .editor_display_frame(state, size, "r> ", "+  ", character(), fixture$config))
  expect_identical(state, original)
  expect_true(all(vapply(frames, function(frame) frame$cursor_row < length(frame$lines), logical(1))))
  expect_identical(frames[[1L]], frames[[4L]])
})

test_that("condition cards reflow and historical source follows theme changes", {
  fixture <- screen_fixture(color = TRUE)
  lines <- .condition_lines("A long diagnostic message with enough words to wrap on a narrow terminal.",
                             "error", fixture$config, width = 80L)
  screen_append(fixture$screen, lines, "error", ansi = TRUE)
  entry <- fixture$screen$entries[[1L]]
  expect_true(is.list(entry$condition))
  narrow <- .screen_entry_lines(entry, fixture$config, 30L)
  expect_true(all(vapply(narrow, .screen_width, numeric(1)) <= 30L))
  expect_true(any(grepl("[ERROR]", screen_plain(narrow), fixed = TRUE)))
  code <- 'x <- "first\nsecond"'
  styled <- paste0(c("r> ", "+  "), .highlight_r_window(code, 1L, 2L, fixture$config$theme, TRUE))
  attr(styled, "prism_source") <- list(text = code, prefixes = c("r> ", "+  "))
  screen_append(fixture$screen, styled, "input", ansi = TRUE)
  source <- fixture$screen$entries[[2L]]
  dark <- .screen_entry_lines(source, fixture$config, 80L)
  fixture$config$theme <- repl_theme("paper")
  light <- .screen_entry_lines(source, fixture$config, 80L)
  expect_identical(screen_plain(dark), screen_plain(light))
  expect_false(identical(dark, light))
  tiny <- screen_state(max_lines = 1L)
  screen_append(tiny, styled, "input", ansi = TRUE)
  expect_null(tiny$entries[[1L]]$source)
  expect_length(.screen_transcript(tiny, fixture$config, 80L), 1L)
})

test_that("emitted semantic previews and clickable links follow theme changes", {
  fixture <- screen_fixture(color = TRUE)
  screen <- fixture$screen
  screen$active <- TRUE
  config <- fixture$config
  config$color_depth <- 16777216L
  config$theme <- repl_theme("midnight", color_depth = config$color_depth)
  config$hyperlinks <- TRUE
  config$link_context <- FALSE
  config$output_hook <- function(lines, role) screen_append(screen, lines, role, ansi = TRUE)
  preview <- preview_object(data.frame(x = 1:2, label = c("cafe", "\u754c")), width = 80L, rows = 2L)
  .emit(preview, "output", config)
  .emit("Preview heading", "heading", config)
  .emit("Preview note", "muted", config)
  .show_error(list(error = simpleError("Theme recolor regression"),
    code = "stop('Theme recolor regression')", trace = character()), config)
  expect_length(screen$entries, 5L)
  invisible(lapply(seq_len(3L), function(i) {
    expect_false(screen$entries[[i]]$ansi)
    expect_false(any(grepl("\033", screen$entries[[i]]$lines, fixed = TRUE)))
  }))
  link <- screen$entries[[5L]]
  expect_true(is.list(link$link))
  expect_match(link$link$url, "https://chat.mistral.ai/chat?", fixed = TRUE)
  expect_true(startsWith(link$link$label, "  Explain with Mistral AI"))
  expect_identical(link$lines, link$link$label)

  renderer <- screen_test_renderer()
  editor <- list(lines = "r> ", cursor_row = 0L, cursor_column = 3L)
  dark_frame <- renderer$render(screen, editor, config, c(width = 132L, height = 40L))
  dark <- lapply(screen$entries, .screen_entry_lines, config = config, width = 80L)
  retained <- screen$entries
  old_theme <- config$theme
  config$theme <- repl_theme("white", color_depth = config$color_depth)
  screen_invalidate(screen)
  light_frame <- renderer$render(screen, editor, config, c(width = 132L, height = 40L))
  light <- lapply(screen$entries, .screen_entry_lines, config = config, width = 80L)
  expect_identical(screen$entries, retained)
  expect_false(identical(dark_frame$lines, light_frame$lines))
  invisible(lapply(c(1L, 2L, 3L, 5L), function(i) {
    role <- screen$entries[[i]]$role
    expect_identical(screen_plain(dark[[i]]), screen_plain(light[[i]]))
    expect_true(any(grepl(paste0("\033[", config$theme$colors[[role]], "m"), light[[i]], fixed = TRUE)))
    expect_false(any(grepl(paste0("\033[", old_theme$colors[[role]], "m"), light[[i]], fixed = TRUE)))
  }))
  expect_true(any(grepl(paste0("\033]8;;", link$link$url, "\033\\"), light[[5L]], fixed = TRUE)))
  expect_identical(.screen_entry_block(screen$entries[[5L]], 1L, 1L, config, 80L), light[[5L]])
  expect_true(grepl("\033[2J", renderer$calls$text[[2L]], fixed = TRUE))
})

test_that("semantic transcript retention cannot revive discarded lines", {
  screen <- screen_state(max_lines = 2L)
  config <- repl_config(color = TRUE, settings_file = NULL)
  config$output_hook <- function(lines, role) screen_append(screen, lines, role, ansi = TRUE)
  .emit(c("discarded", "retained one", "retained two"), "muted", config)
  config$theme <- repl_theme("white")
  expect_identical(screen$entries[[1L]]$lines, c("retained one", "retained two"))
  expect_identical(screen_plain(.screen_transcript(screen, config, 80L)), c("retained one", "retained two"))

  label <- "discarded\nretained one\nretained two"
  lines <- structure(label, prism_link = list(url = "https://example.org", label = label))
  screen_clear(screen)
  screen_append(screen, lines, "link", ansi = TRUE)
  expect_null(screen$entries[[1L]]$link)
  expect_identical(screen_plain(.screen_transcript(screen, config, 80L)), c("retained one", "retained two"))
})
