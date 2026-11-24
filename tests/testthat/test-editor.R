editor_event <- function(key = "", text = "", modifiers = character(), type = "key",
                         truncated = FALSE) {
  list(type = type, key = key, text = text, modifiers = modifiers, truncated = truncated)
}

editor_test_config <- function() {
  list(pipe = "|>", max_paste = 100000L, max_lines = 2000L,
       theme = repl_theme(), color = FALSE)
}

editor_apply <- function(state, event, history = character(), envir = .GlobalEnv) {
  .editor_step(state, event, history, envir, editor_test_config())
}

editor_text <- function(state) paste0(state$chars, collapse = "")

test_that("combining marks and emoji are edited as whole graphemes", {
  state <- .editor_state("e\u0301\U0001f469\u200d\U0001f4bb")
  expect_length(state$chars, 2L)
  state <- editor_apply(state, editor_event("backspace"))
  expect_identical(editor_text(state), "e\u0301")
  state <- editor_apply(state, editor_event("backspace"))
  expect_identical(editor_text(state), "")
  state <- editor_apply(state, editor_event(type = "text", text = "e"))
  state <- editor_apply(state, editor_event(type = "text", text = "\u0301"))
  expect_length(state$chars, 1L)
  expect_equal(state$cursor, 1L)
})

test_that("editing and history preserve complete multiline expressions", {
  history <- c("one", "f <- function(x) {\n  x + 1\n}")
  state <- .editor_state("unfinished", history)
  state <- editor_apply(state, editor_event("p", modifiers = "ctrl"), history)
  expect_identical(editor_text(state), history[[2L]])
  state <- editor_apply(state, editor_event("p", modifiers = "ctrl"), history)
  expect_identical(editor_text(state), "one")
  state <- editor_apply(state, editor_event("n", modifiers = "ctrl"), history)
  state <- editor_apply(state, editor_event("n", modifiers = "ctrl"), history)
  expect_identical(editor_text(state), "unfinished")
})

test_that("Enter respects parse completeness and explicit newlines", {
  state <- editor_apply(.editor_state("sum("), editor_event("enter"))
  expect_identical(state$action, "edit")
  expect_match(editor_text(state), "\n", fixed = TRUE)
  state <- editor_apply(.editor_state("1 + 1"), editor_event("enter"))
  expect_identical(state$action, "submit")
  state <- editor_apply(.editor_state("1 + 1"), editor_event("j", modifiers = "ctrl"))
  expect_identical(state$action, "edit")
  expect_identical(editor_text(state), "1 + 1\n")
  state <- editor_apply(.editor_state("1 + 1"), editor_event("enter", modifiers = "alt"))
  expect_identical(state$action, "edit")
  expect_identical(editor_text(state), "1 + 1")
  state <- editor_apply(.editor_state("1 + )"), editor_event("enter"))
  expect_identical(state$action, "submit")
  expect_identical(editor_apply(.editor_state(":objects"), editor_event("enter"))$action,
                   "submit")
})

test_that("bracketed paste never submits and rejects terminal controls", {
  state <- editor_apply(.editor_state(),
    editor_event(type = "paste", text = "x <- 1\r\nprint(x)\r\n"))
  expect_identical(state$action, "edit")
  expect_identical(editor_text(state), "x <- 1\nprint(x)\n")
  bad <- editor_apply(.editor_state("safe"),
    editor_event(type = "paste", text = "\033[2Jbad"))
  expect_identical(editor_text(bad), "safe")
  expect_match(bad$overlay, "rejected", fixed = TRUE)
  long <- editor_apply(.editor_state(),
    editor_event(type = "paste", text = paste(rep("a", 100001L), collapse = "")))
  expect_identical(editor_text(long), "")
  expect_match(long$overlay, "limit", fixed = TRUE)
})

test_that("pipe shortcuts and cancellation are predictable", {
  state <- editor_apply(.editor_state("x"), editor_event("m", modifiers = c("ctrl", "shift")))
  expect_identical(editor_text(state), "x |> ")
  state <- editor_apply(.editor_state("x"), editor_event("m", modifiers = "alt"))
  expect_identical(editor_text(state), "x |> ")
  expect_identical(editor_apply(state, editor_event("c", modifiers = "ctrl"))$action, "cancel")
  expect_identical(editor_apply(.editor_state(), editor_event("d", modifiers = "ctrl"))$action, "eof")
  state <- .editor_state("xy")
  state$cursor <- 0L
  state <- editor_apply(state, editor_event("d", modifiers = "ctrl"))
  expect_identical(editor_text(state), "y")
  expect_identical(state$action, "edit")
})

test_that("reverse search accepts a match for editing without execution", {
  history <- c("alpha <- 1", "beta <- 2", "alpha + beta")
  state <- editor_apply(.editor_state("draft", history), editor_event("r", modifiers = "ctrl"), history)
  state <- editor_apply(state, editor_event(type = "text", text = "alpha"), history)
  expect_identical(editor_text(state), "alpha + beta")
  state <- editor_apply(state, editor_event("r", modifiers = "ctrl"), history)
  expect_identical(editor_text(state), "alpha <- 1")
  accepted <- editor_apply(state, editor_event("enter"), history)
  expect_null(accepted$search)
  expect_identical(accepted$action, "edit")
  cancelled <- editor_apply(state, editor_event("escape"), history)
  expect_identical(editor_text(cancelled), "draft")
})

test_that("name completion never forces promises or active bindings", {
  envir <- new.env(parent = baseenv())
  envir$alpha <- 1
  touched <- FALSE
  makeActiveBinding("alarming", function() { touched <<- TRUE; stop("active") }, envir)
  delayedAssign("also_lazy", { touched <<- TRUE; stop("promise") }, assign.env = envir)
  result <- complete_r("al", envir = envir)
  expect_true(all(c("alpha", "alarming", "also_lazy") %in% result$candidates))
  expect_false(touched)
  expect_length(complete_r("alarming$", envir = envir)$candidates, 0L)
  expect_length(complete_r("also_lazy$", envir = envir)$candidates, 0L)
  expect_false(touched)
})

test_that("completion spans use code points and members use safe names", {
  envir <- new.env(parent = baseenv())
  envir$beta <- 1
  envir$data <- list("two words" = 1, total = 2)
  envir$f <- function(alpha = 1, beta = 2) NULL
  result <- complete_r("\u03bb <- be", envir = envir)
  expect_equal(result$start, 5L)
  expect_equal(result$end, 7L)
  expect_true("beta" %in% result$candidates)
  expect_identical(complete_r("data$tw", envir = envir)$candidates, "`two words`")
  expect_true("alpha = " %in% complete_r("f(al", envir = envir)$candidates)
  expect_length(complete_r("# be", envir = envir)$candidates, 0L)
  expect_length(complete_r("definitely_unloaded_package_xyz::", envir = envir)$candidates, 0L)
})

test_that("quoted path completion preserves R string escaping", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  file.create(file.path(directory, "two words.txt"))
  input <- paste0('readLines("', directory, '/two')
  candidates <- complete_r(input)$candidates
  expect_true(any(endsWith(candidates, "/two words.txt")))
  expect_length(complete_r('readLines("https://example.org/fi')$candidates, 0L)
})

test_that("viewport frames fit wide text and preserve cursor bounds", {
  state <- .editor_state(paste(rep("\u754c", 30L), collapse = ""))
  frame <- .editor_frame(state, 30L, 12L, "r> ", "+  ", character(), editor_test_config())
  expect_lte(utf8::utf8_width(frame$lines[[1L]], utf8 = TRUE), 29L)
  expect_lte(frame$cursor_column, 29L)
  expect_gte(frame$cursor_column, 0L)
  state <- .editor_state(paste(seq_len(100L), collapse = "\n"))
  frame <- .editor_frame(state, 40L, 10L, "r> ", "+  ", "status", editor_test_config())
  expect_lte(length(frame$lines), 8L)
  expect_lt(frame$cursor_row, length(frame$lines))
})

test_that("completion cycles choices without corrupting surrounding code", {
  envir <- new.env(parent = emptyenv())
  envir$alpha <- 1
  envir$alpine <- 2
  state <- .editor_state("al + 1")
  state$cursor <- 2L
  state <- editor_apply(state, editor_event("tab"), envir = envir)
  expect_identical(editor_text(state), "alp + 1")
  state <- editor_apply(state, editor_event("tab"), envir = envir)
  expect_identical(editor_text(state), "alpha + 1")
  state <- editor_apply(state, editor_event("tab"), envir = envir)
  expect_identical(editor_text(state), "alpine + 1")
})

test_that("a window retains string color from hidden earlier lines", {
  state <- .editor_state('x <- "first\nsecond\nthird"')
  state$cursor <- which(state$chars == "\n")[[2L]] - 1L
  config <- editor_test_config()
  config$color <- TRUE
  frame <- .editor_frame(state, 40L, 3L, "r> ", "+  ", character(), config)
  expect_identical(as.character(cli::ansi_strip(frame$lines[[1L]])), "^ second")
  expect_true(grepl(paste0("\033[", config$theme$colors[["string"]], "m"),
                    frame$lines[[1L]], fixed = TRUE))
})

test_that("tabs retain four-column cursor alignment through highlighting", {
  state <- .editor_state("\t\u03bb")
  frame <- .editor_frame(state, 40L, 10L, "r> ", "+  ", character(), editor_test_config())
  expect_identical(as.character(cli::ansi_strip(frame$lines[[1L]])), "r>     \u03bb")
  expect_equal(frame$cursor_column, 8L)
})

test_that("search with no match cannot accept a stale result", {
  history <- "alpha"
  state <- editor_apply(.editor_state("draft", history), editor_event("r", modifiers = "ctrl"), history)
  state <- editor_apply(state, editor_event(type = "text", text = "unmatched"), history)
  state <- editor_apply(state, editor_event("enter"), history)
  expect_false(is.null(state$search))
  expect_identical(state$action, "edit")
  expect_match(state$overlay, "No history match", fixed = TRUE)
})

test_that("local edits preserve regional-indicator grapheme boundaries", {
  flags <- "\U0001f1e6\U0001f1e7x\U0001f1e8\U0001f1e9"
  state <- .editor_state(flags)
  state <- .editor_replace(state, 1L, 2L, "")
  expect_length(state$chars, 2L)
  state <- .editor_replace(state, 1L, 1L, "\U0001f1ea")
  expect_identical(state$chars, .editor_graphemes(editor_text(state)))
  state <- .editor_replace(state, 1L, 1L, "q")
  expect_identical(state$chars, .editor_graphemes(editor_text(state)))
})

test_that("highlight cache invalidates when the input changes", {
  config <- editor_test_config()
  config$highlight_cache <- new.env(parent = emptyenv())
  first <- .editor_frame(.editor_state("alpha"), 40L, 10L, "r> ", "+  ", character(), config)
  repeat_frame <- .editor_frame(.editor_state("alpha"), 40L, 10L, "r> ", "+  ", character(), config)
  second <- .editor_frame(.editor_state("beta"), 40L, 10L, "r> ", "+  ", character(), config)
  expect_identical(first, repeat_frame)
  expect_identical(second$lines[[1L]], "r> beta")
})

test_that("effective bindings control editing and the shortcut table", {
  config <- repl_config(settings_file = NULL, keybindings = list(newline = "F4", pipe = "F5"))
  state <- .editor_step(.editor_state("1 + 1"), editor_event("f4"), character(), .GlobalEnv, config)
  expect_identical(editor_text(state), "1 + 1\n")
  expect_identical(state$action, "edit")
  state <- .editor_step(.editor_state("x"), editor_event("f5"), character(), .GlobalEnv, config)
  expect_identical(editor_text(state), "x |> ")
  expect_true("F4" %in% keymap(config)$key)
  state <- .editor_step(.editor_state("x"), editor_event("j", modifiers = "ctrl"), character(), .GlobalEnv, config)
  expect_identical(editor_text(state), "x")
})

test_that("wrapped editor mouse hits land on complete Unicode graphemes", {
  config <- repl_config(color = FALSE, settings_file = NULL)
  config$highlight_cache <- new.env(parent = emptyenv())
  state <- .editor_state(paste0("abcdefghijklmnop", "e\u0301\u754c", "qrstuvwxyz"))
  geometry <- .editor_wrap_geometry(state, 18L, "r> ", "+  ", config)
  frame <- .editor_wrapped_frame(state, geometry, 18L, 8L, config)
  expect_gt(length(frame$hit_rows), 1L)
  expect_identical(paste0(state$chars, collapse = ""), editor_text(state))
  layout <- list(editor_start = 12L, content_width = 18L)
  invisible(lapply(seq_along(frame$hit_rows), function(index) {
    row <- frame$hit_rows[[index]]
    boundary <- min(4L, length(row$columns))
    event <- list(type = "mouse", key = "left", action = "press",
      x = row$columns[[boundary]] + 2L, y = layout$editor_start + index)
    moved <- .editor_mouse(state, event, frame, layout)
    expect_equal(moved$cursor, row$from + boundary - 1L)
    expect_identical(moved$chars, state$chars)
  }))
  outside <- list(type = "mouse", key = "left", action = "press", x = 80L, y = 2L)
  expect_identical(.editor_mouse(state, outside, frame, layout), state)
  expect_true(all(vapply(frame$lines, .screen_width, numeric(1)) <= 17L))
})

test_that("vertical movement uses visual wrapped rows before history", {
  config <- repl_config(color = FALSE, settings_file = NULL)
  config$highlight_cache <- new.env(parent = emptyenv())
  state <- .editor_state(strrep("abc", 12L), history = "history entry")
  geometry <- .editor_wrap_geometry(state, 18L, "r> ", "+  ", config)
  frame <- .editor_wrapped_frame(state, geometry, 18L, 8L, config)
  moved <- .editor_visual_move(state, frame, -1L, "history entry")
  expect_identical(moved$chars, state$chars)
  expect_lt(moved$cursor, state$cursor)
  state$cursor <- 0L
  frame <- .editor_wrapped_frame(state, geometry, 18L, 8L, config)
  moved <- .editor_visual_move(state, frame, -1L, "history entry")
  expect_identical(editor_text(moved), "history entry")
})

test_that("expanded tabs followed by combining marks preserve wrapped hit boundaries", {
  config <- repl_config(color = FALSE, settings_file = NULL)
  state <- .editor_state("a\t\u0301b")
  geometry <- .editor_wrap_geometry(state, 9L, "r> ", "+  ", config)
  frame <- .editor_wrapped_frame(state, geometry, 9L, 8L, config)
  expect_length(frame$lines, 2L)
  expect_identical(frame$lines[[1L]], "r> a    \u0301")
  expect_true(endsWith(frame$lines[[2L]], "b"))
  expect_equal(unname(vapply(frame$lines, .screen_width, numeric(1))), c(8L, 4L))
  hit <- frame$hit_rows[[2L]]
  expect_equal(hit$from, 3L)
  layout <- list(editor_start = 10L, content_width = 9L)
  event <- list(type = "mouse", key = "left", action = "press", x = 5L, y = 12L)
  expect_equal(.editor_mouse(state, event, frame, layout)$cursor, 3L)
})

test_that("shortcut, recall and completion insertions all enforce input limits", {
  config <- repl_config(settings_file = NULL, max_paste = 4L, max_lines = 1L)
  apply <- function(state, event, history = character(), envir = .GlobalEnv)
    .editor_step(state, event, history, envir, config)
  cases <- list(editor_event("j", modifiers = "ctrl"), editor_event("m", modifiers = "alt"),
    editor_event("y", modifiers = "ctrl"))
  invisible(lapply(cases, function(event) {
    state <- .editor_state("x")
    state$kill <- "over-limit"
    found <- apply(state, event)
    expect_identical(editor_text(found), "x")
    expect_match(found$overlay, "Input limit", fixed = TRUE)
  }))
  found <- apply(.editor_state("f("), editor_event("enter"))
  expect_identical(editor_text(found), "f(")
  expect_identical(found$action, "edit")
  history <- "over-limit"
  state <- .editor_state("x", history)
  found <- apply(state, editor_event("p", modifiers = "ctrl"), history)
  expect_identical(editor_text(found), "x")
  expect_identical(found$history_index, state$history_index)
  found <- apply(state, editor_event("r", modifiers = "ctrl"), history)
  expect_false(found$search$found)
  expect_identical(editor_text(found), "x")
  envir <- new.env(parent = emptyenv())
  envir$alphabet <- 1L
  found <- apply(.editor_state("al"), editor_event("tab"), envir = envir)
  expect_identical(editor_text(found), "al")
  expect_match(found$overlay, "Input limit", fixed = TRUE)
})

test_that("incremental byte and line counts remain exact through Unicode edits", {
  state <- .editor_state("e\u0301\n\u754c")
  state <- .editor_replace(state, 1L, 2L, "\n\t")
  state <- .editor_replace(state, 0L, 1L, "x")
  expect_equal(state$bytes, nchar(editor_text(state), type = "bytes"))
  expect_equal(state$line_count, stringi::stri_count_fixed(editor_text(state), "\n") + 1L)
})

test_that("large-buffer viewport metadata is materialized only for visible rows", {
  config <- repl_config(color = FALSE, settings_file = NULL)
  config$highlight_cache <- new.env(parent = emptyenv())
  state <- .editor_state(paste(rep("alpha <- mean(iris$Sepal.Length)", 2000L), collapse = "\n"))
  geometry <- .editor_wrap_geometry(state, 90L, "r> ", "+  ", config)
  expect_length(geometry$starts, 2000L)
  expect_null(geometry$rows)
  frame <- .editor_wrapped_frame(state, geometry, 90L, 8L, config)
  expect_length(frame$hit_rows, 8L)
  expect_equal(frame$hit_rows[[1L]]$line, 1993L)
  expect_equal(tail(frame$hit_rows, 1L)[[1L]]$to, length(state$chars))
})
