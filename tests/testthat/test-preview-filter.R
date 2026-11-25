preview_filter_config <- function(...) repl_config(theme = "midnight", color = FALSE,
  settings_file = NULL, ...)
preview_filter_event <- function(key = "", text = "", type = if (nzchar(text)) "text" else "key",
    modifiers = character()) list(type = type, key = key, text = text, modifiers = modifiers)
preview_filter_state <- function(x, limit = 10L) {
  state <- .explorer_state(x, "records", limit = limit)
  state$stack[[1L]]$view <- "preview"
  state
}
preview_filter_type <- function(state, text, config = preview_filter_config()) {
  if (state$searching) state <- .explorer_apply(state, preview_filter_event("enter"), config)
  state <- .explorer_apply(state, preview_filter_event(text = "/"), config)
  .explorer_apply(state, preview_filter_event(text = text), config)
}

test_that("slash filters table values beyond the first batch and visible columns", {
  cfg <- preview_filter_config()
  value <- as.data.frame(stats::setNames(rep(list(rep("ordinary", 260L)), 12L),
    paste0("column", 1:12)), stringsAsFactors = FALSE)
  value[[12L]][245L] <- "caf\u00e9 TARGET"
  value[[10L]][249L] <- "caf\u00e9 second"
  before <- serialize(value, NULL)
  state <- preview_filter_type(preview_filter_state(value), "CAF\u00c9", cfg)
  expect_true(state$searching)
  loaded <- .explorer_preview_load(state$stack[[1L]])
  expect_identical(loaded$indices, c(245L, 249L))
  expect_identical(loaded$total, 260L)
  expect_identical(loaded$matched, 2L)
  frame <- .explorer_frame(state, cfg, c(width = 80L, height = 20L))
  expect_true(any(grepl("Filter values:", frame, fixed = TRUE)))
  expect_true(any(grepl("Rows 1-2 of 2 (260 total)", frame, fixed = TRUE)))
  expect_true(any(grepl("245", frame, fixed = TRUE)))
  expect_true(any(grepl("249", frame, fixed = TRUE)))
  state <- .explorer_apply(state, preview_filter_event("enter"), cfg)
  expect_false(state$searching)
  expect_false(state$quit)
  expect_identical(serialize(value, NULL), before)
  state <- preview_filter_type(state, "column12", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$matched, 0L)
})

test_that("preview Escape restores the applied query and exact navigation", {
  cfg <- preview_filter_config()
  state <- preview_filter_state(data.frame(id = 1:51, category = rep(c("odd", "even"), length.out = 51L)), limit = 5L)
  state <- preview_filter_type(state, "odd", cfg)
  state <- .explorer_apply(state, preview_filter_event("enter"), cfg)
  state <- .explorer_apply(state, preview_filter_event(text = "]"), cfg)
  state <- .explorer_apply(state, preview_filter_event("down"), cfg)
  state$stack[[1L]]$top <- 2L
  previous <- state$stack[[1L]][c("query", "offset", "cursor", "top")]
  before <- .explorer_frame(state, cfg, c(width = 100L, height = 18L))
  state <- preview_filter_type(state, "even", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, c(2L, 4L, 6L, 8L, 10L))
  state <- .explorer_apply(state, preview_filter_event("escape"), cfg)
  expect_false(state$searching)
  expect_identical(state$stack[[1L]][names(previous)], previous)
  expect_identical(.explorer_frame(state, cfg, c(width = 100L, height = 18L)), before)
  state <- preview_filter_type(state, "no match", cfg)
  state <- .explorer_apply(state, preview_filter_event("c", modifiers = "ctrl"), cfg)
  expect_identical(state$stack[[1L]][names(previous)], previous)
  expect_false(state$quit)
})

test_that("tables and matrices page matching original rows and retain identifiers", {
  cfg <- preview_filter_config()
  value <- data.frame(id = 1:40, category = factor(rep(c("alpha", "beta"), 20L)),
    date = as.Date("2026-01-01") + 1:40,
    row.names = paste0("r", 1:40))
  state <- preview_filter_type(preview_filter_state(value, 4L), "BETA", cfg)
  state <- .explorer_apply(state, preview_filter_event("enter"), cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, c(2L, 4L, 6L, 8L))
  state <- .explorer_apply(state, preview_filter_event(text = "]"), cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, c(10L, 12L, 14L, 16L))
  lines <- .explorer_content(state$stack[[1L]], 120L)$lines
  expect_true(any(grepl("10: r10", lines, fixed = TRUE)))
  expect_true(any(grepl("2026-01-11", lines, fixed = TRUE)))
  state <- .explorer_apply(state, preview_filter_event("end"), cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, c(34L, 36L, 38L, 40L))
  state <- .explorer_apply(state, preview_filter_event("home"), cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, c(2L, 4L, 6L, 8L))
  matrix <- matrix(c(rep("left", 40L), rep("right", 40L)), ncol = 2L)
  matrix[35L, 2L] <- "target"
  state <- preview_filter_type(preview_filter_state(matrix, 4L), "TARGET", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 35L)
  expect_true(any(grepl("35", .explorer_content(state$stack[[1L]], 80L)$lines, fixed = TRUE)))
  state <- .explorer_apply(state, preview_filter_event("enter"), cfg)
  state <- .explorer_apply(state, preview_filter_event("right"), cfg)
  expect_length(state$stack, 1L)
  expect_match(state$status, "no further children")
})

test_that("empty results keep headings and filters support paste, clear and grapheme deletion", {
  cfg <- preview_filter_config()
  state <- preview_filter_type(preview_filter_state(data.frame(word = c("cafe\u0301", "tom"))), "absent", cfg)
  frame <- .explorer_frame(state, cfg, c(width = 100L, height = 16L))
  expect_true(any(grepl("(No matching rows)", frame, fixed = TRUE)))
  expect_true(any(grepl("word", frame, fixed = TRUE)))
  expect_true(any(grepl("Rows 0-0 of 0", frame, fixed = TRUE)))
  state <- .explorer_apply(state, preview_filter_event("u", modifiers = "ctrl"), cfg)
  expect_identical(state$stack[[1L]]$query, "")
  expect_identical(.explorer_preview_load(state$stack[[1L]])$matched, 2L)
  state <- .explorer_apply(state, preview_filter_event(text = "cafe\u0301\U0001f469\u200d\U0001f4bb", type = "paste"), cfg)
  state <- .explorer_apply(state, preview_filter_event("backspace"), cfg)
  expect_identical(state$stack[[1L]]$query, "cafe\u0301")
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 1L)
  state <- .explorer_apply(state, preview_filter_event("enter"), cfg)
  state <- .explorer_apply(state, preview_filter_event(text = "TOM", type = "paste"), cfg)
  expect_true(state$searching)
  expect_identical(state$stack[[1L]]$query, "TOM")
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 2L)
  invisible(lapply(list(c(width = 68L, height = 18L), c(width = 25L, height = 8L),
    c(width = 4L, height = 3L)), function(size) {
    frame <- .explorer_frame(state, cfg, size)
    expect_length(frame, size[["height"]])
    expect_true(all(vapply(frame, .screen_width, numeric(1L)) <= size[["width"]] - 1L))
  }))
})

test_that("primitive filters search full text and preserve atomic leaves", {
  cfg <- preview_filter_config()
  values <- c("tom", paste0(strrep("x", 1000L), "needle"), "needle other")
  state <- preview_filter_type(preview_filter_state(values, 1L), "needle", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 2L)
  state <- .explorer_apply(state, preview_filter_event("enter"), cfg)
  state <- .explorer_apply(state, preview_filter_event(text = "]"), cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 3L)
  state <- .explorer_apply(state, preview_filter_event("enter"), cfg)
  state <- .explorer_apply(state, preview_filter_event("right"), cfg)
  expect_length(state$stack, 1L)
  state <- preview_filter_type(state, "absent", cfg)
  expect_true(any(grepl("(No matching values)", .explorer_content(state$stack[[1L]], 80L)$lines, fixed = TRUE)))
  array <- array(as.character(1:24), c(2L, 3L, 4L))
  state <- preview_filter_type(preview_filter_state(array), "24", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 24L)
})

test_that("preview filtering remains passive for custom classes and list cells", {
  cfg <- preview_filter_config()
  class_name <- "prismrepl_preview_filter_poison"
  method_names <- paste0(c("print", "format", "[", "[[", "length", "dim", "names", "str"), ".", class_name)
  invisible(lapply(method_names, assign, value = function(...) stop("custom method executed"), envir = .GlobalEnv))
  withr::defer(rm(list = method_names, envir = .GlobalEnv))
  env <- new.env(parent = emptyenv())
  active <- function() stop("active binding executed")
  environment(active) <- baseenv()
  makeActiveBinding("active", active, env)
  delayedAssign("pending", stop("promise forced"), eval.env = baseenv(), assign.env = env)
  table <- structure(list(value = structure(c("ordinary", "target", "other"), class = class_name),
    details = list(env, list(hidden = "secret target"), structure(3:4, class = class_name))),
    class = c(class_name, "data.frame"), row.names = 1:3)
  before <- serialize(table, NULL)
  state <- preview_filter_type(preview_filter_state(table), "target", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 2L)
  state <- preview_filter_type(state, "secret", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$matched, 0L)
  state <- preview_filter_type(state, "environment", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 1L)
  expect_identical(serialize(table, NULL), before)
  matrix_column <- data.frame(id = 1:4)
  matrix_column$pair <- matrix(c(rep("ordinary", 4L), "ordinary", "target", "ordinary", "ordinary"), nrow = 4L)
  state <- preview_filter_type(preview_filter_state(matrix_column), "target", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 2L)
})

test_that("zero-sized tables and bounded non-tabular summaries support the filter", {
  cfg <- preview_filter_config()
  values <- list(data.frame(word = character()), data.frame(row.names = 1:3), matrix(integer(), 0L, 2L), NULL)
  invisible(lapply(values, function(value) {
    state <- preview_filter_type(preview_filter_state(value), "absent", cfg)
    expect_true(state$searching)
    expect_identical(.explorer_preview_load(state$stack[[1L]])$matched, 0L)
    expect_true(length(.explorer_content(state$stack[[1L]], 60L)$lines) > 0L)
  }))
  state <- preview_filter_type(preview_filter_state(new.env(parent = emptyenv())), "promises", cfg)
  expect_null(.explorer_preview_load(state$stack[[1L]]))
  expect_true(any(grepl("promises", .explorer_content(state$stack[[1L]], 80L)$lines, fixed = TRUE)))
})

test_that("primitive formats and compound array columns remain safe and searchable", {
  cfg <- preview_filter_config()
  ordered <- ordered(c("low", "high", NA), levels = c("low", "high"))
  node <- preview_filter_state(ordered)$stack[[1L]]
  node$query <- "HIGH"
  expect_identical(.explorer_preview_load(node)$indices, 2L)
  expect_identical(.explorer_preview_atom(unclass(ordered), .explorer_template(ordered)),
    c('"low"', '"high"', "NA"))
  invalid_codes <- c(1, 2.5, Inf, NA)
  expect_false(anyNA(.explorer_preview_atom(invalid_codes, list(class = "factor", levels = "level"))))
  dates <- structure(c(0, Inf, -Inf, NA), class = "Date")
  expect_false(anyNA(.explorer_preview_atom(unclass(dates), .explorer_template(dates))))
  table <- data.frame(id = 1:4)
  table$array <- array(c(rep("ordinary", 15L), "last target"), c(4L, 2L, 2L))
  state <- preview_filter_type(preview_filter_state(table), "target", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 4L)
})

test_that("literal filters and query changes retain the complete result scope", {
  cfg <- preview_filter_config()
  state <- preview_filter_state(data.frame(value = c("target", "target long", "other", "a.b")))
  state <- preview_filter_type(state, "tar", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 1:2)
  state <- .explorer_apply(state, preview_filter_event(text = "get long"), cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 2L)
  state <- preview_filter_type(state, "tar", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 1:2)
  state <- preview_filter_type(state, ".", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 4L)
  state <- preview_filter_type(state, "absent", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, integer())
  state <- .explorer_apply(state, preview_filter_event(text = " also absent"), cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, integer())
  state <- preview_filter_type(state, "other", cfg)
  expect_identical(.explorer_preview_load(state$stack[[1L]])$indices, 3L)
})

test_that("redraws reuse matches and longer queries scan only earlier candidates", {
  cfg <- preview_filter_config()
  state <- preview_filter_state(data.frame(value = c("target", "target long", "other", "last")))
  calls <- list()
  original <- .explorer_preview_matches
  scope <- new.env(parent = environment(.explorer_preview_load))
  scope$.explorer_preview_matches <- function(spec, query, candidates = NULL, chunk = 4096L) {
    calls[[length(calls) + 1L]] <<- list(query = query, candidates = candidates)
    original(spec, query, candidates, chunk)
  }
  invisible(lapply(c(".explorer_preview_load", ".explorer_content", ".explorer_frame"), function(name) {
    fun <- get(name, envir = scope, inherits = TRUE)
    environment(fun) <- scope
    assign(name, fun, envir = scope)
  }))
  state$stack[[1L]]$query <- "tar"
  first <- scope$.explorer_frame(state, cfg, c(width = 80L, height = 18L))
  expect_identical(length(calls), 1L)
  expect_null(calls[[1L]]$candidates)
  expect_identical(scope$.explorer_frame(state, cfg, c(width = 80L, height = 18L)), first)
  scope$.explorer_frame(state, cfg, c(width = 58L, height = 14L))
  expect_identical(length(calls), 1L)
  state$stack[[1L]]$query <- "target long"
  scope$.explorer_frame(state, cfg, c(width = 58L, height = 14L))
  expect_identical(length(calls), 2L)
  expect_identical(calls[[2L]]$candidates, 1:2)
  expect_identical(scope$.explorer_preview_load(state$stack[[1L]])$indices, 2L)
  state$stack[[1L]]$query <- "last"
  scope$.explorer_frame(state, cfg, c(width = 58L, height = 14L))
  expect_identical(length(calls), 3L)
  expect_null(calls[[3L]]$candidates)
  expect_identical(scope$.explorer_preview_load(state$stack[[1L]])$indices, 4L)
})
