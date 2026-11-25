context_event <- function(key = "f1", modifiers = character()) {
  list(type = "key", key = key, text = "", modifiers = modifiers)
}

test_that("context help resolves names and namespaces in unfinished multiline input", {
  expected <- list(topic = "mean", package = NULL)
  invisible(lapply(0:4, function(cursor) {
    expect_identical(.context_help_target("mean(x,", cursor), expected)
  }))
  qualified <- list(topic = "median", package = "stats")
  invisible(lapply(0:13, function(cursor) {
    expect_identical(.context_help_target("stats::median(x", cursor), qualified)
  }))
  expect_identical(.context_help_target("x <- 1\nstats ::\n median(", 19L), qualified)
  expect_identical(.context_help_target("stats:::median.default(", 16L),
    list(topic = "median.default", package = "stats"))
  commented <- "identity(stats:: # comment\nfilter)"
  expect_length(parse(text = commented), 1L)
  expect_identical(.context_help_target(commented,
    as.integer(regexpr("filter", commented, fixed = TRUE)[[1L]])),
    list(topic = "filter", package = "stats"))
  expect_identical(.context_help_target(commented, 14L),
    list(topic = "filter", package = "stats"))
  expect_identical(.context_help_target("\u03b4\u03bf\u03ba\u03b9\u03bc\u03ae(\n", 3L),
    list(topic = "\u03b4\u03bf\u03ba\u03b9\u03bc\u03ae", package = NULL))
  expect_null(.context_help_target("", 0L))
  expect_null(.context_help_target(":help mean", 8L))
  expect_error(.context_help_target("mean", 5L), "valid character offset")
})

test_that("backtick function names and R operators have local help targets", {
  expect_identical(.context_help_target("base::`+`(1, 2)", 8L), list(topic = "+", package = "base"))
  expect_identical(.context_help_target("`some function`(x)", 6L),
    list(topic = "some function", package = NULL))
  expect_identical(.context_help_target("`a\\`b`(x)", 3L), list(topic = "a`b", package = NULL))
  expect_identical(.context_help_target("x %in% y", 4L), list(topic = "%in%", package = NULL))
  expect_identical(.context_help_target("x |> mean()", 3L), list(topic = "|>", package = NULL))
  expect_identical(.context_help_target("x[[1]]", 2L), list(topic = "[[", package = NULL))
  expect_null(.context_help_target("`unfinished name", 5L))
})

test_that("context help ignores comments, strings, numeric values and blank space", {
  cases <- list(c("# mean(x)", 3L), c("\"mean(x)\"", 3L), c("'mean(x)'", 4L),
    c("r\"---(mean(x))---\"", 8L), c("mean(123)", 7L), c("mean  ", 6L))
  invisible(lapply(cases, function(case) {
    expect_null(.context_help_target(case[[1L]], as.integer(case[[2L]])))
  }))
  expect_null(.context_help_target("x <- '\"mean\"", 10L))
  expect_null(.context_help_target("mean # comment", 7L))
  expect_null(.context_help_target("mean#comment", 4L))
  expect_null(.context_help_target("identity(stats:: # comment\nfilter)", 17L))
})

test_that("F1 stages Unicode context help without forcing or editing bindings", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  workspace <- new.env(parent = baseenv())
  forced <- FALSE
  delayedAssign("deferred", { forced <<- TRUE; function() 1 }, assign.env = workspace)
  makeActiveBinding("active", function() { forced <<- TRUE; function() 1 }, workspace)
  invisible(lapply(c("deferred", "active", "e\u0301lan"), function(name) {
    state <- .editor_state(paste0("x <- 1\n", name, "(x"), history = c("1", "2"))
    state$cursor <- 9L
    before <- state
    result <- .editor_step(state, context_event(), character(), workspace, cfg)
    expect_identical(result$help_request, list(topic = name, package = NULL))
    expect_identical(result$chars, before$chars)
    expect_identical(result$cursor, before$cursor)
    expect_identical(result$history_index, before$history_index)
    expect_identical(result$action, "edit")
  }))
  expect_false(forced)
  keys <- .editor_step(.editor_state("mean("), context_event("f2"), character(), workspace, cfg)
  expect_null(keys$help_request)
  expect_match(paste(keys$overlay, collapse = "\n"), "function name under the cursor", fixed = TRUE)
  blank <- .editor_step(.editor_state(), context_event(), character(), workspace, cfg)
  expect_match(blank$overlay, "Move the cursor", fixed = TRUE)
})

test_that("both help shortcuts remain persistently remappable", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  repl_settings(keybindings = list(function_help = "Ctrl+F", help = "F3"), file = path)
  cfg <- repl_config(settings_file = path, color = FALSE)
  expect_identical(.key_action(context_event("f", "ctrl"), cfg), "function_help")
  expect_identical(.key_action(context_event("f3"), cfg), "help")
  expect_identical(.key_action(context_event(), cfg), "")
  state <- .editor_step(.editor_state("mean"), context_event("f", "ctrl"), character(), baseenv(), cfg)
  expect_identical(state$help_request, list(topic = "mean", package = NULL))
  expect_identical(.viewer_action(context_event("f3"), cfg), "help")
})

test_that("saved legacy defaults adopt F1 help without replacing custom shortcuts", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines(c("Version: 1", "Theme: black", "Key-help: F1", "Key-redraw: Ctrl+L"), path)
  original <- readLines(path)
  cfg <- repl_config(settings_file = path, color = FALSE)
  expect_identical(.key_action(context_event("f1"), cfg), "function_help")
  expect_identical(.key_action(context_event("f2"), cfg), "help")
  expect_identical(cfg$theme$name, "black")
  expect_identical(readLines(path), original)
  writeLines(c("Version: 1", "Key-help: F1", "Key-redraw: f2"), path)
  cfg <- repl_config(settings_file = path, color = FALSE)
  expect_identical(.key_action(context_event("f1"), cfg), "function_help")
  expect_identical(.key_action(context_event("f2"), cfg), "redraw")
  expect_length(cfg$keybindings$help, 0L)
  writeLines(c("Version: 1", "Key-help: F3"), path)
  cfg <- repl_config(settings_file = path, color = FALSE)
  expect_identical(.key_action(context_event("f1"), cfg), "function_help")
  expect_identical(.key_action(context_event("f3"), cfg), "help")
  writeLines(c("Version: 1", "Key-help: F1", "Key-function_help: none"), path)
  cfg <- repl_config(settings_file = path, color = FALSE)
  expect_identical(.key_action(context_event("f1"), cfg), "help")
  expect_length(cfg$keybindings$function_help, 0L)
})

test_that("displaying help consumes the request and preserves the exact editor draft", {
  seen <- NULL
  local_mocked_bindings(view_help = function(topic, package, config) {
    seen <<- list(topic = topic, package = package, config = config)
    invisible("Help text")
  })
  state <- .editor_state("x <- 1\nbase::mean(x)")
  state$cursor <- 15L
  state <- .editor_request_help(state)
  before <- state
  result <- .editor_open_help(state, list(color = FALSE), handle = TRUE, alternate = TRUE)
  expect_identical(seen$topic, "mean")
  expect_identical(seen$package, "base")
  expect_s3_class(seen$config, "prismrepl_config")
  expect_true(seen$config$viewer_handle)
  expect_true(seen$config$viewer_alternate)
  expect_identical(result$chars, before$chars)
  expect_identical(result$cursor, before$cursor)
  expect_null(result$help_request)
  expect_false(result$clear)
  local_mocked_bindings(view_help = function(...) stop("Missing\ndocumentation"))
  failed <- .editor_open_help(before, list(color = FALSE), handle = TRUE)
  expect_identical(failed$chars, before$chars)
  expect_identical(failed$cursor, before$cursor)
  expect_match(failed$overlay, "Cannot open help: Missing documentation", fixed = TRUE)
  expect_true(failed$clear)
})

test_that("context viewers borrow input ownership and restore inline screens", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  cfg$viewer_handle <- methods::new("externalptr")
  output <- character()
  modes <- logical()
  closed <- FALSE
  local_mocked_bindings(
    term_open = function(...) stop("Input must not be reopened"),
    term_close = function(...) { closed <<- TRUE; invisible(NULL) },
    term_write = function(text, ...) { output <<- c(output, text); invisible(NULL) },
    term_output_screen = function(active) { modes <<- c(modes, active); invisible(TRUE) })
  session <- .viewer_start(cfg)
  expect_identical(session$handle, cfg$viewer_handle)
  expect_true(session$borrowed)
  .viewer_finish(session, cfg)
  expect_false(closed)
  expect_length(modes, 0L)
  expect_false(any(grepl("1049", output, fixed = TRUE)))
  cfg$viewer_alternate <- TRUE
  session <- .viewer_start(cfg)
  .viewer_finish(session, cfg)
  expect_false(closed)
  expect_identical(modes, c(TRUE, FALSE))
  expect_true(any(grepl("\033[?1049h", output, fixed = TRUE)))
  expect_true(any(grepl("\033[?1049l", output, fixed = TRUE)))
})

test_that("function editing requests help without saving or changing undo history", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  state <- .function_editor_state("function(x) stats::median(x)")
  state$editor$cursor <- 23L
  result <- .function_editor_apply(state, context_event(), cfg, baseenv())
  expect_identical(result$editor$help_request, list(topic = "median", package = "stats"))
  expect_identical(result$editor$chars, state$editor$chars)
  expect_identical(result$editor$cursor, state$editor$cursor)
  expect_identical(result$undo, state$undo)
  expect_identical(result$redo, state$redo)
  expect_false(result$save)
  expect_false(result$quit)
})
