test_that("edited closures preserve scope and metadata without running code", {
  scope <- new.env(parent = baseenv())
  scope$value <- 41
  scope$ran <- FALSE
  original <- function(x = value) x + 1
  environment(original) <- scope
  attr(original, "label") <- "custom metadata"
  assign("function", function(...) { scope$ran <- TRUE; stop("masked constructor") }, scope)
  result <- .function_edit_validate("function(x = { ran <<- TRUE; 2 }) { ran <<- TRUE; x + value }", original)
  expect_true(result$ok)
  expect_false(scope$ran)
  expect_identical(environment(result$fun), scope)
  expect_identical(attr(result$fun, "label"), "custom metadata")
  expect_identical(typeof(result$fun), "closure")
  invalid <- lapply(c("function(x) {", "x <- function() 1", "function() 1; stop('bad')", "local(function() 1)"),
    .function_edit_validate, original = original)
  expect_true(all(!vapply(invalid, `[[`, logical(1L), "ok")))
  expect_false(scope$ran)
})

test_that("only an explicit valid save mutates a workspace function", {
  workspace <- new.env(parent = baseenv())
  workspace$f <- function(x) x + 1L
  target <- .function_edit_target("f", workspace)
  original <- workspace$f
  expect_false(.function_edit_save(target, "function(x) {", workspace)$ok)
  expect_identical(workspace$f, original)
  result <- .function_edit_save(target, "function(x) x + 2L", workspace)
  expect_true(result$ok)
  expect_identical(workspace$f(3L), 5L)
  expect_identical(environment(workspace$f), environment(original))
  expect_false(.function_edit_save(target, "function(x) x + 3L", workspace)$ok)
  expect_identical(workspace$f(3L), 5L)
})

test_that("inherited functions are copied locally and special bindings stay untouched", {
  parent <- new.env(parent = baseenv())
  workspace <- new.env(parent = parent)
  parent$f <- function(x) x + 1L
  target <- .function_edit_target("f", workspace)
  expect_false(target$local)
  expect_true(.function_edit_save(target, "function(x) x + 2L", workspace)$ok)
  expect_identical(parent$f(3L), 4L)
  expect_identical(workspace$f(3L), 5L)
  forced <- FALSE
  delayedAssign("promise", { forced <<- TRUE; function() 1 }, assign.env = workspace)
  makeActiveBinding("active", function() { forced <<- TRUE; function() 1 }, workspace)
  expect_error(.function_edit_target("promise", workspace), "ordinary binding")
  expect_error(.function_edit_target("active", workspace), "ordinary binding")
  expect_false(forced)
  lockBinding("f", workspace)
  expect_error(.function_edit_target("f", workspace), "unlocked")
  expect_error(.function_edit_target("sum", workspace), "ordinary binding|editable bodies")
})

test_that("function editor inserts newlines, supports undo and only requests explicit saving", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  state <- .function_editor_state("function(x) x")
  state$editor$cursor <- length(state$editor$chars)
  event <- function(key = "", text = "", type = "key", modifiers = character())
    list(type = type, key = key, text = text, modifiers = modifiers)
  newline <- .function_editor_apply(state, event("enter"), cfg, baseenv())
  expect_identical(paste(newline$editor$chars, collapse = ""), "function(x) x\n")
  expect_false(newline$save)
  expect_false(newline$quit)
  undone <- .function_editor_apply(newline, event("z", modifiers = "ctrl"), cfg, baseenv())
  expect_identical(undone$editor$chars, state$editor$chars)
  redone <- .function_editor_apply(undone, event("y", modifiers = "ctrl"), cfg, baseenv())
  expect_identical(redone$editor$chars, newline$editor$chars)
  expect_true(.function_editor_apply(state, event("s", modifiers = "ctrl"), cfg, baseenv())$save)
  expect_true(.function_editor_apply(state, event("x", modifiers = "ctrl"), cfg, baseenv())$quit)
  expect_true(.function_editor_apply(state, event("escape"), cfg, baseenv())$quit)
})

test_that("function editor search and frames work with Unicode and narrow windows", {
  cfg <- repl_config(color = TRUE, settings_file = NULL)
  cfg$highlight_cache <- new.env(parent = emptyenv())
  state <- .function_editor_state("function(x) {\n  paste('e\u0301', x, x)\n}")
  state$query <- "x"
  found <- .function_editor_find(state, restart = TRUE)
  again <- .function_editor_find(found)
  expect_gt(again$editor$cursor, found$editor$cursor)
  expect_match(again$status, "Match 2/3", fixed = TRUE)
  invisible(lapply(c(1L, 12L, 48L, 120L), function(width) {
    frame <- .function_editor_frame(state, "f", cfg, c(width = width, height = 16L))
    expect_length(frame$lines, 16L)
    expect_true(all(vapply(frame$lines, .screen_width, numeric(1L)) <= max(1L, width - 1L)))
    expect_true(frame$cursor_row < 16L)
    moved <- .function_editor_move(state$editor, frame$editor, -100L)
    expect_identical(moved$cursor, 0L)
  }))
})

test_that("the live function editor uses its own painted background for cursor contrast", {
  workspace <- new.env(parent = baseenv())
  workspace$f <- function(x) x
  target <- .function_edit_target("f", workspace)
  written <- list()
  local_mocked_bindings(
    .viewer_start = function(...) list(handle = TRUE, guard = NULL, shared = FALSE),
    .viewer_finish = function(...) invisible(NULL),
    term_size = function() c(width = 80L, height = 15L),
    term_read = function(...) list(type = "key", key = "x", text = "", modifiers = "ctrl"),
    term_write = function(text, synchronize = FALSE, cursor = NULL) {
      written[[length(written) + 1L]] <<- list(text = text, cursor = cursor)
      invisible(NULL)
    })
  cfg <- repl_config(theme = "black", color = TRUE, settings_file = NULL)
  result <- .run_function_editor(target, workspace, cfg)
  expect_false(result$saved)
  expect_identical(written[[1L]]$cursor, "#FFFFFF")
  cfg$theme <- repl_theme("black", list(editor = list(bg = "#FFFFFF")))
  .run_function_editor(target, workspace, cfg)
  expect_identical(written[[2L]]$cursor, "#000000")
  expect_true(all(vapply(written, function(x) grepl("\033[?25h", x$text, fixed = TRUE), logical(1))))
})
