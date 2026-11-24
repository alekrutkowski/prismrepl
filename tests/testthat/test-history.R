test_that("history is opt-in and missing files do not create directories", {
  directory <- tempfile()
  path <- file.path(directory, "history.rds")
  expect_identical(repl_history(path), character())
  expect_false(dir.exists(directory))
})

test_that("history retains multiline Unicode, bounds size and merges sessions", {
  path <- tempfile()
  on.exit(unlink(c(path, paste0(path, ".lock")), recursive = TRUE), add = TRUE)
  expect_true(save_repl_history(c("one", "\u03bb <- function(x) {\n  x + 1\n}"), path))
  expect_identical(repl_history(path), c("one", "\u03bb <- function(x) {\n  x + 1\n}"))
  expect_true(save_repl_history(c("one", "three", "four"), path, limit = 3L))
  expect_identical(repl_history(path), c("one", "three", "four"))
  expect_identical(repl_history(path, limit = 2L), c("three", "four"))
})

test_that("history lock protects concurrent writes", {
  path <- tempfile()
  lock <- paste0(path, ".lock")
  on.exit(unlink(c(path, lock), recursive = TRUE), add = TRUE)
  save_repl_history("original", path)
  dir.create(lock)
  expect_warning(expect_false(save_repl_history("replacement", path)), "another writer")
  expect_identical(repl_history(path), "original")
})

test_that("history rejects damaged formats and invalid arguments", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(version = 999L, entries = "x"), path)
  expect_error(repl_history(path), "unsupported format")
  expect_error(repl_history(path, limit = 0L), "positive whole number")
  expect_error(save_repl_history(NA_character_, path), "non-missing")
})
