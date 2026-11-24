test_that("inventory skips deferred and active bindings", {
  e <- new.env(parent = emptyenv())
  e$values <- 1:5
  e$table <- data.frame(x = 1:3, y = letters[1:3])
  delayedAssign("deferred", stop("must not be evaluated"), assign.env = e)
  calls <- 0L
  makeActiveBinding("active", function(value) { calls <<- calls + 1L; 42 }, e)
  inventory <- workspace_objects(e)
  expect_identical(calls, 0L)
  expect_equal(inventory$binding[match(c("deferred", "active"), inventory$name)],
               c("promise", "active"))
  expect_true(all(is.na(inventory$bytes[inventory$binding != "value"])))
  expect_equal(inventory$dimensions[inventory$name == "table"], "3 x 2")
  expect_true(all(is.na(workspace_objects(e, sizes = FALSE)$bytes)))
  expect_equal(nrow(workspace_objects(new.env(parent = emptyenv()))), 0L)
})

test_that("batch removal validates before changing the environment", {
  e <- list2env(list(x = 1, y = 2), parent = emptyenv())
  expect_error(remove_objects(c("x", "absent"), e), "all selected")
  expect_true(exists("x", e, inherits = FALSE))
  lockBinding("y", e)
  expect_error(remove_objects(c("x", "y"), e), "locked")
  expect_true(exists("x", e, inherits = FALSE))
  unlockBinding("y", e)
  expect_equal(remove_objects(c("x", "y", "x"), e), c("x", "y"))
  expect_length(ls(e), 0L)
})

test_that("loaded namespace and attached status stay distinct", {
  packages <- loaded_packages()
  expect_true(all(c("package", "version", "attached", "loaded") %in% names(packages)))
  expect_true(packages$attached[packages$package == "base"])
  expect_error(detach_packages(c("base", "stats")), "cannot be detached")
  absent <- detach_packages("prismrepl_test_absent_namespace")
  expect_equal(absent$status, "not attached")
  expect_equal(nrow(detach_packages(character())), 0L)
})

test_that("previews are bounded, Unicode-safe, and passive for environments", {
  lines <- preview_object(data.frame(text = rep("\u4e2d\u6587 caf\u00e9", 100),
                                    value = seq_len(100)), width = 32, rows = 3)
  expect_true(any(grepl("Showing 3 of 100 rows", lines, fixed = TRUE)))
  expect_lte(length(lines), 24L)
  expect_true(all(utf8::utf8_width(lines) <= 32))
  expect_true(any(grepl("\u4e2d\u6587", lines, fixed = TRUE)))
  e <- new.env(parent = emptyenv())
  makeActiveBinding("danger", function(value) stop("must not run"), e)
  expect_true(any(grepl("values are not evaluated", preview_object(e), fixed = TRUE)))
  expect_false(any(grepl("\033", preview_object("\033[31m"), fixed = TRUE)))
  expect_error(preview_object(1, rows = 0), "positive integer")
})

test_that("Git status is local and reports tracked changes", {
  skip_if(!nzchar(Sys.which("git")), "Git is unavailable")
  path <- tempfile("prismrepl-git-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  git <- Sys.which("git")
  expect_equal(system2(git, c("init", "--quiet", shQuote(path)),
                      stdout = FALSE, stderr = FALSE), 0L)
  clean <- git_info(path)
  expect_true(clean$is_repo)
  expect_false(clean$dirty)
  writeLines("example", file.path(path, "example.txt"))
  expect_equal(system2(git, c("-C", shQuote(path), "add", "--", "example.txt"),
                      stdout = FALSE, stderr = FALSE), 0L)
  dirty <- git_info(path)
  expect_true(dirty$dirty)
  expect_equal(dirty$tracked_changes, 1L)
})
