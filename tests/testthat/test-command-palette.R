palette_fixture <- function() data.frame(
  id = c("view", "edit", "attach", "help"),
  command = c(":view NAME", ":edit NAME", ":attach PACKAGE", ":help"),
  alias = c("v", "ed", "a", "h"),
  group = c("Workspace", "Workspace", "Packages", "Discover"),
  description = c("Inspect nested object values and attributes.", "Modify a function and save explicitly.",
                  "Load an installed package into the session.", "Search commands and descriptions."),
  insert = c(":view ", ":edit ", ":attach ", ":help"), stringsAsFactors = FALSE)

test_that("palette search includes descriptions, aliases and groups", {
  data <- palette_fixture()
  expect_identical(.palette_data(data, "attributes")$id, "view")
  expect_identical(.palette_data(data, "SAVE FUNCTION")$id, "edit")
  expect_identical(.palette_data(data, "Workspace")$id, c("view", "edit"))
  expect_identical(.palette_data(data, ":attach")$id, "attach")
  expect_identical(.palette_data(data, "no matching phrase")$id, character())
  expect_identical(.palette_data(data, sort = "alphabetical")$id, c("attach", "edit", "help", "view"))
  expect_identical(.palette_data(data, sort = "groups")$id, data$id)
})

test_that("palette chooses insertion text without executing a command", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  data <- palette_fixture()
  event <- function(key = "", text = "", type = "key", modifiers = character())
    list(type = type, key = key, text = text, modifiers = modifiers)
  state <- .palette_apply(.palette_state(), event("down"), cfg, data)
  expect_identical(state$cursor, 2L)
  expect_null(state$chosen)
  chosen <- .palette_apply(state, event("enter"), cfg, data)
  expect_identical(chosen$chosen, ":edit ")
  expect_false(chosen$quit)
  typed <- .palette_apply(.palette_state(), event(text = "installed", type = "text"), cfg, data)
  expect_identical(typed$query, "installed")
  expect_identical(.palette_apply(typed, event("enter"), cfg, data)$chosen, ":attach ")
  cleared <- .palette_apply(typed, event("u", modifiers = "ctrl"), cfg, data)
  expect_identical(cleared$query, "")
  expect_true(.palette_apply(state, event("escape"), cfg, data)$quit)
  no_match <- .palette_apply(.palette_state("zzzz"), event("enter"), cfg, data)
  expect_null(no_match$chosen)
  expect_identical(no_match$cursor, 1L)
  clicked <- .palette_apply(.palette_state(), list(type = "mouse", key = "left", text = "",
    modifiers = character(), action = "press", button = "left"), cfg, data, hit = 3L)
  expect_identical(clicked$chosen, ":attach ")
})

test_that("palette toggles sorting while retaining the selected command", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  data <- palette_fixture()
  state <- .palette_state()
  state$cursor <- 1L
  tab <- list(type = "key", key = "tab", text = "", modifiers = character())
  sorted <- .palette_apply(state, tab, cfg, data)
  expect_identical(sorted$sort, "alphabetical")
  expect_identical(sorted$cursor, 4L)
  original <- .palette_apply(sorted, tab, cfg, data)
  expect_identical(original$sort, "groups")
  expect_identical(original$cursor, 1L)
})

test_that("palette frames preserve description pairing and mouse hit records", {
  cfg <- repl_config(color = TRUE, settings_file = NULL)
  data <- palette_fixture()
  invisible(lapply(c(24L, 48L, 132L), function(width) {
    frame <- .palette_frame(.palette_state(), cfg, c(width = width, height = 24L), data)
    expect_length(frame, 24L)
    expect_true(all(vapply(frame, .screen_width, numeric(1L)) <= width - 1L))
    records <- attr(frame, "palette_records")
    expect_length(records, length(frame))
    expect_true(all(records[!is.na(records)] %in% seq_len(nrow(data))))
  }))
  paired <- .palette_lines(data, 110L)
  expect_true(all(paired$records[grepl("attributes", paired$lines)] == 1L))
  expect_true(all(paired$records[grepl("Modify", paired$lines)] == 2L))
  expect_true(all(paired$records[grepl("^\\[", paired$lines)] == 0L))
})

test_that("palette description columns stay aligned across logical groups", {
  data <- palette_fixture()
  formatted <- .palette_lines(data, 110L, grouped = TRUE)
  starts <- vapply(seq_len(nrow(data)), function(i) {
    line <- formatted$lines[which(formatted$records == i)[[1L]]]
    as.integer(regexpr(data$description[[i]], line, fixed = TRUE)[[1L]])
  }, integer(1L))
  expect_true(all(starts > 0L))
  expect_length(unique(starts), 1L)
  expect_identical(unique(formatted$records[formatted$records > 0L]), seq_len(nrow(data)))
})
