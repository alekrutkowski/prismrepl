test_that("AI choices contain every provider and focus the current destination", {
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  repl_settings(ai_provider = "claude", file = path)
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = path)
  state$initial <- "unfinished input"
  before <- state$config
  stored <- readLines(path)
  received <- NULL
  local_mocked_bindings(
    .command_choice = function(data, title, config, current = NULL) {
      received <<- list(data = data, title = title, current = current)
      NULL
    },
    .persist_preferences = function(...) stop("Cancelled choices must not save"))
  expect_null(.ai_command(character(), state))
  expect_identical(received$data$id, c(repl_ai_providers()$provider, "custom"))
  expect_identical(received$current, "claude")
  expect_match(received$title, "current: claude", fixed = TRUE)
  expect_true(all(vapply(received$data, is.character, logical(1L))))
  expect_match(received$data$prompt[received$data$id == "custom"], "{prompt}", fixed = TRUE)
  expect_identical(state$config, before)
  expect_identical(state$initial, "unfinished input")
  expect_identical(readLines(path), stored)
})

test_that("accepted built-in AI choices use the explicit-command persistence path", {
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = path)
  picks <- 0L
  local_mocked_bindings(.command_choice = function(...) {
    picks <<- picks + 1L
    "chatgpt"
  })
  output <- capture.output(.ai_command(character(), state))
  expect_identical(state$config$ai_provider, "chatgpt")
  expect_identical(repl_settings(file = path)$ai_provider, "chatgpt")
  expect_true(any(grepl("ChatGPT (saved)", output, fixed = TRUE)))
  capture.output(.ai_command("claude", state))
  expect_identical(picks, 1L)
  expect_identical(state$config$ai_provider, "claude")
  expect_identical(repl_settings(file = path)$ai_provider, "claude")
})

test_that("custom AI selection without a URL stages editable input without applying", {
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  repl_settings(ai_provider = "mistral", file = path)
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = path)
  state$initial <- ""
  before <- state$config
  stored <- readLines(path)
  local_mocked_bindings(
    .command_choice = function(...) "custom",
    .persist_preferences = function(...) stop("Incomplete custom setup must not save"))
  output <- capture.output(.ai_command(character(), state))
  expect_identical(state$initial, ":ai custom ")
  expect_identical(state$config, before)
  expect_identical(readLines(path), stored)
  expect_true(any(grepl("HTTPS URL containing {prompt}", output, fixed = TRUE)))
  expect_true(any(grepl("staged", output, fixed = TRUE)))
})

test_that("custom AI choices reuse a saved URL without changing staged input", {
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  url <- "https://example.org/chat?q={prompt}"
  repl_settings(ai_provider = "mistral", ai_url = url, file = path)
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = path)
  state$initial <- "keep this input"
  prompt <- NULL
  local_mocked_bindings(.command_choice = function(data, ...) {
    prompt <<- data$prompt[data$id == "custom"]
    "custom"
  })
  output <- capture.output(.ai_command(character(), state))
  expect_identical(prompt, "Use saved URL template")
  expect_identical(state$config$ai_provider, "custom")
  expect_identical(state$config$ai_url, url)
  expect_identical(state$initial, "keep this input")
  expect_identical(repl_settings(file = path)$ai_provider, "custom")
  expect_identical(repl_settings(file = path)$ai_url, url)
  expect_true(any(grepl("Custom AI (saved)", output, fixed = TRUE)))
})

test_that("editing choices apply on, off and toggle through the existing update path", {
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = path)
  state$config$screen <- screen_state()
  chosen <- "off"
  received <- list()
  local_mocked_bindings(.toggle_choice = function(name, label, value, config, toggle = FALSE) {
    received[[length(received) + 1L]] <<- list(name = name, label = label, value = value, toggle = toggle)
    chosen
  })
  invisible(lapply(c("rainbow", "autopair"), function(name) {
    chosen <<- "off"
    state$config$screen$last_frame <- list(lines = "stale paint")
    capture.output(.editing_preference_command(name, character(), state))
    expect_false(state$config[[name]])
    expect_false(repl_settings(file = path)[[name]])
    expect_null(state$config$screen$last_frame)
    chosen <<- "toggle"
    capture.output(.editing_preference_command(name, character(), state))
    expect_true(state$config[[name]])
    expect_true(repl_settings(file = path)[[name]])
    chosen <<- "on"
    capture.output(.editing_preference_command(name, character(), state))
    expect_true(state$config[[name]])
  }))
  expect_identical(received[[1L]], list(name = "rainbow", label = "Rainbow delimiters", value = TRUE, toggle = TRUE))
  expect_identical(received[[4L]], list(name = "autopair", label = "Automatic pairing", value = TRUE, toggle = TRUE))
  before <- length(received)
  capture.output(.editing_preference_command("rainbow", "off", state))
  expect_length(received, before)
  expect_false(state$config$rainbow)
})

test_that("cancelled editing choices preserve both configuration and saved preferences", {
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  repl_settings(rainbow = FALSE, autopair = TRUE, file = path)
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = path)
  before <- state$config
  stored <- readLines(path)
  local_mocked_bindings(
    .toggle_choice = function(...) NULL,
    .persist_preferences = function(...) stop("Cancelled choices must not save"))
  expect_null(.editing_preference_command("rainbow", character(), state))
  expect_null(.editing_preference_command("autopair", character(), state))
  expect_identical(state$config, before)
  expect_identical(readLines(path), stored)
})

test_that("accepted choices respect disabled persistence", {
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, persist = FALSE, settings_file = path)
  local_mocked_bindings(.command_choice = function(...) "gemini", .toggle_choice = function(...) "off")
  capture.output(.ai_command(character(), state))
  capture.output(.editing_preference_command("autopair", character(), state))
  expect_identical(state$config$ai_provider, "gemini")
  expect_false(state$config$autopair)
  expect_false(file.exists(path))
})

test_that("noninteractive option lists never apply or write a choice", {
  skip_if(interactive(), "This check exercises the noninteractive fallback")
  path <- tempfile(fileext = ".dcf")
  withr::defer(unlink(path))
  repl_settings(ai_provider = "claude", rainbow = FALSE, autopair = TRUE, file = path)
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(color = FALSE, settings_file = path)
  state$initial <- "unfinished input"
  before <- state$config
  stored <- readLines(path)
  local_mocked_bindings(.persist_preferences = function(...) stop("Noninteractive lists must not save"))
  output <- capture.output({
    .ai_command(character(), state)
    .editing_preference_command("rainbow", character(), state)
    .editing_preference_command("autopair", character(), state)
  })
  expect_gt(length(output), 0L)
  expect_identical(state$config, before)
  expect_identical(state$initial, "unfinished input")
  expect_identical(readLines(path), stored)
})
