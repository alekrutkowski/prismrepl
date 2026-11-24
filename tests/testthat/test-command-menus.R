.menu_test_state <- function(path = NULL) {
  state <- new.env(parent = emptyenv())
  state$config <- repl_config(theme = "black", color = FALSE, settings_file = path)
  state$initial <- "unchanged"
  state$native <- FALSE
  state
}

test_that("cancelling every preference menu preserves configuration and input", {
  state <- .menu_test_state()
  before <- state$config
  saved <- 0L
  local_mocked_bindings(.command_choice = function(...) NULL, .tui_panel = function(...) NULL,
    .persist_preferences = function(...) { saved <<- saved + 1L; TRUE })
  invisible(lapply(c(":pipe", ":ui", ":sidebar", ":mouse", ":rainbow", ":autopair",
    ":ai", ":bind", ":bind newline", ":keys", ":unbind", ":settings", ":status", ":theme"),
    function(command) {
      capture.output(.repl_command(command, state, new.env()))
      expect_identical(state$config, before)
      expect_identical(state$initial, "unchanged")
    }))
  expect_equal(saved, 0L)
})

test_that("accepted pipe and mouse choices persist through explicit command paths", {
  path <- tempfile(fileext = ".dcf")
  on.exit(unlink(path))
  state <- .menu_test_state(path)
  choices <- c("%>%", "off", "off", "inline")
  index <- 0L
  local_mocked_bindings(.command_choice = function(data, title, config, current = NULL) {
    index <<- index + 1L
    expect_true(choices[[index]] %in% data$id)
    choices[[index]]
  })
  invisible(lapply(c(":pipe", ":mouse", ":sidebar", ":ui"), function(command)
    capture.output(.repl_command(command, state, new.env()))))
  fresh <- repl_config(settings_file = path)
  expect_identical(state$config$pipe, "%>%")
  expect_false(state$config$mouse)
  expect_false(state$config$sidebar)
  expect_identical(state$config$ui, "inline")
  expect_identical(fresh$pipe, "%>%")
  expect_false(fresh$mouse)
  expect_true(fresh$sidebar)
  expect_identical(fresh$ui, "auto")
})

test_that("binding and shortcut lists stage a command without remapping", {
  state <- .menu_test_state()
  before <- state$config$keybindings
  choice <- "newline"
  local_mocked_bindings(.command_choice = function(...) choice)
  invisible(lapply(c(":bind", ":bind newline", ":keys"), function(command) {
    .repl_command(command, state, new.env())
    expect_identical(state$initial, ":bind newline ")
    expect_identical(state$config$keybindings, before)
  }))
  choice <- "reset"
  .repl_command(":bind", state, new.env())
  expect_identical(state$initial, ":bind reset")
  expect_identical(state$config$keybindings, before)
  choice <- "newline"
  capture.output(.repl_command(":unbind", state, new.env()))
  expect_identical(state$config$keybindings$newline, character())
  expect_identical(state$config$keybindings$submit, before$submit)
})

test_that("status selection distinguishes no enabled fields from cancellation", {
  state <- .menu_test_state()
  state$config$status <- c("directory", "memory")
  chosen <- NULL
  initial <- NULL
  local_mocked_bindings(.tui_panel = function(lines, title, config, selected) {
    initial <<- selected
    chosen
  })
  .repl_command(":status", state, new.env())
  expect_identical(initial, c(TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(state$config$status, c("directory", "memory"))
  chosen <- c(2L, 4L)
  .repl_command(":status", state, new.env())
  expect_identical(state$config$status, c("git", "packages"))
  chosen <- integer()
  .repl_command(":status", state, new.env())
  expect_identical(state$config$status, character())
})

test_that("settings choices open the selected menu or stage editable commands", {
  state <- .menu_test_state()
  choose <- c("pipe", "%>%")
  index <- 0L
  local_mocked_bindings(.command_choice = function(data, title, config, current = NULL) {
    index <<- index + 1L
    expect_true(choose[[index]] %in% data$id)
    choose[[index]]
  })
  .repl_command(":settings", state, new.env())
  expect_identical(state$config$pipe, "%>%")
  choose <- "custom"; index <- 0L
  .repl_command(":settings", state, new.env())
  expect_identical(state$initial, ":ai custom ")
  expect_identical(state$config$ai_provider, "mistral")
  state$config$ai_url <- "https://example.com/?q={prompt}"
  index <- 0L
  .repl_command(":settings", state, new.env())
  expect_identical(state$initial, ":ai custom https://example.com/?q={prompt}")
  choose <- "reset"; index <- 0L
  .repl_command(":settings", state, new.env())
  expect_identical(state$initial, ":settings reset")
  expect_identical(state$config$pipe, "%>%")
  expect_identical(state$config$theme$name, "black")
})

test_that("the automatic theme entry detects appearance only after acceptance", {
  cfg <- repl_config(theme = "black", color = FALSE, settings_file = NULL)
  local_mocked_bindings(.tui_panel = function(lines, title, config, formatter, choose, initial_record) {
    formatted <- formatter(30L)
    expect_true(any(grepl("desktop appearance", formatted$lines, fixed = TRUE)))
    as.integer(length(repl_themes()) + 1L)
  })
  expect_identical(.theme_picker(cfg), "auto")
  state <- .menu_test_state()
  local_mocked_bindings(.initial_theme = function(...) "paper")
  capture.output(.repl_command(":theme", state, new.env()))
  expect_identical(state$config$theme$name, "paper")
})
