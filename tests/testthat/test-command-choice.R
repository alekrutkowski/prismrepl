.choice_test_function <- function(fun, bindings) {
  environment(fun) <- list2env(bindings, parent = environment(fun))
  fun
}

test_that("command choices keep identifiers separate and focus the current option", {
  cfg <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  data <- data.frame(option = c("First option", "Second option"),
    id = c("internal-first", "internal-second"),
    description = c("First description", "Second description"),
    details = c("Initial alternative", "Current alternative"), stringsAsFactors = FALSE)
  before <- data
  received <- list()
  choose <- .choice_test_function(.command_choice, list(.tui_panel =
    function(lines, title, config, formatter, choose, initial_record) {
      received <<- list(lines = lines, title = title, config = config, choose = choose,
        initial = initial_record, wide = formatter(90L), narrow = formatter(22L))
      1L
    }))
  expect_identical(choose(data, "Options", cfg, "internal-second"), "internal-first")
  expect_identical(data, before)
  expect_identical(received$lines, character())
  expect_identical(received$title, "Options")
  expect_identical(received$config, cfg)
  expect_true(received$choose)
  expect_identical(received$initial, 2L)
  expect_false(any(grepl("internal-", received$wide$lines, fixed = TRUE)))
  expect_true(any(grepl("Second option (current)", received$wide$lines, fixed = TRUE)))
  expect_true(any(grepl("details: Current alternative", received$wide$lines, fixed = TRUE)))
  marker <- which(grepl("(current)", received$narrow$lines, fixed = TRUE))
  expect_identical(unique(received$narrow$records[marker]), 2L)
  expect_true(all(.panel_cells(received$narrow$lines) <= 22L))
})

test_that("cancelled, unmatched and empty command choices do not invent an action", {
  cfg <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  data <- data.frame(id = c("a", "b"), label = c("Alpha", "Beta"))
  received <- list()
  choose <- .choice_test_function(.command_choice, list(.tui_panel = function(...,
      formatter, initial_record, choose) {
    received <<- list(initial = initial_record, formatted = formatter(60L))
    NULL
  }))
  expect_null(choose(data, "Options", cfg, "unknown"))
  expect_identical(received$initial, 1L)
  expect_false(any(grepl("(current)", received$formatted$lines, fixed = TRUE)))
  expect_null(choose(data, "Options", cfg))
  expect_identical(received$initial, 1L)
  expect_null(choose(data[FALSE, ], "Empty options", cfg))
  expect_identical(received$formatted$lines, character())
})

test_that("command choices validate identifiers, labels and returned records", {
  cfg <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  data <- data.frame(id = c("a", "b"), label = c("Alpha", "Beta"))
  choose <- .choice_test_function(.command_choice, list(.tui_panel = function(...) 0L))
  expect_error(choose(data, "Options", cfg), "valid record", fixed = TRUE)
  choose <- .choice_test_function(.command_choice, list(.tui_panel = function(...) 3L))
  expect_error(choose(data, "Options", cfg), "valid record", fixed = TRUE)
  expect_error(choose(transform(data, id = c("a", "a")), "Options", cfg), "identifiers", fixed = TRUE)
  expect_error(choose(transform(data, id = c("a", " ")), "Options", cfg), "identifiers", fixed = TRUE)
  expect_error(choose(transform(data, label = c("Alpha", NA_character_)), "Options", cfg),
    "nonmissing character", fixed = TRUE)
  expect_error(choose(transform(data, label = factor(label)), "Options", cfg), "character vectors", fixed = TRUE)
  expect_error(choose(data["id"], "Options", cfg), "display column", fixed = TRUE)
  expect_error(choose(data, "", cfg), "title", fixed = TRUE)
  expect_error(choose(data, "Options", list()), "configuration", fixed = TRUE)
  expect_error(choose(data, "Options", cfg, c("a", "b")), "one identifier", fixed = TRUE)
})

test_that("command choices retain noninteractive display and the numeric fallback", {
  cfg <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  data <- data.frame(id = c("first", "second"), label = c("Alpha", "Beta"),
    description = c("First action", "Second action"))
  text <- capture.output(result <- .command_choice(data, "Command options", cfg, "second"))
  expect_null(result)
  expect_true(any(grepl("Command options", text, fixed = TRUE)))
  expect_true(any(grepl("Beta (current)", text, fixed = TRUE)))
  expect_true(any(grepl("Second action", text, fixed = TRUE)))
  menu <- list()
  choose <- .choice_test_function(.command_choice, list(.tui_panel = function(..., formatter) {
    formatted <- formatter(19L)
    .panel_plain_choice(formatted$lines, "Options", formatted$records,
      ask = function(choices, graphics, title) {
        menu <<- list(choices = choices, graphics = graphics, title = title)
        2L
      })
  }))
  expect_identical(choose(data, "Options", cfg), "second")
  expect_length(menu$choices, 2L)
  expect_match(menu$choices[[2L]], "Second action", fixed = TRUE)
  expect_false(menu$graphics)
  expect_match(menu$title, "0 cancels", fixed = TRUE)
})

test_that("toggle choices offer descriptive actions with the current value focused", {
  cfg <- repl_config(theme = "midnight", color = FALSE, settings_file = NULL)
  received <- list()
  toggle <- .choice_test_function(.toggle_choice, list(.command_choice =
    function(data, title, config, current) {
      received <<- list(data = data, title = title, config = config, current = current)
      "off"
    }))
  expect_identical(toggle("sidebar", "Session information", TRUE, cfg), "off")
  expect_identical(received$data$id, c("on", "off"))
  expect_identical(received$data$option, c(":sidebar on", ":sidebar off"))
  expect_identical(received$current, "on")
  expect_match(received$title, "current: on", fixed = TRUE)
  expect_identical(received$config, cfg)
  expect_true(all(nzchar(received$data$description)))
  expect_identical(toggle("rainbow", "Rainbow delimiters", FALSE, cfg, toggle = TRUE), "off")
  expect_identical(received$data$id, c("on", "off", "toggle"))
  expect_identical(received$current, "off")
  expect_identical(received$data$description[[3L]], "Switch from off to on.")
  toggle <- .choice_test_function(.toggle_choice, list(.command_choice = function(...) NULL))
  expect_null(toggle("autopair", "Automatic pairing", TRUE, cfg, toggle = TRUE))
  expect_error(toggle(":sidebar", "Session information", TRUE, cfg), "leading colon", fixed = TRUE)
  expect_error(toggle("sidebar", "Session information", NA, cfg), "logical", fixed = TRUE)
  expect_error(toggle("sidebar", "Session information", TRUE, cfg, toggle = 1L), "logical", fixed = TRUE)
  toggle <- .choice_test_function(.toggle_choice, list(.command_choice = function(...) "toggle"))
  expect_error(toggle("sidebar", "Session information", TRUE, cfg), "offered action", fixed = TRUE)
  expect_identical(toggle("sidebar", "Session information", TRUE, cfg, toggle = TRUE), "toggle")
})
