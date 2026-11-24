test_that("built-in themes select contrasting cursors from painted backgrounds", {
  names <- setdiff(repl_themes(), "mono")
  result <- vapply(names, function(name) .cursor_color(repl_config(theme = name,
    color = TRUE, color_depth = 16777216L, settings_file = NULL)), character(1L))
  expect_identical(unname(result[c("paper", "white", "solarized-light")]), rep("#000000", 3L))
  expect_true(all(result[setdiff(names, c("paper", "white", "solarized-light"))] == "#FFFFFF"))
  expect_identical(.cursor_color(repl_config(theme = "black", color = TRUE, color_depth = 16L,
    settings_file = NULL)), "#FFFFFF")
  expect_identical(.cursor_color(repl_config(theme = "white", color = TRUE, color_depth = 256L,
    settings_file = NULL)), "#000000")
})

test_that("cursor contrast follows custom surface overrides rather than theme names", {
  cfg <- repl_config(theme = repl_theme("black", list(panel = list(bg = "#FFFFFF"))),
    color = TRUE, color_depth = 16777216L, settings_file = NULL)
  expect_identical(.cursor_color(cfg), "#000000")
  expect_identical(.cursor_color(cfg, "editor"), "#FFFFFF")
  cfg <- repl_config(theme = repl_theme("white", list(editor = list(bg = "#000000"))),
    color = TRUE, color_depth = 16777216L, settings_file = NULL)
  expect_identical(.cursor_color(cfg), "#000000")
  expect_identical(.cursor_color(cfg, "editor"), "#FFFFFF")
  cfg <- repl_config(theme = repl_theme("black", c(panel = "48;2;255;255;255")),
    color = TRUE, color_depth = 16777216L, settings_file = NULL)
  expect_identical(.cursor_color(cfg), "#000000")
})

test_that("cursor colors honor ordered SGR colors, resets and extended parameters", {
  color <- function(sgr) .cursor_color(list(color = TRUE, theme = list(colors = c(panel = sgr))))
  expect_identical(color("40"), "#FFFFFF")
  expect_identical(color("107"), "#000000")
  expect_identical(color("47;40"), "#FFFFFF")
  expect_identical(color("48;5;16"), "#FFFFFF")
  expect_identical(color("48;5;231"), "#000000")
  expect_identical(color("48;5;255"), "#000000")
  expect_identical(color("48;2;0;0;0"), "#FFFFFF")
  expect_identical(color("48;2;255;255;255;49"), "")
  expect_identical(color("48;2;255;255;255;0"), "")
  expect_identical(color("48;2;255;255;255;"), "")
  expect_identical(color("0;48;2;255;255;255"), "#000000")
  expect_identical(color("48;2;255;255;255;39"), "#000000")
  expect_identical(color("48;2;255;255;255;58;2;0;0;40"), "#000000")
  expect_identical(color("48;2;255;255;255;58;5;40"), "#000000")
  expect_identical(color("48;2;255;255"), "")
  expect_identical(color("48;5;256"), "")
  expect_identical(color("48;2;999;0;0"), "")
  expect_identical(color("48;6;0"), "")
  expect_identical(color("38;2;255;255;255"), "")
})

test_that("reverse-video cursor backgrounds use known foreground colors", {
  color <- function(sgr) .cursor_color(list(color = TRUE, theme = list(colors = c(panel = sgr))))
  expect_identical(color("30;107;7"), "#FFFFFF")
  expect_identical(color("97;40;7"), "#000000")
  expect_identical(color("30;107;7;27"), "#000000")
  expect_identical(color("38;2;255;255;255;40;7"), "#000000")
  expect_identical(color("38;5;231;40;7"), "#000000")
  expect_identical(color("7;40"), "")
  expect_identical(color("97;40;7;39"), "")
  expect_identical(color("1;31;107;7"), "")
  expect_identical(color("1;31;107;7;22"), "#FFFFFF")
  expect_identical(color("2;97;40;7"), "")
  expect_identical(color("8;97;40;7"), "")
  expect_identical(color("8;97;40;7;28"), "#000000")
})

test_that("cursor contrast uses sRGB luminance for intermediate and saturated colors", {
  color <- function(rgb) .cursor_color(list(color = TRUE,
    theme = list(colors = c(panel = paste(c(48L, 2L, rgb), collapse = ";")))))
  expect_identical(color(rep(117L, 3L)), "#FFFFFF")
  expect_identical(color(rep(118L, 3L)), "#000000")
  expect_identical(color(c(255L, 0L, 0L)), "#000000")
  expect_identical(color(c(0L, 255L, 0L)), "#000000")
  expect_identical(color(c(0L, 0L, 255L)), "#FFFFFF")
})

test_that("unknown, disabled and monochrome surfaces retain host cursor colors", {
  cfg <- repl_config(theme = "black", color = FALSE, settings_file = NULL)
  expect_identical(.cursor_color(cfg), "")
  cfg <- repl_config(theme = "mono", color = TRUE, settings_file = NULL)
  expect_identical(.cursor_color(cfg), "")
  expect_identical(.cursor_color(list()), "")
  expect_identical(.cursor_color(list(color = TRUE)), "")
  expect_identical(.cursor_color(list(color = TRUE, theme = list(colors = c(panel = "")))), "")
  expect_identical(.cursor_color(list(color = TRUE, theme = list(colors = c(panel = NA_character_)))), "")
  expect_identical(.cursor_color(list(color = TRUE, theme = list(colors = c(panel = "40"))), "missing"), "")
  withr::local_envvar(c(NO_COLOR = "1"))
  expect_identical(.cursor_color(repl_config(theme = "black", settings_file = NULL)), "")
})

test_that("cursor frame metadata preserves content, position and hit-test attributes", {
  cfg <- repl_config(theme = "black", color = TRUE, settings_file = NULL)
  original <- structure(list(lines = c("one", "two"), cursor_row = 1L, cursor_column = 2L,
    layout = list(width = 80L, height = 24L), editor = list(hit = 2L)), class = "cursor_fixture")
  visible <- .cursor_frame(original, cfg)
  expect_identical(visible$cursor_visible, TRUE)
  expect_identical(visible$cursor_color, "#FFFFFF")
  expect_identical(visible[names(original)], unclass(original))
  expect_identical(class(visible), class(original))
  hidden <- .cursor_frame(visible, cfg, visible = FALSE)
  expect_identical(hidden$cursor_visible, FALSE)
  expect_null(hidden$cursor_color)
  expect_true("cursor_color" %in% names(hidden))
  expect_identical(hidden[names(original)], unclass(original))
  expect_identical(.cursor_frame(hidden, cfg)$cursor_color, "#FFFFFF")
  cfg$color <- FALSE
  expect_identical(.cursor_frame(original, cfg)$cursor_color, "")
  expect_error(.cursor_frame(original, cfg, visible = NA), "visible")
  expect_error(.cursor_color(cfg, role = ""), "role")
})
