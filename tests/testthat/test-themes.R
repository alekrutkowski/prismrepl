test_that("themes expose complete validated palettes", {
  expect_gte(length(repl_themes()), 6L)
  palettes <- lapply(repl_themes(), repl_theme)
  expect_true(all(vapply(palettes, function(x)
    identical(names(x$colors), names(palettes[[1L]]$colors)), logical(1))))
  expect_equal(repl_theme("paper", c(error = "1;31"))$colors[["error"]], "1;31")
  expect_error(repl_theme("missing"), "theme name")
  expect_error(repl_theme(overrides = c(error = "\033[31m")), "SGR")
  expect_error(repl_theme(overrides = c(no_such_role = "31")), "known roles")
  expect_identical(style_text("hello", "error", color = FALSE), "hello")
  expect_identical(style_text("hello", "error", repl_theme("mono")), "hello")
})

test_that("display escaping prevents terminal control injection", {
  text <- paste0("normal\033]52;c;clipboard\007\r", "\u03b1\u4e2d\n\tend")
  safe <- .safe_text(text)
  expect_false(grepl("\033", safe, fixed = TRUE))
  expect_false(grepl("\r", safe, fixed = TRUE))
  expect_true(grepl("\u03b1\u4e2d\n    end", safe, fixed = TRUE))
  expect_identical(.safe_text(character()), character())
  expect_identical(.safe_text(c("", NA_character_)), c("", "NA"))
})


test_that("surface and badge roles emit real foreground and background RGB", {
  theme <- repl_theme("midnight")
  expect_equal(theme$color_depth, 16777216L)
  expect_true(all(vapply(theme$colors[c("background", "panel", "panel_header", "status",
    "error_badge", "warning_badge", "message_badge")], function(x)
      grepl("38;2;", x, fixed = TRUE) && grepl("48;2;", x, fixed = TRUE), logical(1))))
  expect_equal(repl_theme("paper")$mode, "light")
  expect_equal(repl_theme("midnight")$mode, "dark")
  expect_false(identical(repl_theme("paper")$palette$background,
                         repl_theme("midnight")$palette$background))
})

test_that("themes downgrade colors while retaining semantic badge distinction", {
  small <- repl_theme("paper", color_depth = 16L)
  indexed <- repl_theme("midnight", color_depth = 256L)
  expect_false(any(grepl("(?:38|48);[25];", small$colors, perl = TRUE)))
  expect_true(all(grepl("48;5;", indexed$colors[c("panel", "error_badge", "warning_badge")], fixed = TRUE)))
  expect_equal(small$colors[["error_badge"]], "1;97;41")
  expect_equal(small$colors[["warning_badge"]], "1;30;43")
  expect_equal(small$colors[["message_badge"]], "1;97;44")
  expect_true(all(repl_theme(color_depth = 0L)$colors == ""))
})

test_that("color depth changes preserve friendly and legacy custom styles", {
  original <- repl_theme(overrides = list(prompt = "#A7F3D0",
    panel = list(fg = "#FFFFFF", bg = "#182433"), error = "1;31",
    message_badge = "1;38;2;255;255;255;48;2;0;0;205"))
  changed <- .theme_at_depth(original, 16L)
  restored <- .theme_at_depth(changed, 16777216L)
  expect_identical(changed$overrides, original$overrides)
  expect_identical(changed$styles, original$styles)
  expect_identical(restored$colors, original$colors)
  expect_equal(changed$colors[["error"]], "1;31")
  expect_false(grepl("38;2|48;2", changed$colors[["message_badge"]]))
  expect_error(repl_theme(overrides = list(panel = list(bg = "red"))), "hex colors")
})

test_that("native styles survive cli suppression and restore nested panel backgrounds", {
  withr::local_envvar(c(NO_COLOR = "1"))
  withr::local_options(cli.num_colors = 1L)
  theme <- repl_theme()
  colored <- style_text("visible", "error_badge", theme, color = TRUE)
  expect_match(colored, "\033[", fixed = TRUE)
  inner <- style_text("token", "keyword", theme)
  outer <- style_text(paste0("before ", inner, " after\nnext"), "panel", theme)
  panel <- paste0("\033[", theme$colors[["panel"]], "m")
  expect_match(outer, paste0("\033[0m", panel, " after"), fixed = TRUE)
  expect_match(outer, paste0("\033[0m\n", panel, "next"), fixed = TRUE)
  expect_identical(gsub("\033\\[[0-9;]*m", "", outer, perl = TRUE), "before token after\nnext")
})
