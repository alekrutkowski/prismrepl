test_that("highlighting preserves R source without evaluation", {
  text <- c("mean(c(1, NA_real_), na.rm = TRUE) # comment",
    "\u03b1 <- 'caf\u00e9'; `odd name` %>% f()",
    'r"---(a \\ string )---"', 'r"[square]"', 'R"{braces}"',
    "if (x > 0) {\n  message(\"hello\")\n}",
    "'an unfinished string", "# comment with \u4e2d", "")
  highlighted <- highlight_r(text)
  strip <- function(x) gsub("\033\\[[0-9;]*m", "", x, perl = TRUE)
  expect_identical(strip(highlighted), text)
  expect_identical(highlight_r(text, color = FALSE), text)
  expect_identical(highlight_r(text, repl_theme("mono")), text)
  expect_identical(highlight_r(character()), character())
  expect_error(highlight_r(NA_character_), "without missing")
})

test_that("strings and comments receive their own colors", {
  theme <- repl_theme(overrides = c(string = "32", comment = "90", keyword = "34"))
  expect_match(highlight_r('"literal"', theme), "\033[32m", fixed = TRUE)
  expect_match(highlight_r("# comment", theme), "\033[90m", fixed = TRUE)
  expect_match(highlight_r("TRUE", theme), "\033[34m", fixed = TRUE)
})


test_that("visible windows retain multiline string context and trailing blanks", {
  text <- paste0('x <- r"---(first\n', 'middle \u4e2d\n', 'last)---"\n', 'x\n')
  rendered <- .highlight_r_window(text, 2L, 5L)
  strip <- function(x) gsub("\033\\[[0-9;]*m", "", x, perl = TRUE)
  expect_identical(strip(rendered), c("middle \u4e2d", 'last)---"', "x", ""))
  expect_match(rendered[[1L]], paste0("\033[", repl_theme()$colors[["string"]], "m"), fixed = TRUE)
  expect_match(rendered[[2L]], paste0("\033[", repl_theme()$colors[["string"]], "m"), fixed = TRUE)
  expect_identical(.highlight_r_window(text, 5L, 5L), "")
  expect_identical(.highlight_r_window("", 1L, 1L), "")
  expect_identical(.highlight_r_window("a\n\nb", 2L, 2L, color = FALSE), "")
})
