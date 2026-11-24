test_that("Mistral URL encoding preserves Unicode and reserved characters", {
  msg <- "object '\u03b1 & x+1 %2F' not found"
  code <- "\u03b1 <- '\u6f22\u5b57'"
  link <- mistral_link(msg, code)
  expect_true(startsWith(link, "https://chat.mistral.ai/chat?q="))
  decoded <- utils::URLdecode(sub("^.*[?]q=", "", link))
  expect_match(decoded, msg, fixed = TRUE)
  expect_match(decoded, code, fixed = TRUE)
  expect_false(grepl("[&]", link))
  expect_false(grepl("utm_", link, fixed = TRUE))
})

test_that("Mistral context is bounded and can be omitted", {
  link <- mistral_link("error", paste(rep("\U0001f9ea", 10000L), collapse = ""))
  expect_lte(nchar(link, type = "bytes"), 8000L)
  expect_match(utils::URLdecode(link), "context truncated", fixed = TRUE)
  link <- mistral_link("error", "sensitive-code", "sensitive-frame", include_context = FALSE)
  expect_false(grepl("sensitive", link, fixed = TRUE))
  expect_error(mistral_link(NA_character_))
})

test_that("terminal control characters cannot escape condition displays", {
  text <- prismrepl:::.safe_text("error\033]8;;https://example.com\007\r")
  expect_false(grepl("\033", text, fixed = TRUE))
  expect_false(grepl("\007", text, fixed = TRUE))
  expect_match(text, "\\x1b", fixed = TRUE)
})


.condition_test_config <- function(color = TRUE, depth = 16777216L, width = 48L,
                                    unicode = TRUE, hook = NULL, hyperlinks = FALSE) {
  structure(list(theme = repl_theme(color_depth = depth), color = color,
    color_depth = depth, width = width, unicode = unicode, output_hook = hook,
    hyperlinks = hyperlinks, link_context = FALSE), class = "prismrepl_config")
}

test_that("condition cards have distinct badges even without color", {
  cfg <- .condition_test_config(color = FALSE)
  lines <- lapply(c("error", "warning", "message"), function(role)
    .condition_lines(paste0(tools::toTitleCase(role), ": explanation"), role, cfg))
  expect_true(all(vapply(seq_along(lines), function(i)
    any(grepl(c("[ERROR]", "[WARNING]", "[MESSAGE]")[[i]], lines[[i]], fixed = TRUE)), logical(1))))
  expect_true(all(vapply(lines, function(x) any(grepl("explanation", x, fixed = TRUE)), logical(1))))
  expect_false(any(grepl("\033", unlist(lines), fixed = TRUE)))
  expect_false(any(grepl("Error: explanation|Warning: explanation|Message: explanation", unlist(lines))))
  once <- .condition_lines("Warning: Warning: retained", "warning", cfg)
  expect_true(any(grepl("Warning: retained", once, fixed = TRUE)))
})

test_that("condition cards wrap Unicode by cells and retain readable traces", {
  cfg <- .condition_test_config(color = FALSE, width = 32L)
  lines <- .condition_lines(paste0("\u6f22\u5b57 e\u0301 ", strrep("longword", 10L),
    "\nsecond paragraph"), "error", cfg, trace = c("model(data)", "predict(model, newdata)"))
  expect_true(all(utf8::utf8_width(lines, encode = FALSE, utf8 = TRUE) == 32L))
  expect_true(any(grepl("1. model(data)", lines, fixed = TRUE)))
  expect_true(any(grepl("2. predict", lines, fixed = TRUE)))
  expect_true(any(grepl("\u6f22\u5b57 e\u0301", lines, fixed = TRUE)))
  expect_identical(attr(lines, "prism_condition")$trace, c("model(data)", "predict(model, newdata)"))
  narrow <- .condition_lines("a longer diagnostic", "warning", cfg, width = 12L)
  expect_true(all(utf8::utf8_width(narrow, encode = FALSE, utf8 = TRUE) <= 12L))
  expect_equal(attr(narrow, "prism_condition")$role, "warning")
})

test_that("native color badges work without cli color capability detection", {
  withr::local_options(cli.num_colors = 1L)
  cfg <- .condition_test_config()
  error <- .condition_lines("bad input", "error", cfg)
  warning <- .condition_lines("check input", "warning", cfg)
  message <- .condition_lines("completed step", "message", cfg)
  expect_match(error[[1L]], paste0("\033[", cfg$theme$colors[["error_badge"]], "m"), fixed = TRUE)
  expect_match(warning[[1L]], paste0("\033[", cfg$theme$colors[["warning_badge"]], "m"), fixed = TRUE)
  expect_match(message[[1L]], paste0("\033[", cfg$theme$colors[["message_badge"]], "m"), fixed = TRUE)
  expect_true(grepl("48;2;", error[[1L]], fixed = TRUE))
})

test_that("hooks receive formatted conditions and links without print leakage", {
  captured <- list()
  hook <- function(lines, role) { captured[[length(captured) + 1L]] <<- list(lines = lines, role = role); invisible(NULL) }
  cfg <- .condition_test_config(hook = hook, hyperlinks = TRUE)
  leaked <- utils::capture.output({
    .emit("normal output", "output", cfg)
    .emit("Warning: watch this", "warning", cfg)
    .emit("information", "message", cfg)
    .show_error(list(error = simpleError("failure"), code = "f()", trace = "f()"), cfg)
  })
  expect_length(leaked, 0L)
  expect_identical(vapply(captured, `[[`, character(1), "role"),
                   c("output", "warning", "message", "error", "link"))
  expect_equal(attr(captured[[2L]]$lines, "prism_condition")$text, "Warning: watch this")
  expect_match(captured[[5L]]$lines, "\033]8;;https://chat.mistral.ai/chat?q=", fixed = TRUE)
  expect_false(any(grepl("\033]52", captured[[4L]]$lines, fixed = TRUE)))
})

test_that("conditions strip incoming style but escape other terminal controls", {
  cfg <- .condition_test_config(color = FALSE, unicode = FALSE)
  lines <- .condition_lines("\033[31mcolored\033[0m\033]52;c;payload\007", "message", cfg)
  expect_false(any(grepl("\033", lines, fixed = TRUE)))
  expect_true(any(grepl("colored", lines, fixed = TRUE)))
  expect_true(any(grepl("\\x1b]52", lines, fixed = TRUE)))
  expect_false(any(grepl("\\x1b[31m", lines, fixed = TRUE)))
})
