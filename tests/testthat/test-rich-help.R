test_that("rich help preserves Unicode, semantic emphasis, and literal R code", {
  cfg <- repl_config(color = TRUE, color_depth = 16777216L, settings_file = NULL)
  html <- c("<h2>A caf&#233; &alpha; &lt;title&gt;</h2>",
    "<p>A <strong>bold <em>bold italic</em></strong> and <em>italic</em> paragraph.",
    "&minus;2 &times; 3 &le; 4 &amp; e&#x301; &#x754c;.</p>",
    "<pre><code class='language-R'>x &lt;- c(1, NA)\nmean(x, na.rm = TRUE)</code></pre>",
    "<p><code>median(x)</code> preserves a code label.</p>")
  styled <- .help_html_lines(html, 55L, cfg)
  plain <- gsub(.screen_ansi_pattern(), "", styled, perl = TRUE)
  expect_match(paste(plain, collapse = "\n"), "caf\u00e9 \u03b1 <title>", fixed = TRUE)
  expect_match(paste(plain, collapse = " "), "\u22122 \u00d7 3 \u2264 4 & e\u0301 \u754c", fixed = TRUE)
  expect_true(any(grepl("\033[1m", styled, fixed = TRUE)))
  expect_true(any(grepl("\033[3m", styled, fixed = TRUE)))
  expect_true(any(grepl("\033[1;3m", styled, fixed = TRUE)))
  expect_true(any(grepl(paste0("\033[", cfg$theme$colors[["number"]], "m1"), styled, fixed = TRUE)))
  expect_true(any(plain == "x <- c(1, NA)"))
  expect_true(any(plain == "mean(x, na.rm = TRUE)"))
  expect_false(any(grepl("&lt;|&#|<h2|</", plain)))
  expect_true(all(vapply(styled, .screen_width, numeric(1L)) <= 55L))
})

test_that("documentation ignores executable HTML and neutralizes terminal controls", {
  cfg <- repl_config(color = FALSE, settings_file = NULL)
  html <- c("<script>do_not_show()</script><style>.do_not_show {color:red}</style>",
    "<p>Visible <unknown>content</unknown> &unsupported; &#0; &#xD800;.",
    "\033[2Jbad control</p>", "<ul><li>First <em>entry</em></li><li>Second entry</li></ul>",
    "<ol><li>Number one</li><li>Number two</li></ol>")
  out <- .help_html_lines(html, 60L, cfg)
  expect_false(any(grepl("do_not_show", out, fixed = TRUE)))
  expect_false(any(grepl("\033", out, fixed = TRUE)))
  expect_true(any(grepl("\\x1b[2J", out, fixed = TRUE)))
  expect_true(any(grepl("&unsupported;", out, fixed = TRUE)))
  expect_true(any(grepl("\ufffd", out, fixed = TRUE)))
  expect_true(any(grepl("First entry", out, fixed = TRUE)))
  expect_true(any(grepl("^1[.] Number one", out)))
  expect_true(any(grepl("^2[.] Number two", out)))
})

test_that("real installed rich pages keep arguments and change colors with the theme", {
  dark <- repl_config(theme = "midnight", color = TRUE, color_depth = 16777216L, settings_file = NULL)
  light <- repl_config(theme = "white", color = TRUE, color_depth = 16777216L, settings_file = NULL)
  first <- .help_rich_text("base::mean", NULL, 70L, dark)
  second <- .help_rich_text("mean", "base", 70L, light)
  plain <- function(x) gsub(.screen_ansi_pattern(), "", x, perl = TRUE)
  expect_identical(plain(first), plain(second))
  expect_true(any(grepl("^na.rm: a logical", plain(first))))
  expect_true(any(grepl("Examples", plain(first), fixed = TRUE)))
  expect_true(any(grepl("\033[3m", first, fixed = TRUE)))
  expect_false(identical(first, second))
  expect_true(any(grepl(paste0("\033[", light$theme$colors[["heading"]], "m"), second, fixed = TRUE)))
  expect_false(any(grepl("\b", second, fixed = TRUE)))
  missing <- .help_rich_text("prismrepl_unknown_topic_xyz", "base", 70L, dark)
  expect_match(missing, "No documentation")
})

test_that("every literal occurrence is found without matching ANSI controls", {
  cfg <- repl_config(color = TRUE, settings_file = NULL)
  source <- c(highlight_r("mean(x) + mean(y) + MEAN(z)", cfg$theme, TRUE),
    "\u754c e\u0301 MEAN and mean", "an unrelated line")
  hits <- .panel_matches(source, "mean")
  expect_identical(hits$id, 1:5)
  expect_identical(hits$row, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(hits$column[1:3], c(0L, 10L, 20L))
  expect_equal(hits$column[4:5], c(5L, 14L))
  expect_equal(nrow(.panel_matches(source, "38;2")), 0L)
  painted <- .panel_highlight(source[[1L]], hits[hits$row == 1L, ], 2L, cfg)
  expect_identical(gsub(.screen_ansi_pattern(), "", painted, perl = TRUE),
    "mean(x) + mean(y) + MEAN(z)")
  expect_equal(stringi::stri_count_fixed(painted, paste0("\033[", cfg$theme$colors[["warning_badge"]], "m")), 2L)
  expect_equal(stringi::stri_count_fixed(painted, paste0("\033[", cfg$theme$colors[["success_badge"]], "m")), 1L)
  next_hit <- .panel_match_step(hits, 1L, 1L)
  expect_identical(next_hit$index, 2L)
  expect_identical(next_hit$row, 1L)
  expect_identical(.panel_match_step(hits, 1L, -1L)$index, 5L)
  expect_identical(.panel_match_step(hits, 5L, 1L)$index, 1L)
  expect_equal(.panel_reveal_match(hits, 3L, 0L, 80L), 0L)
  expect_gt(.panel_reveal_match(hits, 3L, 0L, 12L), 0L)
  expect_equal(nrow(.panel_matches(source, "")), 0L)
})

test_that("match highlighting and panning preserve whole Unicode graphemes", {
  cfg <- repl_config(color = TRUE, settings_file = NULL)
  line <- style_text("\u754c e\u0301 \U0001f469\u200d\U0001f4bb e\u0301", "string", cfg$theme, TRUE)
  hits <- .panel_matches(line, "e")
  expect_identical(hits$first, c(3L, 7L))
  highlighted <- .panel_highlight(line, hits, 1L, cfg)
  expect_identical(gsub(.screen_ansi_pattern(), "", highlighted, perl = TRUE),
    "\u754c e\u0301 \U0001f469\u200d\U0001f4bb e\u0301")
  slice <- .panel_slice_ansi(highlighted, 3L, 4L)
  expect_identical(gsub(.screen_ansi_pattern(), "", slice, perl = TRUE), "e\u0301 \U0001f469\u200d\U0001f4bb")
  expect_equal(.screen_width(slice), 4L)
})

test_that("a searched phrase can span a wrapped line without crossing paragraphs", {
  cfg <- repl_config(color = TRUE, settings_file = NULL)
  lines <- c("The arithmetic", "  mean is useful; arithmetic mean is common.",
    "", "Arithmetic", "", "mean is in another paragraph.")
  hits <- .panel_matches(lines, "arithmetic mean")
  expect_equal(.panel_match_count(hits), 2L)
  expect_identical(hits$id, c(1L, 1L, 2L))
  expect_identical(hits$row, c(1L, 2L, 2L))
  expect_identical(.panel_match_step(hits, 1L)$index, 2L)
  expect_identical(.panel_match_step(hits, 2L)$index, 1L)
  painted <- .panel_highlight(lines[[2L]], hits[hits$row == 2L, ], 1L, cfg)
  expect_identical(gsub(.screen_ansi_pattern(), "", painted, perl = TRUE), lines[[2L]])
})

test_that("pager search advances through repeated occurrences on one line", {
  cfg <- repl_config(color = TRUE, settings_file = NULL)
  event <- function(text = "", key = "", type = if (nzchar(text)) "text" else "key")
    list(type = type, text = text, key = key, modifiers = character())
  events <- list(event("/"), event("mean"), event(key = "enter"), event("n"), event("n"), event("p"), event("q"))
  frames <- list()
  env <- new.env(parent = environment(.tui_panel))
  env$.viewer_start <- function(config) list(handle = TRUE)
  env$.viewer_finish <- function(...) invisible(NULL)
  env$term_size <- function() c(width = 80L, height = 12L)
  env$term_read <- function(...) {
    out <- events[[1L]]
    events <<- events[-1L]
    out
  }
  env$.panel_draw <- function(lines, previous, size, config) {
    frames[[length(frames) + 1L]] <<- lines
    list(lines = lines)
  }
  panel <- .tui_panel
  environment(panel) <- env
  panel(c("mean mean MEAN", "Second line"), "Documentation", cfg)
  statuses <- vapply(frames, function(frame) gsub(.screen_ansi_pattern(), "", frame[[11L]], perl = TRUE), character(1L))
  expect_true(any(grepl("Match 1/3", statuses, fixed = TRUE)))
  expect_true(any(grepl("Match 2/3", statuses, fixed = TRUE)))
  expect_true(any(grepl("Match 3/3", statuses, fixed = TRUE)))
  expect_true(all(grepl("Line 1/2", statuses, fixed = TRUE)))
  events <- list(event("/"), event("TARGET"), event(key = "enter"), event("q"))
  frames <- list()
  panel(c(rep("Previous context", 20L), "TARGET heading", "Following context", rep("Later", 10L)),
    "Documentation", cfg)
  found <- Filter(function(frame) any(grepl("Match 1/1", frame, fixed = TRUE)), frames)
  expect_gt(length(found), 0L)
  expect_true(all(vapply(found, function(frame) any(grepl("Following context", frame, fixed = TRUE)), logical(1L))))
})
