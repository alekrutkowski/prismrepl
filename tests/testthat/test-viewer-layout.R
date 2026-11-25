test_that("viewers paint exact surfaces on dark and light themes at every window size", {
  sizes <- list(c(width = 132L, height = 40L), c(width = 55L, height = 14L),
                c(width = 20L, height = 5L), c(width = 8L, height = 2L),
                c(width = 1L, height = 1L))
  invisible(lapply(c("paper", "midnight", "mono"), function(theme) {
    cfg <- repl_config(settings_file = NULL, theme = theme, color = TRUE, color_depth = 16777216L)
    invisible(lapply(sizes, function(size) {
      frame <- .panel_frame(c("\u03b1\u6f22e\u0301", strrep("wide ", 40L)), "Function documentation", cfg,
                            size, caption = "Arrows scroll / search q close")
      expect_length(frame, size[["height"]])
      expect_true(all(vapply(frame, .screen_width, numeric(1L)) < max(2L, size[["width"]])))
      if (theme != "mono") expect_true(all(grepl("48;2;", frame, fixed = TRUE)))
    }))
  }))
})

test_that("viewer selection and paging preserve text markers without color", {
  cfg <- repl_config(settings_file = NULL, color = FALSE)
  frame <- .panel_frame(c("first", "second", "third"), "Objects", cfg,
    c(width = 40L, height = 8L), cursor = 2L, top = 2L, selected = c(FALSE, TRUE, FALSE))
  expect_true(any(grepl("> [x] second", frame, fixed = TRUE)))
  expect_false(any(grepl("first", frame, fixed = TRUE)))
  expect_length(frame, 8L)
  expect_error(.panel_frame("one", "Objects", cfg, c(width = 40L, height = 8L), selected = logical()))
})
