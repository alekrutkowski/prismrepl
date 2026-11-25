# Run from the checkout root after installing the package:
# Rscript tools/test-appearance-independence.R
# Test the real rendering checks against both possible first-run appearances.
# The package's suite setup keeps personal preference files out of these runs.

#' Check Rendering Tests Under Light and Dark Initial Appearances
#' @param root Package checkout directory containing the test suite.
#' @return A data frame with successful test and expectation totals by appearance.
check_appearance_independence <- function(root = ".") {
  stopifnot("root must be one existing package directory" =
    is.character(root) && length(root) == 1L && !is.na(root) && dir.exists(root),
    "prismrepl must be installed" = requireNamespace("prismrepl", quietly = TRUE))
  directory <- file.path(root, "tests", "testthat")
  stopifnot("the real package test setup must exist" =
    file.exists(file.path(directory, "setup-settings.R")))
  inherited <- Sys.getenv("PRISMREPL_CONFIG_FILE", unset = NA_character_)
  initial <- get(".initial_theme", asNamespace("prismrepl"))
  # Keep source and target palettes at the same depth, so a capability change
  # cannot accidentally satisfy an assertion intended to detect recoloring.
  withr::local_options(cli.num_colors = 16777216L)
  rows <- lapply(c("paper", "midnight"), function(appearance) {
    calls <- 0L
    testthat::local_mocked_bindings(.initial_theme = function(refresh = FALSE) {
      calls <<- calls + 1L
      appearance
    }, .package = "prismrepl")
    cat("Checking rendering with injected initial theme: ", appearance, "\n", sep = "")
    result <- testthat::test_dir(directory,
      filter = "^(matching-pairs|screen|files-footer|rich-help|theme-picker|cursor)$",
      reporter = "summary", package = "prismrepl", load_package = "installed",
      stop_on_failure = TRUE, stop_on_warning = TRUE)
    totals <- as.data.frame(result)
    stopifnot("the injected appearance must be exercised" = calls > 0L,
      "rendering tests must pass without warnings, errors or skips" =
        nrow(totals) > 0L && sum(totals$failed) == 0L && sum(totals$warning) == 0L &&
        !any(totals$error) && !any(totals$skipped),
      "the inherited preference override must be restored" =
        identical(Sys.getenv("PRISMREPL_CONFIG_FILE", unset = NA_character_), inherited))
    data.frame(theme = appearance, tests = nrow(totals),
      expectations = sum(totals$passed), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  stopifnot("both appearances must be verified" =
    identical(out$theme, c("paper", "midnight")) && all(out$expectations > 0L),
    "the real appearance detector must be restored" =
      identical(get(".initial_theme", asNamespace("prismrepl")), initial))
  print(out, row.names = FALSE)
  cat("PASS: light and dark appearances, full color depth, isolated preferences and restored detector.\n")
  invisible(out)
}

check_appearance_independence()
