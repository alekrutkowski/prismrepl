# Run from the checkout root: Rscript tools/test-settings-isolation.R
# This checks the actual suite setup, including its lifetime and cleanup,
# without requiring a package installation or recursively running its tests.

#' Verify Package Test Preference Isolation Across Test Files
#' @param root Package checkout directory.
#' @return TRUE invisibly after the lifetime and cleanup checks succeed.
check_settings_isolation <- function(root = ".") {
  stopifnot("root must be one existing package directory" =
    is.character(root) && length(root) == 1L && !is.na(root) && dir.exists(root))
  setup <- file.path(root, "tests", "testthat", "setup-settings.R")
  stopifnot("the real package test setup must exist" = file.exists(setup))
  directory <- withr::local_tempdir("prism-test-lifetime-")
  inherited <- file.path(directory, "personal-preferences.dcf")
  writeLines(c("Version: 1", "AI-Provider: chatgpt", "Theme: white"), inherited)
  original <- readBin(inherited, "raw", n = file.info(inherited)$size)
  withr::local_envvar(c(PRISMREPL_CONFIG_FILE = inherited))

  suite <- file.path(directory, "tests")
  stopifnot("probe suite directory must be created" = dir.create(suite),
    "the real setup file must be copied unchanged" = file.copy(setup, suite))
  fixture <- new.env(parent = baseenv())
  fixture$inherited <- inherited
  fixture$seen <- character()
  probe <- c(
    'testthat::test_that("suite setup outlives each sourced file", {',
    '  current <- Sys.getenv("PRISMREPL_CONFIG_FILE")',
    '  testthat::expect_false(identical(current, fixture$inherited))',
    '  testthat::expect_false(file.exists(current))',
    '  testthat::expect_true(dir.exists(dirname(current)))',
    '  fixture$seen <- c(fixture$seen, current)',
    '})'
  )
  invisible(lapply(c("first", "second"), function(name)
    writeLines(probe, file.path(suite, paste0("test-", name, ".R")))))
  env <- new.env(parent = globalenv())
  env$fixture <- fixture
  result <- testthat::test_dir(suite, reporter = "summary", env = env,
    stop_on_failure = TRUE, stop_on_warning = TRUE)
  totals <- as.data.frame(result)
  stopifnot("both probe files must run successfully" =
    nrow(totals) == 2L && sum(totals$failed) == 0L && !any(totals$error),
    "the same isolated path must remain active across files" =
      length(fixture$seen) == 2L && length(unique(fixture$seen)) == 1L,
    "the inherited environment must be restored after the suite" =
      identical(Sys.getenv("PRISMREPL_CONFIG_FILE"), inherited),
    "the original preferences must remain byte-identical" =
      identical(original, readBin(inherited, "raw", n = file.info(inherited)$size)),
    "the isolated directory must be cleaned up after the suite" =
      !dir.exists(dirname(fixture$seen[[1L]])))
  cat("PASS: two files, suite lifetime, environment restoration, unchanged preferences and cleanup.\n")
  invisible(TRUE)
}

check_settings_isolation()
