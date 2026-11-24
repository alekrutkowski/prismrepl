test_that("installed help renders as text without control characters", {
  lines <- help_text("mean", package = "base", width = 60)
  expect_true(any(grepl("mean", lines, fixed = TRUE)))
  expect_false(any(grepl("\b", lines, fixed = TRUE)))
  expect_false(any(grepl("\033", lines, fixed = TRUE)))
  expect_identical(help_text("stats::lm", width = 60),
                   help_text("lm", package = "stats", width = 60))
  expect_match(help_text("prismrepl_unknown_topic_xyz", package = "base"),
               "No documentation")
  expect_error(help_text("base::mean", package = "stats"), "agree")
})

test_that("package indexes contain topics and missing packages explain the problem", {
  lines <- help_text(package = "base")
  expect_true(any(lines == "Topics"))
  expect_true(any(grepl("mean", lines, fixed = TRUE)))
  expect_match(help_text(package = "prismrepl_unknown_package_xyz"), "not installed")
})

test_that("Rd rendering preserves accented, Greek, and CJK text", {
  input <- paste0("\\name{unicodefixture}\n\\alias{unicodefixture}\n",
                  "\\encoding{UTF-8}\n\\title{caf\u00e9 \u03b1 \u4e2d\u6587}\n",
                  "\\description{Unicode content: na\u00efve, \u03b2, \u6771\u4eac.}\n")
  con <- textConnection(input, encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  fixture <- tools::parse_Rd(con)
  lines <- .help_render_rd(fixture, package = "fixture", width = 80L)
  text <- paste(lines, collapse = "\n")
  expect_match(text, "caf\u00e9", fixed = TRUE)
  expect_match(text, "\u03b1", fixed = TRUE)
  expect_match(text, "\u4e2d\u6587", fixed = TRUE)
  expect_false(grepl("\b", text, fixed = TRUE))
})


test_that("cached help follows text-formatting options", {
  saved <- tools::Rd2txt_options()
  on.exit(tools::Rd2txt_options(saved), add = TRUE)
  invisible(tools::Rd2txt_options(code_quote = TRUE))
  quoted <- help_text("mean", package = "base", width = 60)
  expect_identical(help_text("mean", package = "base", width = 60), quoted)
  invisible(tools::Rd2txt_options(code_quote = FALSE))
  unquoted <- help_text("mean", package = "base", width = 60)
  expect_false(identical(quoted, unquoted))
})

test_that("dynamic render-stage Rd expressions bypass rendered-page caching", {
  input <- paste0("\\name{dynamicfixture}\n\\alias{dynamicfixture}\n",
                  "\\title{Dynamic fixture}\n\\description{Current value: ",
                  "\\Sexpr[stage=render]{getOption('prismrepl_fixture', 'first')}}\n")
  con <- textConnection(input)
  on.exit(close(con), add = TRUE)
  fixture <- tools::parse_Rd(con)
  saved <- options(prismrepl_fixture = "first")
  on.exit(options(saved), add = TRUE)
  first <- .help_cached_render("base", 999999L, fixture, 80L)
  options(prismrepl_fixture = "second")
  second <- .help_cached_render("base", 999999L, fixture, 80L)
  expect_true(any(grepl("first", first, fixed = TRUE)))
  expect_true(any(grepl("second", second, fixed = TRUE)))
})
