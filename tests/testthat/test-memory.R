test_that("native process memory reports a current resident byte count", {
  before <- list(directory = getwd(), sink = sink.number(),
    rng = if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      get(".Random.seed", envir = .GlobalEnv) else NULL)
  bytes <- .process_memory()
  expect_type(bytes, "double")
  expect_length(bytes, 1L)
  expect_false(is.nan(bytes))
  expect_true(is.na(bytes) || (is.finite(bytes) && bytes >= 0))
  available <- .Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin") ||
    (identical(Sys.info()[["sysname"]], "Linux") && file.exists("/proc/self/statm"))
  if (available) {
    expect_false(is.na(bytes))
    expect_gt(bytes, 0)
    expect_match(.process_memory_label(bytes), "^R RAM: [0-9]+[.][0-9] [MG]iB$")
  }
  after <- list(directory = getwd(), sink = sink.number(),
    rng = if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      get(".Random.seed", envir = .GlobalEnv) else NULL)
  expect_identical(after, before)
})

test_that("Linux resident memory agrees with the independent status field", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux") &&
    file.exists("/proc/self/status"), "Linux proc counters are unavailable")
  bytes <- .process_memory()
  status <- readLines("/proc/self/status", warn = FALSE)
  rss <- grep("^VmRSS:[[:space:]]+[0-9]+[[:space:]]+kB$", status, value = TRUE)
  expect_length(rss, 1L)
  reported <- as.double(sub("^VmRSS:[[:space:]]+([0-9]+).*", "\\1", rss)) * 1024
  expect_true(is.finite(bytes) && bytes > 0)
  expect_true(is.finite(reported) && reported > 0)
  # Reading R text allocates memory and kernel accounting is approximate.
  # This tolerance still detects page/byte confusion and virtual-memory use.
  expect_lt(abs(bytes - reported), max(8 * 1024^2, reported / 4))
})

test_that("resident memory labels use stable units and honest unavailability", {
  expect_identical(.process_memory_label(0), "R RAM: 0.0 MiB")
  expect_identical(.process_memory_label(123.5 * 1024^2), "R RAM: 123.5 MiB")
  expect_identical(.process_memory_label(1024^3), "R RAM: 1.0 GiB")
  expect_identical(.process_memory_label(2.5 * 1024^3), "R RAM: 2.5 GiB")
  expect_identical(.process_memory_label(NA_real_), "R RAM: unavailable")
  expect_identical(.process_memory_label(NA_integer_), "R RAM: unavailable")
  invisible(lapply(list(NULL, numeric(), c(1, 2), -1, Inf, NaN, "123", 1 + 1i),
    function(value) expect_error(.process_memory_label(value), "bytes must be")))
})
