file_preview_fixture <- function(raw, extension = ".log") {
  path <- tempfile("preview ", fileext = extension)
  writeBin(raw, path)
  withr::defer(unlink(path), envir = parent.frame())
  path
}

file_preview_unicode <- function(text, encoding, bom = TRUE) {
  marks <- list(`UTF-8` = as.raw(c(239, 187, 191)),
    `UTF-16LE` = as.raw(c(255, 254)), `UTF-16BE` = as.raw(c(254, 255)),
    `UTF-32LE` = as.raw(c(255, 254, 0, 0)), `UTF-32BE` = as.raw(c(0, 0, 254, 255)))
  bytes <- stringi::stri_encode(text, from = "UTF-8", to = encoding, to_raw = TRUE)[[1L]]
  c(if (bom) marks[[encoding]] else raw(), bytes)
}

test_that("ordinary ini and log files preview Unicode content regardless of extension", {
  text <- "[settings]\r\nname=caf\u00e9\r\npath=C:\\Users\\\u0141\u00f3d\u017a\r\nvalue=\u0394 \u65e5\u672c \U0001f603"
  expected <- c("[settings]", "name=caf\u00e9", "path=C:\\Users\\\u0141\u00f3d\u017a", "value=\u0394 \u65e5\u672c \U0001f603")
  invisible(lapply(c(".ini", ".log", ".dat"), function(extension) {
    bytes <- charToRaw(enc2utf8(text))
    path <- file_preview_fixture(bytes, extension)
    preview <- .file_preview(path)
    expect_identical(preview$lines, expected)
    expect_identical(preview$language, "text")
    expect_identical(preview$encoding, "UTF-8")
    expect_false(preview$truncated)
    expect_identical(readBin(path, "raw", n = length(bytes) + 1L), bytes)
  }))
})

test_that("Unicode BOMs are decoded before any binary decision", {
  text <- "[settings]\r\nname=caf\u00e9\r\nvalue=\u4e2d \U0001f603\tend"
  expected <- c("[settings]", "name=caf\u00e9", "value=\u4e2d \U0001f603    end")
  invisible(lapply(c("UTF-8", "UTF-16LE", "UTF-16BE", "UTF-32LE", "UTF-32BE"), function(encoding) {
    bytes <- file_preview_unicode(text, encoding)
    path <- file_preview_fixture(bytes, ".ini")
    preview <- .file_preview(path)
    expect_identical(preview$lines, expected)
    expect_identical(preview$encoding, encoding)
    expect_false(preview$truncated)
    expect_identical(readBin(path, "raw", n = length(bytes) + 1L), bytes)
  }))
})

test_that("BOMless UTF-16 requires strong printable and reversible evidence", {
  text <- "[settings]\r\nname=caf\u00e9\r\nvalue=\U0001f603"
  invisible(lapply(c("UTF-16LE", "UTF-16BE"), function(encoding) {
    bytes <- file_preview_unicode(text, encoding, bom = FALSE)
    path <- file_preview_fixture(bytes)
    preview <- .file_preview(path)
    expect_identical(preview$encoding, encoding)
    expect_identical(preview$lines, c("[settings]", "name=caf\u00e9", "value=\U0001f603"))
  }))
  ambiguous <- list(file_preview_unicode("a", "UTF-16LE", bom = FALSE),
    file_preview_unicode("apparently text\001", "UTF-16LE", bom = FALSE),
    file_preview_unicode(text, "UTF-32LE", bom = FALSE),
    c(file_preview_unicode("name=value", "UTF-16LE", bom = FALSE), as.raw(c(0, 216))))
  invisible(lapply(ambiguous, function(bytes) {
    preview <- .file_preview(file_preview_fixture(bytes))
    expect_match(preview$lines, "Binary file", fixed = TRUE)
    expect_match(preview$lines, "byte order mark", fixed = TRUE)
  }))
})

test_that("binary content stays binary even with text filenames or Unicode BOMs", {
  samples <- list(as.raw(c(80, 75, 3, 4, 0, 1, 2)), as.raw(c(1, 2, 3, 4)),
    c(as.raw(c(255, 254)), as.raw(c(65, 0, 0, 0, 66, 0))))
  invisible(lapply(samples, function(bytes) {
    preview <- .file_preview(file_preview_fixture(bytes, ".log"))
    expect_match(preview$lines, "Binary file", fixed = TRUE)
  }))
  text <- "\033[31mERROR\033[0m"
  invisible(lapply(c("UTF-8", "UTF-16LE"), function(encoding) {
    preview <- .file_preview(file_preview_fixture(file_preview_unicode(text, encoding)))
    expect_identical(preview$lines, "\\x1b[31mERROR\\x1b[0m")
    expect_false(any(grepl("\033", preview$lines, fixed = TRUE)))
  }))
})

test_that("byte boundaries never invent malformed trailing Unicode characters", {
  utf8 <- file_preview_fixture(charToRaw(enc2utf8("A\u00e9\U0001f603Z")))
  invisible(lapply(list(c(2L, "A"), c(5L, "A\u00e9"), c(7L, "A\u00e9\U0001f603")), function(case) {
    preview <- .file_preview(utf8, bytes = as.integer(case[[1L]]))
    expect_identical(preview$lines[[1L]], case[[2L]])
    expect_true(preview$truncated)
    expect_false(any(grepl("Invalid", preview$lines, fixed = TRUE)))
  }))
  invisible(lapply(c("UTF-16LE", "UTF-16BE"), function(encoding) {
    path <- file_preview_fixture(file_preview_unicode("A\U0001f603Z", encoding))
    invisible(lapply(c(5L, 6L, 7L), function(limit) {
      preview <- .file_preview(path, bytes = limit)
      expect_identical(preview$lines[[1L]], "A")
      expect_true(preview$truncated)
      expect_false(any(grepl("Invalid", preview$lines, fixed = TRUE)))
    }))
    expect_identical(.file_preview(path, bytes = 8L)$lines[[1L]], "A\U0001f603")
    expect_identical(.file_preview(path, bytes = 1L)$lines[[1L]], "")
  }))
  invisible(lapply(c("UTF-32LE", "UTF-32BE"), function(encoding) {
    path <- file_preview_fixture(file_preview_unicode("A\U0001f603Z", encoding))
    preview <- .file_preview(path, bytes = 9L)
    expect_identical(preview$lines[[1L]], "A")
    expect_true(preview$truncated)
    expect_false(any(grepl("Invalid", preview$lines, fixed = TRUE)))
  }))
})

test_that("malformed complete files explain replacement without changing the original", {
  samples <- list(UTF8 = charToRaw("prefix\303"),
    UTF16 = c(as.raw(c(255, 254)), file_preview_unicode("prefix", "UTF-16LE", bom = FALSE), as.raw(c(0, 216))))
  invisible(lapply(samples, function(bytes) {
    path <- file_preview_fixture(bytes)
    preview <- .file_preview(path)
    expect_match(preview$lines[[1L]], "prefix", fixed = TRUE)
    expect_true(any(grepl("sequences were replaced", preview$lines, fixed = TRUE)))
    expect_false(preview$truncated)
    expect_false(anyNA(preview$lines))
    expect_identical(readBin(path, "raw", n = length(bytes) + 1L), bytes)
  }))
  literal <- .file_preview(file_preview_fixture(charToRaw(enc2utf8("valid \ufffd character"))))
  expect_identical(literal$lines, "valid \ufffd character")
})

test_that("empty files and previews stay bounded in bytes and lines", {
  invisible(lapply(c("UTF-8", "UTF-16LE", "UTF-32BE"), function(encoding) {
    preview <- .file_preview(file_preview_fixture(file_preview_unicode("", encoding)))
    expect_identical(preview$lines, "")
    expect_false(preview$truncated)
  }))
  expect_identical(.file_preview(file_preview_fixture(raw()))$lines, "")
  path <- file_preview_fixture(c(charToRaw(strrep("a", 1000L)), as.raw(0L)))
  observed <- numeric()
  scope <- new.env(parent = environment(.file_preview))
  scope$readBin <- function(con, what, n, ...) {
    observed <<- c(observed, n)
    base::readBin(con, what, n, ...)
  }
  preview <- .file_preview
  environment(preview) <- scope
  value <- preview(path, bytes = 16L)
  expect_identical(observed, 17)
  expect_identical(value$lines[[1L]], strrep("a", 16L))
  expect_true(value$truncated)
  lines <- .file_preview(file_preview_fixture(charToRaw("one\r\ntwo\r\nthree")), lines = 2L)
  expect_identical(lines$lines[1:2], c("one", "two"))
  expect_true(lines$truncated)
})
