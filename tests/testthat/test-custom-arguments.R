test_that("one ordinary formal receives its complete literal remainder", {
  inputs <- c("one  two   three", "name\twith\ttabs", "line one\n\nline two",
    "C:\\Users\\Alek\\My  folder\\file.R", "\\\\server\\share name\\folder\\",
    "don't remove apostrophes", 'keep "inner quotes" here', "NA", "TRUE", "123",
    "x=y", "$HOME", "x;stop('must stay text')", "\u017c\u00f3\u0142\u0107  \u5206\u6790")
  invisible(lapply(inputs, function(input) {
    expect_identical(.custom_command_arguments(input, "x"), input)
    expect_identical(.custom_command_arguments(paste0(" \t", input, " \r\n"), "x"), input)
  }))
  expect_identical(.custom_command_arguments('"one"  "two"', "x"), '"one"  "two"')
  expect_identical(.custom_command_arguments("'one'  two", "x"), "'one'  two")
  expect_identical(.custom_command_arguments('"first"\n\n`second`', "x"), '"first"\n\n`second`')
})

test_that("whole wrappers preserve spaces and literal Windows path separators", {
  quotes <- c('"', "'", "`")
  inputs <- c("one  two", "  keep boundary spaces  ", "\tkeep tabs\t",
    "C:\\new folder\\", "\\\\server\\share name\\", "\\n \\t \\u0041 \\x41",
    "line one\n\nline two", "\u00e9\u00e8  \u6f22\u5b57", "")
  invisible(lapply(quotes, function(quote) invisible(lapply(inputs, function(input) {
    wrapped <- paste0(quote, input, quote)
    expect_identical(.custom_command_arguments(wrapped, "x"), input)
    expect_identical(.custom_command_arguments(paste(wrapped, "next"), c("x", "y")), c(input, "next"))
  }))))
  expect_identical(.custom_command_arguments('"Tom\'s folder" next', c("x", "y")), c("Tom's folder", "next"))
  expect_identical(.custom_command_arguments("'She said \"hello\"' next", c("x", "y")), c('She said "hello"', "next"))
  expect_identical(.custom_command_arguments('`She said "Tom\'s folder"` next', "..."), c('She said "Tom\'s folder"', "next"))
})

test_that("zero multiple and variadic formals retain positional token semantics", {
  signatures <- list(character(), c("x", "y"), "...", c("x", "..."), c("...", "suffix"))
  text <- "first 'second  argument' `third\targument` \"\" NA TRUE 123 x=y $HOME"
  expected <- c("first", "second  argument", "third\targument", "", "NA", "TRUE", "123", "x=y", "$HOME")
  invisible(lapply(signatures, function(parameters) {
    expect_identical(.custom_command_arguments(text, parameters), expected)
    expect_identical(.custom_command_arguments("a\n\n b\t c\r\n\r\nd", parameters), c("a", "b", "c", "d"))
    expect_identical(.custom_command_arguments("", parameters), character())
    expect_identical(.custom_command_arguments(" \t\n\r ", parameters), character())
  }))
  expect_identical(.custom_command_arguments("don't O'Brien", c("x", "y")), c("don't", "O'Brien"))
  expect_identical(.custom_command_arguments('foo"bar baz"', "..."), c('foo"bar', 'baz"'))
  expect_identical(.custom_command_arguments("a;stop('bad') next", "..."), c("a;stop('bad')", "next"))
})

test_that("omission and explicit empty strings remain distinct", {
  expect_identical(.custom_command_arguments("", "x"), character())
  expect_identical(.custom_command_arguments(" \t\n", "x"), character())
  invisible(lapply(c('""', "''", "``"), function(empty) {
    expect_identical(.custom_command_arguments(empty, "x"), "")
    expect_identical(.custom_command_arguments(empty, "..."), "")
    expect_identical(.custom_command_arguments(paste(empty, empty), "..."), c("", ""))
  }))
  expect_identical(.custom_command_arguments("NA", "..."), "NA")
})

test_that("malformed quoting cannot yield partial argument lists", {
  malformed <- c('"', "'", "`", '"unfinished', "'unfinished", "`unfinished",
    'first "unfinished', '"one"two', '"one""two"', "'one'`two`", '"one" "two')
  invisible(lapply(malformed, function(input) {
    expect_error(.custom_command_arguments(input, "..."), "Malformed command arguments", fixed = TRUE)
    if (substr(input, 1L, 1L) %in% c('"', "'", "`"))
      expect_error(.custom_command_arguments(input, "x"), "Malformed command arguments", fixed = TRUE)
  }))
  expect_identical(.custom_command_arguments('literal "unfinished', "x"), 'literal "unfinished')
})

test_that("literal parsing handles the editor's maximum-size input without truncation", {
  text <- strrep("x", 99998L)
  expect_identical(.custom_command_arguments(paste0('"', text, '"'), "..."), text)
  expect_error(.custom_command_arguments(paste0('"', text), "..."), "Malformed command arguments", fixed = TRUE)
  arguments <- rep("data", 20000L)
  expect_identical(.custom_command_arguments(paste(arguments, collapse = " "), "..."), arguments)
})

test_that("custom argument input validation rejects malformed metadata", {
  invisible(lapply(list(NULL, NA_character_, c("one", "two"), 1), function(input)
    expect_error(.custom_command_arguments(input, "x"), "remainder")))
  invisible(lapply(list(NULL, NA_character_, "", 1), function(parameters)
    expect_error(.custom_command_arguments("text", parameters), "parameter names")))
  expect_identical(.custom_command_arguments(c(value = "word"), "x"), "word")
})
