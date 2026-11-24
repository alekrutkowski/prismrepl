pair_event <- function(text = "", key = "", type = "text", modifiers = character())
  list(type = type, text = text, key = key, modifiers = modifiers, truncated = FALSE)

pair_config <- function(enabled = TRUE) {
  list(pipe = "|>", autopair = enabled, max_paste = 100000L, max_lines = 2000L,
       theme = repl_theme(), color = TRUE, rainbow = TRUE)
}

pair_type <- function(text, state = .editor_state(), config = pair_config())
  Reduce(function(state, char) .editor_step(state, pair_event(char), character(),
    baseenv(), config), .editor_graphemes(text), init = state)

pair_text <- function(state) paste0(state$chars, collapse = "")

pair_key <- function(state, key, config = pair_config(), modifiers = character())
  .editor_step(state, pair_event(key = key, type = "key", modifiers = modifiers),
    character(), baseenv(), config)

test_that("typed matching pairs are inserted, overtyped and removed coherently", {
  pairs <- c("(" = ")", "[" = "]", "{" = "}", '"' = '"', "'" = "'", "`" = "`")
  invisible(lapply(names(pairs), function(open) {
    state <- pair_type(open)
    expect_identical(pair_text(state), paste0(open, pairs[[open]]))
    expect_equal(state$cursor, 1L)
    typed <- pair_type(pairs[[open]], state)
    expect_identical(pair_text(typed), pair_text(state))
    expect_equal(typed$cursor, 2L)
    removed <- pair_key(state, "backspace")
    expect_identical(pair_text(removed), "")
    expect_equal(removed$cursor, 0L)
    expect_null(removed$autopairs)
  }))
  nested <- pair_type('f(c("a", "b"), x[[2]])')
  expect_identical(pair_text(nested), 'f(c("a", "b"), x[[2]])')
  expect_equal(nested$cursor, length(nested$chars))
  square <- pair_type("x[[")
  expect_identical(pair_text(square), "x[[]]")
  square <- pair_key(square, "backspace")
  expect_identical(pair_text(square), "x[]")
  square <- pair_key(square, "backspace")
  expect_identical(pair_text(square), "x")
})

test_that("quotes, comments, backticks, raw strings and commands retain literal contents", {
  cases <- c(
    'message("a ( ] { literal", "escaped \\\" quote")',
    "'apostrophe \\' and other \" quote'",
    '`an unusual ( [ name` <- 1',
    'r"(C:\\a folder\\ (content) and "quotes")"',
    'R"--[literal ) { \\" text]--"',
    '# don\'t add any ([{ or "pairs" here',
    'x %custom(operator)% y',
    ':source C:\\my folder\\script (draft).R',
    ':cd /home/user/it\'s a folder',
    ':source `C:\\my folder\\script (draft).R`')
  invisible(lapply(cases, function(code) expect_identical(pair_text(pair_type(code)), code)))
  comment <- pair_type('# don\'t (')
  expect_null(comment$autopairs)
  fresh_line <- pair_type("\nf(", comment)
  expect_true(endsWith(pair_text(fresh_line), "\nf()"))
  percent <- pair_type("x %custom(")
  expect_identical(pair_text(percent), "x %custom(")
})

test_that("pair insertion is independent of syntax coloring and does not rewrite other inputs", {
  disabled <- pair_config(FALSE)
  expect_identical(pair_text(pair_type("(", config = disabled)), "(")
  colorless <- pair_config()
  colorless$rainbow <- FALSE
  colorless$color <- FALSE
  expect_identical(pair_text(pair_type("(", config = colorless)), "()")
  paste <- .editor_step(.editor_state(), pair_event('f("x', type = "paste"),
    character(), baseenv(), pair_config())
  expect_identical(pair_text(paste), 'f("x')
  expect_null(paste$autopairs)
  batched <- .editor_step(.editor_state(), pair_event('f("x'),
    character(), baseenv(), pair_config())
  expect_identical(pair_text(batched), 'f("x')
  expect_null(batched$autopairs)
  recalled <- .editor_history(.editor_state(), -1L, 'f("x', pair_config())
  expect_identical(pair_text(recalled), 'f("x')
  expect_null(recalled$autopairs)
  manual <- .editor_state("()")
  manual$cursor <- 1L
  expect_identical(pair_text(pair_key(manual, "backspace")), ")")
})

test_that("generated markers survive Unicode edits and disappear after replaced endpoints", {
  state <- pair_type('f("\u00e9\U0001f469\u200d\U0001f4bb')
  expect_identical(pair_text(state), 'f("\u00e9\U0001f469\u200d\U0001f4bb")')
  state <- pair_type('")', state)
  expect_equal(state$cursor, length(state$chars))
  expect_identical(pair_text(state), 'f("\u00e9\U0001f469\u200d\U0001f4bb")')
  state <- pair_type('"e\u0301')
  state <- pair_key(state, "backspace")
  expect_identical(pair_text(state), '""')
  expect_equal(state$cursor, 1L)
  state <- pair_key(state, "backspace")
  expect_identical(pair_text(state), "")
  replaced <- .editor_replace(pair_type("["), 0L, 1L, "x")
  expect_null(replaced$autopairs)
  expect_identical(pair_text(pair_type("]", replaced)), "x]]")
  combined <- pair_type("\u0301", pair_type("("))
  expect_false(is.null(combined$autopairs))
  expect_identical(combined$chars, .editor_graphemes(pair_text(combined)))
  expect_identical(pair_text(pair_type(")", combined)), "(\u0301)")
  combining_string <- pair_type('"\u0301"')
  expect_identical(pair_text(combining_string), '"\u0301"')
  expect_equal(combining_string$cursor, length(combining_string$chars))
})

test_that("pairing honors limits, cursor context and complete-code Enter semantics", {
  tiny <- pair_config()
  tiny$max_paste <- 1L
  rejected <- pair_type("(", config = tiny)
  expect_identical(pair_text(rejected), "")
  expect_true(rejected$limit_rejected)
  expect_null(rejected$autopairs)
  expect_match(rejected$overlay, "Input limit")
  middle <- .editor_state("value")
  middle$cursor <- 0L
  expect_identical(pair_text(pair_type("(", middle)), "(value")
  identifier <- .editor_state("word")
  expect_identical(pair_text(pair_type("'", identifier)), "word'")
  complete <- pair_type("f(1")
  expect_identical(pair_text(complete), "f(1)")
  expect_identical(pair_key(complete, "enter")$action, "submit")
  newline <- pair_key(complete, "j", modifiers = "ctrl")
  expect_identical(newline$action, "edit")
  expect_identical(pair_text(newline), "f(1\n)")
  incomplete <- pair_key(.editor_state("f("), "enter")
  expect_identical(incomplete$action, "edit")
  expect_match(pair_text(incomplete), "\n", fixed = TRUE)
})

test_that("rainbow levels follow nested types but ignore literal syntax", {
  tokens <- c("(", "[", "[", '"(ignored)"', "]", "]", "{", "`[name]`", "}", ")")
  expect_identical(.r_rainbow_levels(tokens), c(1L, 2L, 3L, 4L, 3L, 2L, 2L, 3L, 2L, 1L))
  expect_identical(.r_rainbow_levels(c(")", "(", "]", ")", "# [", 'r"({[)]})"')),
    c(0L, 1L, 0L, 1L, 0L, 1L))
  expect_identical(.r_rainbow_levels(tokens, 3L), c(1L, 2L, 3L, rep(0L, 7L)))
  expect_identical(.r_rainbow_levels(character()), integer())
})

test_that("span classification matches lexical nesting for Unicode and raw quotes", {
  codes <- c(
    '\u03b1(\u03b2[["\U0001f469\u200d\U0001f4bb"]], `\u754c`) # ([{',
    'f(r"--{\u754c [literal] "quote"}--", R"[\u03b2]")',
    'f("\u00e9\\\"\u03b2", \'e\u0301\')',
    '\u03b1(\n  R"---[\u754c\n still raw )',
    '( [ ] ) } \u03b1 %bracket(ignored)% \u03b2')
  invisible(lapply(codes, function(code) {
    hits <- gregexpr(.r_lex_pattern(), code, perl = TRUE)[[1L]]
    ends <- hits + attr(hits, "match.length") - 1L
    spans <- .r_lex_spans(code)
    expect_equal(unname(spans), unname(cbind(as.integer(hits), ends)))
    tokens <- substring(code, hits, ends)
    expect_identical(.r_rainbow_spans(utf8ToInt(code), hits, ends), .r_rainbow_levels(tokens))
    expect_identical(as.character(cli::ansi_strip(highlight_r(code))), code)
  }))
  expect_equal(dim(.r_lex_spans("")), c(0L, 2L))
})

test_that("late Unicode windows extract only visible token text and skip unused nesting", {
  line <- '\u03b1 <- f("\u03b2", x[[1]])'
  code <- paste(rep(line, 2000L), collapse = "\n")
  source_start <- 1994L * (nchar(line, type = "chars") + 1L) + 1L
  extracted <- list()
  original <- .r_highlight_span
  local_mocked_bindings(.r_highlight_span = function(points, first, last) {
    extracted[[length(extracted) + 1L]] <<- c(first = first, last = last)
    original(points, first, last)
  })
  colored <- .highlight_r_window(code, 1995L, 2000L, rainbow = TRUE)
  expect_identical(as.character(cli::ansi_strip(colored)), rep(line, 6L))
  expect_lt(length(extracted), 512L)
  expect_true(all(vapply(extracted, function(span) span[["first"]] >= source_start, logical(1L))))
  expect_true(all(vapply(extracted, function(span)
    span[["last"]] - span[["first"]] + 1L <= nchar(line), logical(1L))))
  extracted <- list()
  local_mocked_bindings(.r_rainbow_spans = function(...) stop("unused rainbow scan"))
  uncolored <- .highlight_r_window(code, 1995L, 2000L, rainbow = FALSE)
  expect_identical(as.character(cli::ansi_strip(uncolored)), rep(line, 6L))
  expect_lt(length(extracted), 512L)
  expect_true(all(vapply(extracted, function(span) span[["first"]] >= source_start, logical(1L))))
})

test_that("rainbow delimiters preserve string contents, raw wrappers and source", {
  theme <- repl_theme()
  code <- 'f([[("text")]], `x`, r"--{[literal]}--")'
  rainbow <- highlight_r(code, theme, rainbow = TRUE)
  plain <- highlight_r(code, theme, rainbow = FALSE)
  expect_identical(as.character(cli::ansi_strip(rainbow)), code)
  expect_identical(as.character(cli::ansi_strip(plain)), code)
  expect_false(identical(rainbow, plain))
  expect_identical(highlight_r(code, theme, color = FALSE, rainbow = TRUE), code)
  expect_identical(highlight_r(code, repl_theme("mono"), rainbow = TRUE), code)
  string <- highlight_r('"hello"', theme)
  pair <- paste0("\033[", theme$colors[[.rainbow_role(1L, theme)]], "m")
  literal <- paste0("\033[", theme$colors[["string"]], "m")
  expect_match(string, paste0(pair, '"'), fixed = TRUE)
  expect_match(string, paste0(literal, "hello"), fixed = TRUE)
  expect_equal(stringi::stri_count_fixed(string, pair), 2L)
  raw <- highlight_r('r"--{[literal]}--"', theme)
  expect_match(raw, paste0(pair, 'r"--{'), fixed = TRUE)
  expect_match(raw, paste0(pair, '}--"'), fixed = TRUE)
  expect_match(raw, paste0(literal, '[literal]'), fixed = TRUE)
  escaped <- .r_quote_bounds('"escaped \\"')
  expect_false(escaped$closed)
  expect_true(.r_quote_bounds('"escaped \\\\"')$closed)
  expect_error(highlight_r("x", rainbow = NA), "rainbow")
})

test_that("multiline windows retain rainbow nesting and caches follow the toggle", {
  code <- 'f(\n  list("first\nlast", x[[1]])\n)'
  full <- stringi::stri_split_fixed(highlight_r(code), "\n", omit_empty = FALSE)[[1L]]
  window <- .highlight_r_window(code, 2L, 4L)
  expect_identical(window, full[2:4])
  cfg <- pair_config()
  cfg$highlight_cache <- new.env(parent = emptyenv())
  state <- .editor_state('f("x")')
  first <- .editor_frame(state, 40L, 10L, "r> ", "+  ", character(), cfg)
  cfg$rainbow <- FALSE
  second <- .editor_frame(state, 40L, 10L, "r> ", "+  ", character(), cfg)
  expect_false(identical(first$lines, second$lines))
  expect_identical(as.character(cli::ansi_strip(first$lines)),
    as.character(cli::ansi_strip(second$lines)))
  cfg$rainbow <- TRUE
  geometry <- .editor_wrap_geometry(state, 40L, "r> ", "+  ", cfg)
  first <- .editor_wrapped_frame(state, geometry, 40L, 10L, cfg)
  cfg$rainbow <- FALSE
  second <- .editor_wrapped_frame(state, geometry, 40L, 10L, cfg)
  expect_false(identical(first$lines, second$lines))
  expect_identical(as.character(cli::ansi_strip(first$lines)),
    as.character(cli::ansi_strip(second$lines)))
})

test_that("function editing shares automatic pair insertion and explicit newline behavior", {
  cfg <- pair_config()
  state <- .function_editor_state("function(x) ")
  state$editor$cursor <- length(state$editor$chars)
  state <- .function_editor_apply(state, pair_event("{"), cfg, baseenv())
  expect_true(endsWith(pair_text(state$editor), "{}"))
  state <- .function_editor_apply(state, pair_event(key = "enter", type = "key"), cfg, baseenv())
  expect_true(grepl("\\{\\n +\\}", pair_text(state$editor)))
  expect_false(state$save)
  expect_false(state$quit)
})
