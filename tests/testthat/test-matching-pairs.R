matching_config <- function(...) {
  cfg <- do.call(repl_config, utils::modifyList(list(theme = "midnight", settings_file = NULL,
    color = TRUE, color_depth = 16777216L), list(...), keep.null = TRUE))
  cfg$highlight_cache <- new.env(parent = emptyenv())
  cfg
}

matching_text <- function(source, focus) {
  chars <- .editor_graphemes(source)
  vapply(seq_len(nrow(focus)), function(i)
    paste0(chars[seq.int(focus[i, 1L], focus[i, 2L])], collapse = ""), character(1))
}

matching_marked <- function(text, theme) {
  parts <- .screen_tokens(text)
  active <- FALSE
  marked <- character(length(parts$tokens))
  marker <- paste0("\033[", theme$colors[["pair_match"]], "m")
  for (i in seq_along(parts$tokens)) {
    token <- parts$tokens[[i]]
    if (identical(token, marker)) active <- TRUE
    else if (parts$control[[i]] && startsWith(token, "\033[")) active <- FALSE
    else if (active && !parts$control[[i]]) marked[[i]] <- token
  }
  paste0(marked, collapse = "")
}

test_that("pair indexing understands nesting, escaped quotes and lexical exclusions", {
  code <- 'f({x[[1]]}, "a\\\"b", \'c\\\'d\', `sp ace`) # [ ) "'
  found <- .r_matching_pairs(code)
  expect_equal(nrow(found$pairs), 7L)
  openers <- substring(code, found$pairs[, 1L], found$pairs[, 2L])
  closers <- substring(code, found$pairs[, 3L], found$pairs[, 4L])
  expect_setequal(paste0(openers, closers), c("()", "{}", "[]", '""', "''", "``"))
  comment <- regexpr("#", code, fixed = TRUE)[[1L]]
  expect_true(all(found$lookup[seq.int(comment, nchar(code))] == 0L))
  expect_equal(nrow(.r_matching_pairs('f("unfinished \\" literal ( [')$pairs), 0L)
  expect_equal(nrow(.r_matching_pairs("# () [] {}\nx %quoted(ignored)% y")$pairs), 0L)
  malformed <- .r_matching_pairs("([)]")
  expect_equal(unname(malformed$pairs), matrix(c(2L, 2L, 4L, 4L), nrow = 1L))
  expect_equal(dim(.r_matching_pairs("")$pairs), c(0L, 4L))
  invisible(lapply(1:4, function(slashes) {
    code <- paste0('"first"; "trailing ', strrep("\\", slashes), '"')
    expect_equal(nrow(.r_matching_pairs(code)$pairs), if (slashes %% 2L) 1L else 2L)
  }))
})

test_that("raw-string compound delimiters pair as complete spans", {
  cases <- list(
    c('r"(literal [ ( \' ")"', 'r"(', ')"'),
    c('R"--[literal ]" \" [) --\nnext line]--"', 'R"--[', ']--"'),
    c('r"---{ \\ path \' ] [ ) }--" text }---"', 'r"---{', '}---"'))
  invisible(lapply(cases, function(case) {
    code <- case[[1L]]
    found <- .r_matching_pairs(code)
    expect_equal(nrow(found$pairs), 1L)
    expect_identical(substring(code, found$pairs[, 1L], found$pairs[, 2L]), case[[2L]])
    expect_identical(substring(code, found$pairs[, 3L], found$pairs[, 4L]), case[[3L]])
    expect_equal(sum(found$lookup > 0L), nchar(case[[2L]]) + nchar(case[[3L]]))
    state <- .editor_state(code)
    cfg <- matching_config()
    positions <- which(found$lookup > 0L)
    invisible(lapply(positions, function(position) {
      state$cursor <- position - 1L
      expect_identical(matching_text(code, .editor_pair_focus(state, code, cfg)), case[2:3])
    }))
  }))
  expect_equal(nrow(.r_matching_pairs('r"---[unclosed ) \' ]--"')$pairs), 0L)
})

test_that("cursor focus follows grapheme boundaries and prefers the endpoint under the cursor", {
  code <- '\u03b1("e\u0301\U0001f469\u200d\U0001f4bb", x[[2]])'
  state <- .editor_state(code)
  cfg <- matching_config()
  state$cursor <- 1L
  expect_identical(matching_text(code, .editor_pair_focus(state, code, cfg)), c("(", ")"))
  state$cursor <- 2L
  expect_identical(matching_text(code, .editor_pair_focus(state, code, cfg)), c('"', '"'))
  state$cursor <- 4L
  expect_equal(nrow(.editor_pair_focus(state, code, cfg)), 0L)
  state$cursor <- length(state$chars)
  expect_identical(matching_text(code, .editor_pair_focus(state, code, cfg)), c("(", ")"))
  state <- .editor_state("()[]")
  state$cursor <- 2L
  expect_identical(matching_text("()[]", .editor_pair_focus(state, "()[]", cfg)), c("[", "]"))
  code <- '"\u0301x"'
  state <- .editor_state(code)
  state$cursor <- 0L
  expect_identical(matching_text(code, .editor_pair_focus(state, code, cfg)), c('"\u0301', '"'))
  state$cursor <- 1L
  expect_identical(matching_text(code, .editor_pair_focus(state, code, cfg)), c('"\u0301', '"'))
})

test_that("cursor movement and theme changes reuse one lexical index", {
  code <- paste(rep('f("value", x[[1]])', 1800L), collapse = "\n")
  state <- .editor_state(code)
  cfg <- matching_config()
  original <- .r_matching_pairs
  scans <- 0L
  local_mocked_bindings(.r_matching_pairs = function(text) {
    scans <<- scans + 1L
    original(text)
  })
  state$cursor <- 1L
  expect_equal(nrow(.editor_pair_focus(state, code, cfg)), 2L)
  invisible(lapply(c(2L, 10L, length(state$chars)), function(cursor) {
    state$cursor <- cursor
    .editor_pair_focus(state, code, cfg)
  }))
  cfg$theme <- repl_theme("white")
  invisible(.editor_pair_focus(state, code, cfg))
  expect_identical(scans, 1L)
  state <- .editor_state(paste0(code, " "))
  invisible(.editor_pair_focus(state, paste0(code, " "), cfg))
  expect_identical(scans, 2L)
  cfg$match_pairs <- FALSE
  expect_equal(nrow(.editor_pair_focus(state, paste0(code, " "), cfg)), 0L)
  expect_identical(scans, 2L)
  cfg$match_pairs <- TRUE
  invisible(lapply(seq_len(5L), function(i) {
    next_code <- paste0("f(", i, ")")
    .editor_pair_focus(.editor_state(next_code), next_code, cfg)
  }))
  expect_identical(ls(cfg$highlight_cache), "matching_pairs")
  expect_identical(cfg$highlight_cache$matching_pairs$key, "f(5)")
  expect_lt(as.numeric(object.size(cfg$highlight_cache$matching_pairs)), 5000)
})

test_that("focused endpoints remain visible in every color theme and depth", {
  invisible(lapply(setdiff(repl_themes(), "mono"), function(theme) {
    invisible(lapply(c(16L, 256L, 16777216L), function(depth) {
      cfg <- repl_config(theme = theme, settings_file = NULL, color = TRUE, color_depth = depth,
        rainbow = FALSE, autopair = FALSE)
      state <- .editor_state("f(x)")
      state$cursor <- 1L
      geometry <- .editor_wrap_geometry(state, 60L, "r> ", "+  ", cfg)
      frame <- .editor_wrapped_frame(state, geometry, 60L, 6L, cfg)
      expect_identical(matching_marked(frame$lines[[1L]], cfg$theme), "()")
      expect_identical(as.character(cli::ansi_strip(frame$lines[[1L]])), "r> f(x)")
      expect_true(cfg$theme$styles$pair_match$bold)
      expect_true(cfg$theme$styles$pair_match$underline)
      expect_false(identical(cfg$theme$styles$pair_match$fg, cfg$theme$styles$pair_match$bg))
    }))
  }))
  state <- .editor_state("f(x)")
  cfg <- matching_config(color = FALSE)
  expect_equal(nrow(.editor_pair_focus(state, "f(x)", cfg)), 0L)
  cfg <- matching_config(theme = "mono")
  expect_equal(nrow(.editor_pair_focus(state, "f(x)", cfg)), 0L)
})

test_that("focused rows reuse unchanged styles and refresh when focus colors change", {
  cases <- lapply(list(
    list(from = "midnight", to = "white", changed = TRUE),
    list(from = "paper", to = "black", changed = TRUE),
    list(from = "paper", to = "white", changed = FALSE)), function(case) {
      case$config <- matching_config(theme = case$from)
      case$text <- highlight_r("f(x)", case$config$theme)
      case
    })
  focus <- matrix(c(2L, 2L, 4L, 4L), ncol = 2L, byrow = TRUE)
  original <- .screen_tokens
  scans <- 0L
  local_mocked_bindings(.screen_tokens = function(text) {
    scans <<- scans + 1L
    original(text)
  })
  invisible(lapply(cases, function(case) {
    cfg <- case$config
    before <- scans
    first <- .editor_pair_row(case$text, focus, 1L, 4L, 0L, cfg, positions = 0:4)
    expect_identical(.editor_pair_row(case$text, focus, 1L, 4L, 0L, cfg,
      positions = 0:4), first)
    expect_identical(scans, before + 1L)
    cfg$theme <- repl_theme(case$to)
    changed <- .editor_pair_row(case$text, focus, 1L, 4L, 0L, cfg, positions = 0:4)
    expected <- before + if (case$changed) 2L else 1L
    expect_identical(scans, expected, info = paste(case$from, "to", case$to))
    expect_identical(identical(first, changed), !case$changed,
      info = paste(case$from, "to", case$to))
    expect_identical(.editor_pair_row(case$text, focus, 1L, 4L, 0L, cfg,
      positions = 0:4), changed)
    expect_identical(scans, expected)
    expect_identical(as.character(cli::ansi_strip(changed)), "f(x)")
  }))
})

test_that("inline, wrapped and function-editor rows share accurate focused spans", {
  code <- 'f(\t\u754c, r"--[first\n  second]--")'
  cfg <- matching_config()
  state <- .editor_state(code)
  state$cursor <- 6L
  expect_identical(state$chars[[state$cursor + 1L]], "r")
  frame <- .editor_frame(state, 80L, 12L, "\u754c> ", "+  ", character(), cfg)
  marked <- vapply(frame$lines, matching_marked, character(1), theme = cfg$theme)
  expect_identical(paste0(marked, collapse = ""), 'r"--[]--"')
  geometry <- .editor_wrap_geometry(state, 15L, "\u754c> ", "+  ", cfg)
  wrapped <- .editor_wrapped_frame(state, geometry, 15L, 12L, cfg)
  marked <- vapply(wrapped$lines, matching_marked, character(1), theme = cfg$theme)
  expect_identical(paste0(marked, collapse = ""), 'r"--[]--"')
  function_state <- .function_editor_state(code)
  function_state$editor <- state
  edited <- .function_editor_frame(function_state, "example", cfg, c(width = 50L, height = 18L))
  marked <- vapply(edited$lines, matching_marked, character(1), theme = cfg$theme)
  expect_identical(paste0(marked, collapse = ""), 'r"--[]--"')
  cfg$match_pairs <- FALSE
  unmarked <- .editor_wrapped_frame(state, geometry, 15L, 12L, cfg)
  expect_identical(as.character(cli::ansi_strip(wrapped$lines)), as.character(cli::ansi_strip(unmarked$lines)))
  expect_false(any(vapply(unmarked$lines, function(line)
    nzchar(matching_marked(line, cfg$theme)), logical(1))))
})

test_that("raw wrappers crossing display rows and horizontally clipped pairs preserve geometry", {
  dashes <- strrep("-", 18L)
  code <- paste0('r"', dashes, '(x)', dashes, '"')
  cfg <- matching_config(theme = "white")
  state <- .editor_state(code)
  state$cursor <- 0L
  geometry <- .editor_wrap_geometry(state, 12L, "r> ", "+  ", cfg)
  wrapped <- .editor_wrapped_frame(state, geometry, 12L, 12L, cfg)
  marked <- vapply(wrapped$lines, matching_marked, character(1), theme = cfg$theme)
  expect_identical(paste0(marked, collapse = ""), paste0('r"', dashes, '()', dashes, '"'))
  code <- paste0("f(", strrep("x", 200L), ")")
  state <- .editor_state(code)
  state$cursor <- length(state$chars) - 1L
  inline <- .editor_frame(state, 20L, 8L, "r> ", "+  ", character(), cfg)
  expect_identical(matching_marked(inline$lines[[1L]], cfg$theme), ")")
  cfg$match_pairs <- FALSE
  plain <- .editor_frame(state, 20L, 8L, "r> ", "+  ", character(), cfg)
  expect_identical(as.character(cli::ansi_strip(inline$lines)), as.character(cli::ansi_strip(plain$lines)))
  expect_identical(inline$cursor_column, plain$cursor_column)
  state$cursor <- 8L
  cfg$match_pairs <- TRUE
  away <- .editor_frame(state, 20L, 8L, "r> ", "+  ", character(), cfg)
  cfg$match_pairs <- FALSE
  off <- .editor_frame(state, 20L, 8L, "r> ", "+  ", character(), cfg)
  expect_identical(away$lines, off$lines)
})

test_that("matching preference persists independently and its selectable command is registered", {
  path <- tempfile(fileext = ".dcf")
  on.exit(unlink(path), add = TRUE)
  expect_true(repl_config(settings_file = path)$match_pairs)
  invisible(repl_settings(match_pairs = FALSE, rainbow = TRUE, autopair = FALSE, file = path))
  cfg <- repl_config(settings_file = path)
  expect_false(cfg$match_pairs)
  expect_true(cfg$rainbow)
  expect_false(cfg$autopair)
  expect_true(repl_config(settings_file = path, match_pairs = TRUE)$match_pairs)
  expect_error(repl_config(match_pairs = NA, settings_file = NULL), "match_pairs")
  expect_error(repl_settings(match_pairs = "off", file = path), "match_pairs")
  commands <- .command_catalog()
  expect_true("match" %in% commands$id)
  expect_identical(.command_parse(":ma toggle")$command, "match")
  expect_setequal(.complete_command(":match o", 8L, baseenv(), 100L)$candidates, c("on", "off"))
  state <- new.env(parent = emptyenv())
  state$config <- cfg
  local_mocked_bindings(.emit = function(...) invisible(NULL),
    .toggle_choice = function(name, label, value, config, toggle) {
      expect_identical(name, "match")
      expect_false(value)
      expect_true(toggle)
      "on"
    })
  .editing_preference_command("match_pairs", character(), state)
  expect_true(state$config$match_pairs)
  expect_true(repl_config(settings_file = path)$match_pairs)
})

test_that("themes saved before cursor matching acquire the new role without losing overrides", {
  modern <- repl_theme("white", list(prompt = "1;35", string = "32"))
  older <- modern
  older$colors <- older$colors[names(older$colors) != "pair_match"]
  older$styles$pair_match <- NULL
  legacy <- list(name = "my-white-theme", colors = older$colors, mode = "light")
  invisible(lapply(list(older, legacy), function(theme) {
    cfg <- matching_config(theme = theme)
    expect_true(nzchar(cfg$theme$colors[["pair_match"]]))
    expect_identical(cfg$theme$styles$pair_match$fg, "#FFFFFF")
    expect_identical(cfg$theme$colors[["prompt"]], "1;35")
    expect_identical(cfg$theme$colors[["string"]], "32")
    state <- .editor_state("f(x)")
    expect_equal(nrow(.editor_pair_focus(state, "f(x)", cfg)), 2L)
  }))
})
