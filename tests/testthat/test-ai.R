test_that("AI provider registry distinguishes prompt links from paste destinations", {
  providers <- repl_ai_providers()
  expect_identical(names(providers), c("provider", "label", "url", "prefill", "notes"))
  expect_gte(nrow(providers), 10L)
  expect_false(anyDuplicated(providers$provider) > 0L)
  expect_true(all(c("mistral", "chatgpt", "claude", "gemini", "deepseek") %in% providers$provider))
  expect_identical(providers$prefill, grepl("{prompt}", providers$url, fixed = TRUE))
  expect_true(all(startsWith(providers$url, "https://")))
  expect_false(any(grepl("[?&]utm_", providers$url)))
  expect_identical(.ai_target()$provider, "mistral")
  expect_error(.ai_target("missing"), "ai_provider")
  expect_error(.ai_target(c("mistral", "claude")), "ai_provider")
})

test_that("prompt links use each provider's own endpoint and encode source literally", {
  providers <- repl_ai_providers()
  selected <- providers[providers$prefill, ]
  error <- "object '\u03b1 & x+1 %2F' not found"
  source <- "\u03b1 <- '\u6f22\u5b57'"
  lapply(seq_len(nrow(selected)), function(i) {
    row <- selected[i, ]
    url <- ai_link(error, source, "f(x)", provider = row$provider)
    prefix <- sub("{prompt}", "", row$url, fixed = TRUE)
    expect_true(startsWith(url, prefix), info = row$provider)
    expect_match(utils::URLdecode(url), error, fixed = TRUE)
    expect_match(utils::URLdecode(url), source, fixed = TRUE)
    expect_match(utils::URLdecode(url), "Call trace:\nf(x)", fixed = TRUE)
    expect_false(grepl("[ &]", url))
    expect_match(url, "%252F", fixed = TRUE)
  })
  expect_identical(ai_link(error, source), mistral_link(error, source))
})

test_that("paste destinations never embed an error or source in their URL", {
  providers <- repl_ai_providers()
  selected <- providers[!providers$prefill, ]
  lapply(seq_len(nrow(selected)), function(i) {
    expect_identical(ai_link("private-error", "private-source", provider = selected$provider[[i]]),
      selected$url[[i]])
  })
  expect_error(ai_link(NA_character_, provider = "gemini"), "error must")
})

test_that("prompts are local bounded UTF-8 text and honor minimal context", {
  full <- ai_prompt(simpleError("problem"), c("x <- 1", "stop(x)"), c("a()", "b()"))
  expect_match(full, "Error: problem", fixed = TRUE)
  expect_match(full, "R source:\nx <- 1\nstop(x)", fixed = TRUE)
  expect_match(full, "Call trace:\na()\nb()", fixed = TRUE)
  minimal <- ai_prompt("problem", "private-source", "private-trace", include_context = FALSE)
  expect_match(minimal, "Error: problem", fixed = TRUE)
  expect_false(grepl("private|R source|Call trace", minimal))
  large <- ai_prompt(strrep("\U0001f9ea", 1000L), max_chars = 100L)
  expect_lte(nchar(large, type = "chars"), 100L)
  expect_true(utf8::utf8_valid(large))
  expect_true(endsWith(large, "[context truncated]"))
  controls <- ai_prompt("bad\033]8;;https://example.org\007\r")
  expect_false(grepl("\033|\007|\r", controls))
  expect_error(ai_prompt("x", max_chars = 99L), "max_chars")
  expect_error(ai_prompt("x", code = NA_character_), "code must")
  expect_error(ai_prompt("x", include_context = NA), "include_context")
})

test_that("AI URLs stay within the byte limit for long multibyte prompts", {
  error <- paste(rep("\U0001f9ea\u6f22\u03b1", 20000L), collapse = "")
  lapply(c("mistral", "chatgpt", "claude", "perplexity"), function(provider) {
    url <- ai_link(error, max_chars = 20000L, provider = provider)
    expect_lte(nchar(url, type = "bytes"), 8000L)
    expect_match(utils::URLdecode(url), "[context truncated]", fixed = TRUE)
    expect_true(utf8::utf8_valid(utils::URLdecode(url)))
  })
})

test_that("custom templates retain suffixes and do not interpret prompt substitutions", {
  template <- "https://chat.example.org/ask?text={prompt}&mode=code#composer"
  error <- "a+b &c=%2F {prompt} \\1"
  out <- ai_link(error, provider = "custom", url = template)
  expect_true(startsWith(out, "https://chat.example.org/ask?text="))
  expect_true(endsWith(out, "&mode=code#composer"))
  expect_match(utils::URLdecode(out), error, fixed = TRUE)
  expect_identical(.ai_target("custom", template)$label, "Custom AI")
  expect_identical(.ai_target("mistral", template)$url,
    repl_ai_providers()$url[[1L]])
  expect_identical(ai_link("error", provider = "mistral", url = template), mistral_link("error"))
  long_template <- paste0("https://example.org/", strrep("x", 1900L), "?q={prompt}")
  expect_lte(nchar(ai_link(strrep("\U0001f9ea", 5000L), max_chars = 5000L,
    provider = "custom", url = long_template), type = "bytes"), 8000L)
})

test_that("invalid custom AI URLs fail before they reach the terminal", {
  invalid <- c("http://example.org/?q={prompt}", "javascript:{prompt}",
    "https://user:password@example.org/?q={prompt}", "https://{prompt}.example.org/",
    "https://example.org/?q=no-marker", "https://example.org/?q={prompt}&again={prompt}",
    "https://example.org/?q={prompt}\n", "https://example.org/\\x?q={prompt}",
    "https://example.org/?q={prompt}&model={model}",
    paste0("https://example.org/?q={prompt}", "&", "utm_", "source=tracking"),
    "https://example.org/?q={prompt}&fbclid=tracking",
    paste0("https://example.org/", strrep("x", 2048L), "?q={prompt}"))
  lapply(invalid, function(url) expect_error(.validate_ai_url(url), "ai_url"))
  expect_error(.ai_target("custom"), "requires")
  expect_error(.ai_target("mistral", "broken"), "ai_url")
})

test_that("condition links follow the configured provider and retain semantic metadata", {
  captured <- list()
  cfg <- repl_config(color = FALSE, settings_file = NULL, hyperlinks = TRUE, width = 70L)
  cfg$output_hook <- function(lines, role) {
    captured[[length(captured) + 1L]] <<- list(lines = lines, role = role)
    invisible(NULL)
  }
  cfg$ai_provider <- "claude"
  cfg$ai_url <- NULL
  out <- .show_error(list(error = simpleError("bad input"), code = "f(x)", trace = "f(x)"), cfg)
  expect_true(startsWith(out, "https://claude.ai/new?q="))
  expect_length(captured, 2L)
  link <- attr(captured[[2L]]$lines, "prism_link")
  expect_identical(link$url, out)
  expect_match(link$label, "Explain with Claude", fixed = TRUE)
  expect_identical(captured[[2L]]$role, "link")

  captured <- list()
  cfg$ai_provider <- "gemini"
  out <- .show_error(list(error = simpleError("bad input")), cfg)
  expect_identical(out, "https://gemini.google.com/app")
  expect_length(captured, 3L)
  expect_match(attr(captured[[2L]]$lines, "prism_link")$label, "Open Gemini", fixed = TRUE)
  expect_match(captured[[3L]]$lines, ":explain copy", fixed = TRUE)
  expect_false(grepl("bad input", out, fixed = TRUE))
})

test_that("plain condition links explain provider handoff without OSC controls", {
  cfg <- repl_config(color = FALSE, settings_file = NULL, hyperlinks = FALSE, width = 70L)
  cfg$ai_provider <- "chatgpt"
  text <- utils::capture.output(.show_error(list(error = simpleError("bad")), cfg))
  expect_true(any(grepl("Explain with ChatGPT (opening shares the link text):", text, fixed = TRUE)))
  expect_false(any(grepl("\033", text, fixed = TRUE)))
  cfg$ai_provider <- "deepseek"
  text <- utils::capture.output(.show_error(list(error = simpleError("bad")), cfg))
  expect_true(any(grepl("Open DeepSeek:", text, fixed = TRUE)))
  expect_true(any(grepl(":explain copy", text, fixed = TRUE)))
  expect_false(any(grepl("opening shares", text, fixed = TRUE)))
})
