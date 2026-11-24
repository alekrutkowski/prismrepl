#' List Browser Destinations for Error Explanations
#'
#' These destinations use the provider's web interface, not its API. No account,
#' API key, network request, or browser is needed to construct an explanation
#' link. A provider may require sign-in when the user opens the link.
#'
#' A `prefill` value of `TRUE` identifies a browser prompt URL convention. These
#' web interfaces can change independently of this package. Some services may
#' submit the prompt immediately when the link is opened. If a service ignores
#' the prompt, use `:explain copy` and paste it yourself. Destinations with
#' `prefill = FALSE` open their home page and always require this paste step.
#'
#' @return A data frame with `provider` identifiers, display `label`, `url`
#'   template or home page, logical `prefill`, and plain-language `notes`.
#' @export
#' @examples
#' repl_ai_providers()[c("provider", "prefill")]
repl_ai_providers <- function() {
  out <- data.frame(
    provider = c("mistral", "chatgpt", "claude", "perplexity", "gemini", "copilot",
                 "grok", "deepseek", "qwen", "duckai", "poe"),
    label = c("Mistral AI", "ChatGPT", "Claude", "Perplexity", "Gemini", "Copilot",
              "Grok", "DeepSeek", "Qwen", "Duck.ai", "Poe"),
    url = c("https://chat.mistral.ai/chat?q={prompt}",
            "https://chatgpt.com/?q={prompt}",
            "https://claude.ai/new?q={prompt}",
            "https://www.perplexity.ai/search?q={prompt}",
            "https://gemini.google.com/app", "https://copilot.microsoft.com/",
            "https://grok.com/", "https://chat.deepseek.com/", "https://chat.qwen.ai/",
            "https://duck.ai/", "https://poe.com/"),
    prefill = c(rep(TRUE, 4L), rep(FALSE, 7L)),
    notes = c(rep("Prompt URL; use :explain copy if the page does not fill it.", 4L),
              rep("Open home page, then paste the prompt from :explain copy.", 7L)),
    stringsAsFactors = FALSE)
  stopifnot("AI destinations must have unique identifiers and HTTPS URLs" =
              !anyDuplicated(out$provider) && all(startsWith(out$url, "https://")),
            "AI prompt templates must agree with their delivery mode" =
              identical(grepl("{prompt}", out$url, fixed = TRUE), out$prefill))
  out
}

#' Validate a Custom Browser Prompt Template
#' @param url One HTTPS URL containing exactly one literal `{prompt}` marker.
#' @return The validated template, unchanged.
#' @keywords internal
#' @noRd
.validate_ai_url <- function(url) {
  stopifnot("ai_url must be a nonempty HTTPS template" = .is_text(url),
            "ai_url must be at most 2048 bytes" = nchar(url, type = "bytes") <= 2048L,
            "ai_url cannot contain whitespace, controls, or a backslash" =
              !grepl("[[:space:][:cntrl:]\\\\]", url, perl = TRUE),
            "ai_url must use HTTPS with a host and no embedded credentials" =
              grepl(paste0("^https://(?:\\[[0-9A-Fa-f:]+\\]|",
                "[A-Za-z0-9](?:[A-Za-z0-9.-]*[A-Za-z0-9])?)",
                "(?::[0-9]{1,5})?(?:[/?#]|$)"), url, perl = TRUE),
            "ai_url must contain exactly one {prompt} marker after the host" =
              stringi::stri_count_fixed(url, "{prompt}") == 1L &&
              !grepl("{prompt}", sub("^https://([^/?#]+).*$", "\\1", url), fixed = TRUE),
            "ai_url cannot contain other brace placeholders" =
              !grepl("[{}]", sub("{prompt}", "", url, fixed = TRUE)),
            "ai_url cannot contain tracking parameters" =
              !grepl("[?&](utm_[^=&#]*|fbclid|gclid|msclkid)=", url,
                     ignore.case = TRUE, perl = TRUE))
  stopifnot("validated AI template must remain one string" = .is_text(url))
  url
}

#' Resolve a Built-In or Custom AI Destination
#' @param provider A built-in identifier or `custom`.
#' @param url Optional saved custom URL template. It is validated even while a
#'   built-in provider is active, so switching providers can retain it safely.
#' @return A named list with provider, label, url, prefill and notes.
#' @keywords internal
#' @noRd
.ai_target <- function(provider = "mistral", url = NULL) {
  providers <- repl_ai_providers()
  stopifnot("ai_provider must be a built-in identifier or custom" =
              .is_text(provider) && provider %in% c(providers$provider, "custom"))
  if (!is.null(url)) url <- .validate_ai_url(url)
  if (identical(provider, "custom")) {
    stopifnot("custom AI provider requires an ai_url template containing {prompt}" =
                !is.null(url))
    out <- list(provider = "custom", label = "Custom AI", url = url, prefill = TRUE,
      notes = "Prompt URL; use :explain copy if the page does not fill it.")
  } else out <- as.list(providers[match(provider, providers$provider), , drop = FALSE])
  stopifnot("AI destination must be a complete single-provider record" =
              is.list(out) && .is_text(out$provider) && .is_text(out$label) &&
              .is_text(out$url) && .is_flag(out$prefill) && .is_text(out$notes))
  out
}

#' Create an Error Explanation Prompt Locally
#'
#' Builds plain text without opening a browser, reading the clipboard, or sending
#' anything to a service. Only the supplied error, source, and trace are used;
#' object values, the workspace, and session history are never inspected.
#'
#' @param error A condition or nonmissing character error message.
#' @param code Optional character source code.
#' @param trace Optional character vector of calls.
#' @param include_context Include source and trace. `FALSE` uses only the error.
#' @param max_chars Maximum prompt characters, including a truncation notice.
#' @return One bounded, terminal-safe UTF-8 string.
#' @export
#' @examples
#' ai_prompt("object 'x' not found", "mean(x)")
ai_prompt <- function(error, code = NULL, trace = NULL,
                      include_context = TRUE, max_chars = 1800L) {
  stopifnot("error must be a condition or nonmissing character text" =
              inherits(error, "condition") || (is.character(error) &&
                length(error) > 0L && !anyNA(error)),
            "code must be NULL or nonmissing character text" = is.null(code) ||
              (is.character(code) && !anyNA(code)),
            "trace must be NULL or nonmissing character text" = is.null(trace) ||
              (is.character(trace) && !anyNA(trace)),
            "include_context must be a logical scalar" = .is_flag(include_context),
            "max_chars must be an integer of at least 100" = .is_count(max_chars, 100L))
  msg <- if (inherits(error, "condition")) conditionMessage(error) else paste(error, collapse = "\n")
  content <- c("Explain the following R language error and how to fix it:",
    paste0("Error: ", msg),
    if (include_context && length(code)) c("R source:", code),
    if (include_context && length(trace)) c("Call trace:", trace))
  out <- .ai_truncate(.safe_text(enc2utf8(paste(content, collapse = "\n"))), max_chars)
  stopifnot("explanation prompt must be bounded valid text" = .is_text(out) &&
              isTRUE(utf8::utf8_valid(out)) && nchar(out, type = "chars") <= max_chars)
  out
}

#' Bound Prompt Text Without Splitting UTF-8 Code Points
#' @param text One string.
#' @param limit Maximum output characters, at least 100.
#' @return Bounded text including a truncation notice when needed.
#' @keywords internal
#' @noRd
.ai_truncate <- function(text, limit) {
  stopifnot("prompt text must be one string" = .is_text(text),
            "prompt limit must be an integer of at least 100" = .is_count(limit, 100L))
  suffix <- "\n[context truncated]"
  out <- if (nchar(text, type = "chars") <= limit) text else
    paste0(substr(text, 1L, limit - nchar(suffix)), suffix)
  stopifnot("truncated prompt must be bounded" = .is_text(out) &&
              nchar(out, type = "chars") <= limit)
  out
}

#' Create a Browser Link for an R Error Explanation
#'
#' Creates a URL locally. Nothing is sent to a service and no browser is opened.
#' Opening a prompt URL shares its text with the selected provider and may submit
#' the prompt immediately. Use [ai_prompt()] or `:explain copy` to review or paste
#' the prompt yourself. Source code and error messages can contain secrets;
#' `include_context = FALSE` includes only the error message.
#'
#' Providers listed by [repl_ai_providers()] with `prefill = FALSE` return their
#' home page, with no error data in the URL. Copy and paste the prompt there.
#' Prompt URL conventions are browser conveniences, not stable API contracts.
#'
#' @inheritParams ai_prompt
#' @param provider A provider identifier from [repl_ai_providers()], or `custom`.
#' @param url Custom HTTPS template containing exactly one `{prompt}` marker.
#'   Required for `provider = "custom"`; ignored for a built-in provider after
#'   validation. The prompt is percent-encoded before insertion.
#' @return One HTTPS URL of at most 8000 bytes. Long prompt text is truncated
#'   before encoding to respect this bound.
#' @export
#' @examples
#' ai_link("object 'x' not found", "mean(x)", provider = "chatgpt")
#' ai_link("object 'x' not found", provider = "custom",
#'         url = "https://example.org/chat?q={prompt}")
ai_link <- function(error, code = NULL, trace = NULL, include_context = TRUE,
                    max_chars = 1800L, provider = "mistral", url = NULL) {
  target <- .ai_target(provider, url)
  prompt <- ai_prompt(error, code, trace, include_context, max_chars)
  if (!target$prefill) return(target$url)
  template <- strsplit(target$url, "{prompt}", fixed = TRUE)[[1L]]
  before <- template[[1L]]
  after <- if (length(template) > 1L) template[[2L]] else ""
  make_url <- function(n) paste0(before, utils::URLencode(.ai_truncate(prompt, n),
    reserved = TRUE, repeated = TRUE), after)
  n <- max(100L, min(as.integer(max_chars), nchar(prompt, type = "chars")))
  out <- make_url(n)
  while (nchar(out, type = "bytes") > 8000L && n > 100L) {
    n <- max(100L, as.integer(n * 0.8))
    out <- make_url(n)
  }
  stopifnot("AI explanation link must be a bounded HTTPS URL" = .is_text(out) &&
              startsWith(out, "https://") && nchar(out, type = "bytes") <= 8000L)
  out
}
