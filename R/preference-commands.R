#' Change a Persistent Editing Preference
#' @param name The rainbow or autopair setting.
#' @param args Optional on, off or toggle argument; no argument opens a choice.
#' @param state Console session environment.
#' @return NULL invisibly.
#' @noRd
.editing_preference_command <- function(name, args, state) {
  stopifnot("unknown editing preference" = .is_text(name) && name %in% c("rainbow", "autopair"),
            "args must be nonmissing text" = is.character(args) && !anyNA(args),
            "state must be an environment" = is.environment(state),
            "use on, off or toggle" = !length(args) ||
              (length(args) == 1L && args %in% c("on", "off", "toggle")))
  label <- if (name == "rainbow") "Rainbow delimiters" else "Automatic pairing"
  if (!length(args)) {
    args <- .toggle_choice(name, label, isTRUE(state$config[[name]]), state$config, toggle = TRUE)
    if (is.null(args)) return(invisible(NULL))
  }
  stopifnot("chosen editing action must be on, off or toggle" =
              .is_text(args) && args %in% c("on", "off", "toggle"))
  state$config[[name]] <- if (args == "toggle") !isTRUE(state$config[[name]]) else args == "on"
  if (is.environment(state$config$screen)) screen_invalidate(state$config$screen)
  saved <- .persist_preferences(state$config, name)
  .emit(paste0(label, ": ", if (isTRUE(state$config[[name]])) "on" else "off",
    if (saved) " (saved)" else " (this session)"), "message", state$config)
  if (is.environment(state$config$screen) && isTRUE(state$config$screen$active))
    screen_render(state$config$screen, config = state$config)
  stopifnot("editing preference must remain logical" = .is_flag(state$config[[name]]))
  invisible(NULL)
}

#' Select or Describe an AI Browser Destination
#' @param args Provider identifier, or custom with an optional URL template.
#' @param state Console session environment.
#' @return NULL invisibly.
#' @noRd
.ai_command <- function(args, state) {
  stopifnot("args must be nonmissing text" = is.character(args) && !anyNA(args),
            "state must be an environment" = is.environment(state))
  cfg <- state$config
  if (!length(args)) {
    providers <- repl_ai_providers()
    providers <- data.frame(id = providers$provider, provider = providers$provider, name = providers$label,
      prompt = ifelse(providers$prefill, "in URL; copy fallback", "copy and paste"),
      stringsAsFactors = FALSE)
    providers <- rbind(providers, data.frame(id = "custom", provider = "custom", name = "Custom HTTPS template",
      prompt = if (is.null(cfg$ai_url)) "Enter an HTTPS URL with {prompt}" else "Use saved URL template",
      stringsAsFactors = FALSE))
    args <- .command_choice(providers, paste0("AI provider (current: ", cfg$ai_provider, ")"),
      cfg, current = cfg$ai_provider)
    if (is.null(args)) return(invisible(NULL))
    stopifnot("chosen provider must identify a menu entry" = .is_text(args) && args %in% providers$id)
    if (identical(args, "custom") && is.null(cfg$ai_url)) {
      state$initial <- ":ai custom "
      .emit("Custom AI needs an HTTPS URL containing {prompt}. Complete the staged :ai custom command to apply it.",
        "message", cfg)
      stopifnot("custom provider setup must be staged for editing" = identical(state$initial, ":ai custom "))
      return(invisible(NULL))
    }
  }
  stopifnot("use :ai PROVIDER or :ai custom URL" = length(args) <= 2L)
  provider <- tolower(args[[1L]])
  stopifnot("a URL is allowed only after :ai custom" = length(args) == 1L || provider == "custom")
  url <- if (length(args) == 2L) args[[2L]] else cfg$ai_url
  target <- .ai_target(provider, url)
  state$config$ai_provider <- target$provider
  state$config["ai_url"] <- list(url)
  saved <- .persist_preferences(state$config, c("ai_provider", "ai_url"))
  .emit(paste0("AI provider: ", target$label, if (saved) " (saved). " else " (this session). ",
    if (target$prefill) "Error links include the prompt; :explain copy is also available." else
      "Use :explain copy, open the provider, and paste the prompt."), "message", state$config)
  stopifnot("AI provider must be retained" = identical(state$config$ai_provider, target$provider))
  invisible(NULL)
}

#' Display an Error Link or Explicitly Copy Its Explanation Prompt
#' @param args Optional copy and minimal arguments, in either order.
#' @param state Console session environment.
#' @return NULL invisibly.
#' @noRd
.explain_command <- function(args, state) {
  stopifnot("args must be nonmissing text" = is.character(args) && !anyNA(args),
            "state must be an environment" = is.environment(state),
            "no error is available" = !is.null(state$last_error$error),
            "use :explain [copy] [minimal]" = length(args) <= 2L &&
              all(args %in% c("copy", "minimal")) && !anyDuplicated(args))
  cfg <- state$config
  error <- state$last_error
  include <- isTRUE(cfg$link_context) && !"minimal" %in% args
  target <- .ai_target(cfg$ai_provider, cfg$ai_url)
  if ("copy" %in% args) {
    copied <- .clipboard_write(ai_prompt(error$error, error$code, error$trace, include_context = include))
    .emit(sub("selection", "explanation request", copied$reason, fixed = TRUE), "message", cfg)
  }
  url <- ai_link(error$error, error$code, error$trace, include_context = include,
                 provider = cfg$ai_provider, url = cfg$ai_url)
  .emit(url, "output", cfg)
  if (!target$prefill) .emit(paste0(target$label,
    " uses a pasted prompt. Use :explain copy", if (!include) " minimal", ", open the link, then paste."),
    "message", cfg)
  stopifnot("explanation must have a destination" = .is_text(url))
  invisible(NULL)
}
