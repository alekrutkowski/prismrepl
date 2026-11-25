#' Open the Complete Shortcut Cheatsheet Without Replacing Pending Code
#'
#' The console borrows its existing native input session for the same structured
#' table viewer used elsewhere. Its rows come from the effective keymap, shared
#' with the :keys binding menu. Searching, scrolling and resizing remain inside
#' the viewer; closing it restores the pending expression and its cursor.
#'
#' @param state Current editable input state.
#' @param config Console configuration, or partial editor configuration.
#' @param handle Active terminal input handle, borrowed by the viewer.
#' @param alternate Use a temporary alternate screen when editing inline.
#' @return The unchanged editable state, except for redraw or failure feedback.
#' @keywords internal
#' @noRd
.editor_open_shortcuts <- function(state, config, handle, alternate = FALSE) {
  stopifnot("state must contain an editable draft and valid cursor" =
    is.list(state) && is.character(state$chars) && !anyNA(state$chars) &&
      identical(state$action, "edit") && .is_count(state$cursor) &&
      state$cursor <= length(state$chars),
    "config must be a list" = is.list(config),
    "alternate must be logical" = .is_flag(alternate))
  if (!inherits(config, "prismrepl_config")) {
    defaults <- repl_config(theme = if (is.null(config$theme)) "midnight" else config$theme,
      color = config$color, settings_file = NULL, persist = FALSE)
    config <- utils::modifyList(defaults, config)
    class(config) <- class(defaults)
  }
  config$viewer_handle <- handle
  config$viewer_alternate <- alternate
  problem <- tryCatch({
    .tui_table(keymap(config), "Keyboard shortcuts", config)
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(problem)) state$overlay <- paste0("Cannot open shortcuts: ",
    gsub("[\r\n]", " ", .safe_text(problem)))
  state$clear <- isTRUE(state$clear) || !alternate
  stopifnot("the shortcuts viewer must leave a valid editable draft" =
    identical(state$action, "edit") && .is_count(state$cursor) &&
      state$cursor <= length(state$chars))
  state
}
