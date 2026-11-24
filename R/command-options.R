#' Choose a Command Option with Explicit Acceptance
#'
#' Stable identifiers are kept separate from the displayed labels. The current
#' option is marked and focused initially. The shared single-choice panel
#' supplies search, mouse focus, resizing, remapped shortcuts and a numeric
#' fallback in plain interactive consoles. Moving focus does not accept an
#' option; Enter applies it and Escape or q cancels.
#'
#' @param data A data frame with unique, nonempty character identifiers in id
#'   and at least one additional character display column. Zero rows are allowed.
#' @param title Nonempty panel title.
#' @param config Console configuration.
#' @param current Current identifier, or NULL. Unmatched identifiers focus the
#'   first option without marking any option as current.
#' @return The accepted identifier, or NULL after cancellation, an empty list
#'   or noninteractive display. This helper does not execute or persist choices.
#' @noRd
.command_choice <- function(data, title, config, current = NULL) {
  stopifnot("data must have id and at least one display column" =
              is.data.frame(data) && ncol(data) >= 2L && "id" %in% names(data),
            "column names must be unique and nonempty" =
              !anyDuplicated(names(data)) && all(nzchar(names(data))),
            "option columns must contain nonmissing character vectors" =
              all(vapply(data, function(column) is.character(column) && is.null(dim(column)), logical(1L))) &&
              !anyNA(data),
            "option identifiers must be unique and nonempty" =
              !anyDuplicated(data$id) && all(nzchar(trimws(data$id))),
            "title must be nonempty text" = .is_text(title),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "current must be NULL or one identifier" = is.null(current) || .is_text(current))
  display <- as.data.frame(data)[names(data) != "id"]
  current_row <- if (is.null(current)) NA_integer_ else match(current, data$id)
  if (!is.na(current_row)) display[[1L]][[current_row]] <-
    paste0(display[[1L]][[current_row]], " (current)")
  chosen <- .tui_panel(character(), title, config,
    formatter = function(width) if (nrow(display)) .table_format(display, width) else
      list(lines = character(), records = integer(), count = 0L), choose = TRUE,
    initial_record = if (is.na(current_row)) 1L else current_row)
  stopifnot("chosen option must be NULL or a valid record" = is.null(chosen) ||
              (.is_count(chosen, 1L) && chosen <= nrow(data)))
  out <- if (is.null(chosen)) NULL else data$id[[chosen]]
  stopifnot("command choice must return NULL or one supplied identifier" =
              is.null(out) || (.is_text(out) && out %in% data$id))
  out
}

#' Choose an On or Off Command Option
#' @param name Command name without a leading colon.
#' @param label Human-readable setting label.
#' @param value Current logical setting.
#' @param config Console configuration.
#' @param toggle Whether to offer an explicit toggle option as well.
#' @return on, off, optionally toggle, or NULL without acceptance.
#' @noRd
.toggle_choice <- function(name, label, value, config, toggle = FALSE) {
  stopifnot("name must be a command without a leading colon" =
              .is_text(name) && !grepl("[[:space:][:cntrl:]:]", name),
            "label must be nonempty text" = .is_text(label),
            "value and toggle must be logical" = .is_flag(value) && .is_flag(toggle),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  current <- if (value) "on" else "off"
  ids <- c("on", "off", if (toggle) "toggle")
  data <- data.frame(id = ids, option = paste0(":", name, " ", ids),
    description = c(paste0("Enable ", label, "."), paste0("Disable ", label, "."),
      if (toggle) paste0("Switch from ", current, " to ", if (value) "off" else "on", ".")),
    stringsAsFactors = FALSE)
  out <- .command_choice(data, paste0(label, "; current: ", current), config, current)
  stopifnot("toggle choice must be NULL or an offered action" =
              is.null(out) || (.is_text(out) && out %in% ids))
  out
}
