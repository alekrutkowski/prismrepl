#' Choose a Theme and Apply It Only After Explicit Acceptance
#'
#' The current theme is focused initially. Arrow keys and mouse clicks move
#' focus; Enter accepts and Escape or q cancels. The shared viewer supplies
#' search, resizing and remapped shortcuts. Plain interactive consoles receive
#' a numeric menu; noninteractive use prints the list and an explicit command.
#'
#' @param config Console configuration.
#' @return A built-in theme name, auto, or NULL when cancelled or displayed as text.
#' @noRd
.theme_picker <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  themes <- repl_themes()
  palettes <- lapply(themes, repl_theme)
  current <- match(config$theme$name, themes)
  if (is.na(current)) current <- 1L
  data <- data.frame(theme = ifelse(themes == config$theme$name,
      paste0(themes, " (current)"), themes),
    mode = vapply(palettes, `[[`, character(1L), "mode"),
    background = vapply(palettes, function(theme) theme$palette$background, character(1L)),
    stringsAsFactors = FALSE)
  data <- rbind(data, data.frame(theme = "auto (desktop appearance)", mode = "detect",
    background = "Choose on acceptance", stringsAsFactors = FALSE))
  choices <- c(themes, "auto")
  chosen <- .tui_panel(character(), paste("Theme picker; current:", config$theme$name), config,
    formatter = function(width) .table_format(data, width), choose = TRUE, initial_record = current)
  if (is.null(chosen)) {
    if (!interactive()) .emit("Use :theme NAME to apply a theme, or :theme auto to detect the desktop appearance.", "muted", config)
    return(NULL)
  }
  stopifnot("chosen theme must be a valid record" = .is_count(chosen, 1L) && chosen <= length(choices))
  out <- choices[[chosen]]
  stopifnot("theme picker must return a built-in theme or auto" = .is_text(out) && out %in% choices)
  out
}
