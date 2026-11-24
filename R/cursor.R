#' Resolve the Explicit Background of One Terminal Style
#'
#' Read SGR parameters in order, including color resets and reverse video.
#' Indexed and basic colors use the same ANSI palette as theme conversion.
#' A host-dependent or malformed color yields NULL without sending a query.
#'
#' @param sgr One numeric SGR parameter string, without escape characters.
#' @return Three integer RGB channels, or NULL for an unknown background.
#' @keywords internal
#' @noRd
.cursor_background <- function(sgr) {
  stopifnot("sgr must be one nonmissing string" = is.character(sgr) &&
              length(sgr) == 1L && !is.na(sgr))
  if (!nzchar(sgr) || !grepl("^[0-9;]*$", sgr)) return(NULL)
  # Append a sentinel so empty trailing parameters retain their reset meaning.
  parts <- head(strsplit(paste0(sgr, ";!"), ";", fixed = TRUE)[[1L]], -1L)
  parts[!nzchar(parts)] <- "0"
  codes <- suppressWarnings(as.integer(parts))
  if (anyNA(codes)) return(NULL)
  foreground <- background <- NULL
  reverse <- bold <- faint <- concealed <- foreground_basic <- FALSE
  i <- 1L
  while (i <= length(codes)) {
    code <- codes[[i]]
    if (code %in% c(38L, 48L, 58L)) {
      if (i == length(codes)) return(NULL)
      kind <- codes[[i + 1L]]
      count <- if (kind == 2L) 5L else if (kind == 5L) 3L else 0L
      if (!count || i + count - 1L > length(codes)) return(NULL)
      channels <- codes[seq.int(i + 2L, i + count - 1L)]
      if (any(channels < 0L | channels > 255L)) return(NULL)
      rgb <- if (kind == 2L) channels else as.integer(.theme_ansi_rgb[channels[[1L]] + 1L, ])
      if (code == 38L) {
        foreground <- rgb
        foreground_basic <- kind == 5L && channels[[1L]] < 8L
      } else if (code == 48L) background <- rgb
      # SGR 58 colors an underline; its parameters must not become backgrounds.
      i <- i + count
      next
    }
    if (code == 0L) {
      foreground <- background <- NULL
      reverse <- bold <- faint <- concealed <- foreground_basic <- FALSE
    } else if (code == 39L) {
      foreground <- NULL
      foreground_basic <- FALSE
    } else if (code == 49L) background <- NULL
    else if (code >= 30L && code <= 37L) {
      foreground <- as.integer(.theme_ansi_rgb[code - 30L + 1L, ])
      foreground_basic <- TRUE
    } else if (code >= 90L && code <= 97L) {
      foreground <- as.integer(.theme_ansi_rgb[code - 90L + 9L, ])
      foreground_basic <- FALSE
    } else if (code >= 40L && code <= 47L)
      background <- as.integer(.theme_ansi_rgb[code - 40L + 1L, ])
    else if (code >= 100L && code <= 107L)
      background <- as.integer(.theme_ansi_rgb[code - 100L + 9L, ])
    else if (code == 7L) reverse <- TRUE
    else if (code == 27L) reverse <- FALSE
    else if (code == 1L) bold <- TRUE
    else if (code == 2L) faint <- TRUE
    else if (code == 22L) { bold <- FALSE; faint <- FALSE }
    else if (code == 8L) concealed <- TRUE
    else if (code == 28L) concealed <- FALSE
    i <- i + 1L
  }
  # Terminals may brighten basic foregrounds for bold and interpret dim/conceal
  # differently. Under reverse video those effects make the surface uncertain.
  out <- if (reverse && (faint || concealed || (bold && foreground_basic))) NULL
    else if (reverse) foreground else background
  stopifnot("cursor background must be NULL or three RGB channels" = is.null(out) ||
              (is.integer(out) && length(out) == 3L && all(out >= 0L & out <= 255L)))
  out
}

#' Choose a Contrasting Cursor for an Explicitly Painted Surface
#'
#' Black or white is selected using sRGB relative luminance and contrast ratios.
#' An empty string requests the host cursor color when the surface is unknown,
#' colors are disabled, or the theme is monochrome. Theme names do not otherwise
#' influence the result; the final SGR background includes custom overrides.
#'
#' @param config Console configuration or a list containing color and theme.
#' @param role The painted surface role, normally panel or editor.
#' @return One of #FFFFFF, #000000, or an empty string for the host default.
#' @keywords internal
#' @noRd
.cursor_color <- function(config, role = "panel") {
  stopifnot("config must be a list" = is.list(config),
            "role must be one nonempty string" = .is_text(role))
  theme <- config$theme
  out <- ""
  if (isTRUE(config$color) && is.list(theme) && !identical(theme$name, "mono") &&
      !identical(theme$mode, "mono") && is.character(theme$colors) && role %in% names(theme$colors)) {
    sgr <- theme$colors[[role]]
    rgb <- if (length(sgr) == 1L && !is.na(sgr)) .cursor_background(sgr) else NULL
    if (!is.null(rgb)) {
      # https://www.w3.org/WAI/WCAG22/Understanding/non-text-contrast
      channels <- rgb / 255
      linear <- ifelse(channels <= 0.04045, channels / 12.92, ((channels + 0.055) / 1.055)^2.4)
      luminance <- sum(linear * c(0.2126, 0.7152, 0.0722))
      black <- (luminance + 0.05) / 0.05
      white <- 1.05 / (luminance + 0.05)
      out <- if (black >= white) "#000000" else "#FFFFFF"
    }
  }
  stopifnot("cursor color must be a supported contrast color or host default" =
              is.character(out) && length(out) == 1L && out %in% c("", "#000000", "#FFFFFF"))
  out
}

#' Annotate a Frame with Its Desired Cursor State
#' @param frame A compositor frame list, including any layout and hit metadata.
#' @param config Console configuration.
#' @param role The actual painted editor surface role.
#' @param visible Whether the native editing cursor should be visible.
#' @return The same frame with cursor_visible and cursor_color fields. A hidden
#'   cursor uses NULL color, leaving any managed tint unchanged while hidden.
#' @keywords internal
#' @noRd
.cursor_frame <- function(frame, config, role = "panel", visible = TRUE) {
  stopifnot("frame must be a list" = is.list(frame),
            "config must be a list" = is.list(config),
            "role must be one nonempty string" = .is_text(role),
            "visible must be logical" = .is_flag(visible))
  frame[c("cursor_visible", "cursor_color")] <- list(visible,
    if (visible) .cursor_color(config, role) else NULL)
  stopifnot("frame must retain the requested cursor visibility" = identical(frame$cursor_visible, visible),
            "frame cursor color must match its visibility" =
              if (visible) is.character(frame$cursor_color) && length(frame$cursor_color) == 1L &&
                frame$cursor_color %in% c("", "#000000", "#FFFFFF") else is.null(frame$cursor_color))
  frame
}
