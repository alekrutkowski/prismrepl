#' Built-in terminal themes
#'
#' Complete semantic palettes for the console, editor, sidebars, and conditions.
#' Fullscreen surfaces use explicit foreground and background colors. Inline
#' syntax styles change foreground colors only. RGB palettes are downgraded to
#' 256 or 16 colors when requested, without relying on automatic color stripping.
#'
#' @return `repl_themes()` returns names. `repl_theme()` returns a list containing
#'   `name`, `mode`, `colors` (named SGR strings), `palette`, `styles`,
#'   `overrides`, and `color_depth`.
#' @export
#' @examples
#' repl_themes()
#' repl_theme("paper")
#' repl_theme("midnight", list(prompt = "#A7F3D0",
#'   error_badge = list(fg = "#FFFFFF", bg = "#BE123C", bold = TRUE)))
repl_themes <- function() {
  out <- c("midnight", "paper", "white", "black", "ocean", "forest", "solarized-dark",
           "solarized-light", "mono")
  stopifnot("theme names must be unique" = !anyDuplicated(out))
  out
}

#' @rdname repl_themes
#' @param name A built-in theme name.
#' @param overrides Named role overrides. A value can be an SGR string such as
#'   `"1;36"`, a foreground color such as `"#7DD3FC"`, or a list with `fg`, `bg`,
#'   `bold`, `italic`, and `underline` entries. Color values use six-digit hex.
#' @param color_depth One of `0`, `16`, `256`, or `16777216` (true color).
#' @export
repl_theme <- function(name = "midnight", overrides = NULL,
                       color_depth = 16777216L) {
  stopifnot("name must be one theme name" = is.character(name) &&
              length(name) == 1L && !is.na(name) && name %in% repl_themes(),
            "overrides must be NULL, a list, or character" =
              is.null(overrides) || is.list(overrides) || is.character(overrides),
            "color_depth must be 0, 16, 256, or 16777216" =
              is.numeric(color_depth) && length(color_depth) == 1L &&
              !is.na(color_depth) && color_depth %in% c(0, 16, 256, 16777216))
  p <- .theme_palette(name)
  light <- name %in% c("paper", "white", "solarized-light")
  s <- function(fg = NULL, bg = NULL, bold = FALSE, italic = FALSE,
                underline = FALSE) list(fg = fg, bg = bg, bold = bold,
                                        italic = italic, underline = underline)
  styles <- list(
    prompt = s(p$accent, bold = TRUE), continuation = s(p$muted),
    input = s(p$foreground), output = s(p$foreground),
    message = s(p$message), warning = s(p$warning), error = s(p$error),
    trace = s(p$cyan), comment = s(p$muted), string = s(p$green),
    number = s(p$magenta), keyword = s(p$blue, bold = TRUE),
    function_name = s(p$cyan), operator = s(p$yellow), symbol = s(p$foreground),
    rainbow1 = s(p$yellow), rainbow2 = s(p$magenta), rainbow3 = s(p$cyan),
    rainbow4 = s(p$green), rainbow5 = s(p$blue), rainbow6 = s(p$accent),
    muted = s(p$muted), accent = s(p$accent, bold = TRUE),
    selection = s(p$foreground, p$selection, bold = TRUE),
    heading = s(p$accent, bold = TRUE),
    background = s(p$foreground, p$background),
    panel = s(p$foreground, p$panel),
    border = s(p$border, p$background),
    panel_header = s(p$accent, p$raised, bold = TRUE),
    status = s(p$muted, p$raised),
    error_badge = s("#FFFFFF", p$error_badge, bold = TRUE),
    warning_badge = s(if (light) "#FFFFFF" else "#17202A", p$warning_badge, bold = TRUE),
    message_badge = s(if (light) "#FFFFFF" else "#082032", p$message_badge, bold = TRUE),
    success_badge = s(if (light) "#FFFFFF" else "#10251B", p$success_badge, bold = TRUE),
    error_frame = s(p$error, p$error_panel),
    warning_frame = s(p$warning, p$warning_panel),
    message_frame = s(p$message, p$message_panel),
    error_panel = s(p$foreground, p$error_panel),
    warning_panel = s(p$foreground, p$warning_panel),
    message_panel = s(p$foreground, p$message_panel),
    success = s(p$green, bold = TRUE), link = s(p$blue, underline = TRUE))
  styles$screen <- styles$background
  styles$header <- styles$panel_header
  styles$editor <- styles$panel
  styles$sidebar <- styles$panel
  styles$panel_title <- styles$panel_header
  styles$sidebar_heading <- styles$panel_header
  overrides <- if (is.null(overrides)) list() else as.list(overrides)
  if (length(overrides)) {
    stopifnot("overrides must be named with known roles" =
                !is.null(names(overrides)) && !anyDuplicated(names(overrides)) &&
                all(names(overrides) %in% names(styles)))
    styles[names(overrides)] <- Map(function(base, value) {
      if (is.character(value) && length(value) == 1L && !is.na(value)) {
        stopifnot("overrides must be hex colors or numeric SGR attributes" =
                    grepl("^#[[:xdigit:]]{6}$", value) || grepl("^[0-9;]*$", value))
        if (startsWith(value, "#")) base$fg <- toupper(value)
        return(base)
      }
      stopifnot("style overrides must be lists with known entries" = is.list(value) &&
                  length(value) > 0L && !is.null(names(value)) &&
                  !anyDuplicated(names(value)) &&
                  all(names(value) %in% c("fg", "bg", "bold", "italic", "underline")))
      result <- utils::modifyList(base, value, keep.null = TRUE)
      .style_sgr(result, 16777216L)
      result
    }, styles[names(overrides)], overrides)
  }
  out <- list(name = name, mode = if (name == "mono") "mono" else if (light) "light" else "dark",
              colors = character(), palette = p, styles = styles,
              overrides = overrides, color_depth = as.integer(color_depth))
  out <- .theme_at_depth(out, if (name == "mono") 0L else color_depth)
  stopifnot("theme must contain named color attributes" = is.character(out$colors) &&
              !anyNA(out$colors) && all(grepl("^[0-9;]*$", out$colors)),
            "theme must include every style role" = identical(names(out$colors), names(styles)))
  out
}

#' Define RGB colors for a built-in theme
#' @param name A theme name.
#' @return A named list of six-digit hex colors.
#' @keywords internal
#' @noRd
.theme_palette <- function(name) {
  stopifnot("name must be a theme name" = is.character(name) && length(name) == 1L &&
              name %in% repl_themes())
  p <- list(background = "#0B1020", panel = "#111A2E", raised = "#1A2742",
    foreground = "#E6EDF7", muted = "#A0AEC2", border = "#354867",
    accent = "#7DD3FC", selection = "#263B59", cyan = "#67E8F9",
    blue = "#93C5FD", green = "#A7F3D0", yellow = "#FCD34D", magenta = "#C4B5FD",
    error = "#FDA4AF", warning = "#FDE68A", message = "#BAE6FD",
    error_badge = "#BE123C", warning_badge = "#FBBF24", message_badge = "#38BDF8",
    success_badge = "#6EE7B7", error_panel = "#2A1423", warning_panel = "#2B2414",
    message_panel = "#10243B")
  changes <- switch(name,
    paper = list(background = "#F4F7FC", panel = "#FFFFFF", raised = "#E8EEF8",
      foreground = "#1E293B", muted = "#526477", border = "#A6B6CB",
      accent = "#1D4ED8", selection = "#DBEAFE", cyan = "#0369A1", blue = "#4338CA",
      green = "#047857", yellow = "#92400E", magenta = "#9333EA",
      error = "#A32129", warning = "#805000", message = "#075985",
      error_badge = "#B4232F", warning_badge = "#946200", message_badge = "#0369A1",
      success_badge = "#047857", error_panel = "#FFF1F0", warning_panel = "#FFF8E1",
      message_panel = "#EFF8FF"),
    white = list(background = "#FFFFFF", panel = "#FFFFFF", raised = "#F1F3F5",
      foreground = "#16181D", muted = "#525866", border = "#AEB5C0", accent = "#1248BD",
      selection = "#DBEAFE", cyan = "#006581", blue = "#213DB0", green = "#087031",
      yellow = "#875000", magenta = "#8A267B", error = "#A11928", warning = "#794600",
      message = "#005A80", error_badge = "#B1162E", warning_badge = "#8B5600",
      message_badge = "#00678B", success_badge = "#087031", error_panel = "#FFF0F2",
      warning_panel = "#FFF8E6", message_panel = "#EDF8FF"),
    black = list(background = "#000000", panel = "#000000", raised = "#151515",
      foreground = "#F2F2F2", muted = "#ACACB2", border = "#55555F", accent = "#A5B4FC",
      selection = "#272C48", cyan = "#67E8F9", blue = "#93B4FF", green = "#86EFAC",
      yellow = "#FDE68A", magenta = "#F0ABFC", error = "#FDA4AF", warning = "#FDE68A",
      message = "#7DD3FC", error_panel = "#240A10", warning_panel = "#211900",
      message_panel = "#001A28"),
    ocean = list(background = "#061B26", panel = "#0B2937", raised = "#133B49",
      foreground = "#DEF6F8", muted = "#98BCC4", border = "#2A6372", accent = "#5EEAD4",
      selection = "#1B5360", cyan = "#67E8F9", blue = "#7DD3FC", green = "#99F6E4",
      yellow = "#FDE68A", magenta = "#C4B5FD", message_panel = "#0C334A"),
    forest = list(background = "#101B17", panel = "#182820", raised = "#253B2F",
      foreground = "#E5F4E9", muted = "#A2B8A9", border = "#416B50", accent = "#86EFAC",
      selection = "#2B5039", cyan = "#99F6E4", blue = "#93C5FD", green = "#BEF264",
      yellow = "#FDE68A", magenta = "#E9D5FF", message_panel = "#142D31"),
    `solarized-dark` = list(background = "#002B36", panel = "#073642", raised = "#134552",
      foreground = "#CAD6D6", muted = "#93A1A1", border = "#41636B", accent = "#2AA198",
      selection = "#25525B", cyan = "#2AA198", blue = "#6BB5E8", green = "#B8C851",
      yellow = "#D8B450", magenta = "#DC86B1", error = "#F17D77",
      warning = "#E8C765", message = "#90C7EC", error_badge = "#C33232",
      warning_badge = "#D8B450", message_badge = "#6BB5E8"),
    `solarized-light` = list(background = "#FDF6E3", panel = "#F6EED8", raised = "#EEE8D5",
      foreground = "#40545B", muted = "#657B83", border = "#A6B1A8", accent = "#006A78",
      selection = "#DCDCC3", cyan = "#087F78", blue = "#116CA4", green = "#626F00",
      yellow = "#876300", magenta = "#A72D67", error = "#A82B26",
      warning = "#805800", message = "#08628A", error_badge = "#B52E29",
      warning_badge = "#876300", message_badge = "#116CA4", success_badge = "#626F00",
      error_panel = "#F9E9DA", warning_panel = "#F9EFCB", message_panel = "#E8F0E8"),
    list())
  out <- utils::modifyList(p, changes)
  stopifnot("palette must contain valid hex colors" = all(vapply(out, function(x)
    is.character(x) && length(x) == 1L && grepl("^#[[:xdigit:]]{6}$", x), logical(1))))
  out
}

#' Convert an RGB color to a supported terminal color
#' @param hex A six-digit hex color.
#' @param background Whether the SGR controls the background.
#' @param depth Terminal color depth.
#' @return Numeric SGR parameters.
#' @keywords internal
#' @noRd
.color_sgr <- function(hex, background = FALSE, depth = 16777216L) {
  stopifnot("hex must be one RGB color" = is.character(hex) && length(hex) == 1L &&
              !is.na(hex) && grepl("^#[[:xdigit:]]{6}$", hex),
            "background must be scalar logical" = is.logical(background) &&
              length(background) == 1L && !is.na(background),
            "depth must be supported" = is.numeric(depth) && length(depth) == 1L &&
              depth %in% c(0, 16, 256, 16777216))
  rgb <- strtoi(substring(hex, c(2L, 4L, 6L), c(3L, 5L, 7L)), 16L)
  lead <- if (background) 48L else 38L
  if (depth == 0L) return(integer())
  if (depth == 16777216L) return(c(lead, 2L, rgb))
  indices <- if (depth == 256L) 16:255 else 0:15
  difference <- sweep(.theme_ansi_rgb[indices + 1L, , drop = FALSE], 2L, rgb)
  index <- indices[[which.min(rowSums(difference * difference))]]
  out <- if (depth == 256L) c(lead, 5L, index) else
    if (index < 8L) (if (background) 40L else 30L) + index else
      (if (background) 100L else 90L) + index - 8L
  stopifnot("color SGR must contain numeric parameters" = is.numeric(out) && !anyNA(out))
  out
}

#' Compile foreground, background, and emphasis into SGR
#' @param style A list of style properties.
#' @param depth Terminal color depth.
#' @return A scalar numeric SGR string.
#' @keywords internal
#' @noRd
.style_sgr <- function(style, depth) {
  stopifnot("style must be a list" = is.list(style),
            "depth must be supported" = is.numeric(depth) && length(depth) == 1L &&
              depth %in% c(0, 16, 256, 16777216),
            "style colors must be hex colors or NULL" = all(vapply(style[c("fg", "bg")], function(x)
              is.null(x) || (is.character(x) && length(x) == 1L && !is.na(x) &&
                              grepl("^#[[:xdigit:]]{6}$", x)), logical(1))),
            "style flags must be logical or NULL" = all(vapply(style[c("bold", "italic", "underline")], function(x)
              is.null(x) || (is.logical(x) && length(x) == 1L && !is.na(x)), logical(1))))
  out <- if (depth == 0L) "" else paste(c(
    if (isTRUE(style$bold)) 1L, if (isTRUE(style$italic)) 3L,
    if (isTRUE(style$underline)) 4L,
    if (!is.null(style$fg)) .color_sgr(style$fg, FALSE, depth),
    if (!is.null(style$bg)) .color_sgr(style$bg, TRUE, depth)), collapse = ";")
  stopifnot("style must compile to numeric SGR" = length(out) == 1L && grepl("^[0-9;]*$", out))
  out
}

#' Downgrade custom SGR color parameters
#' @param sgr A numeric SGR string.
#' @param depth Terminal color depth.
#' @return One numeric SGR string.
#' @keywords internal
#' @noRd
.sgr_at_depth <- function(sgr, depth) {
  stopifnot("sgr must be a scalar numeric SGR string" = is.character(sgr) && length(sgr) == 1L &&
              !is.na(sgr) && grepl("^[0-9;]*$", sgr),
            "depth must be supported" = is.numeric(depth) && length(depth) == 1L &&
              depth %in% c(0, 16, 256, 16777216))
  if (!nzchar(sgr) || depth == 0L) return("")
  numbers <- as.integer(strsplit(sgr, ";", fixed = TRUE)[[1L]])
  stopifnot("SGR numbers must be finite integers" = !anyNA(numbers))
  convert <- function(x) {
    if (!length(x)) return(integer())
    if (length(x) >= 5L && x[[1L]] %in% c(38L, 48L) && x[[2L]] == 2L) {
      stopifnot("RGB SGR channels must be between 0 and 255" = all(x[3:5] %in% 0:255))
      color <- paste0("#", paste(sprintf("%02X", x[3:5]), collapse = ""))
      return(c(.color_sgr(color, x[[1L]] == 48L, depth), convert(x[-seq_len(5L)])))
    }
    if (length(x) >= 3L && x[[1L]] %in% c(38L, 48L) && x[[2L]] == 5L) {
      stopifnot("indexed SGR colors must be between 0 and 255" = x[[3L]] %in% 0:255)
      if (depth == 16L) {
        rgb <- .theme_ansi_rgb[x[[3L]] + 1L, ]
        color <- paste0("#", paste(sprintf("%02X", as.integer(rgb)), collapse = ""))
        return(c(.color_sgr(color, x[[1L]] == 48L, depth), convert(x[-seq_len(3L)])))
      }
      return(c(x[1:3], convert(x[-seq_len(3L)])))
    }
    c(x[[1L]], convert(x[-1L]))
  }
  out <- paste(convert(numbers), collapse = ";")
  stopifnot("downgraded SGR must be numeric text" = length(out) == 1L && grepl("^[0-9;]*$", out))
  out
}

#' Recompile a theme for terminal capability while retaining custom overrides
#' @param theme A theme list, including legacy lists with name and colors.
#' @param color_depth Terminal color depth.
#' @return A complete theme list.
#' @keywords internal
#' @noRd
.theme_at_depth <- function(theme, color_depth) {
  stopifnot("theme must be a named theme list" = is.list(theme) &&
              is.character(theme$name) && length(theme$name) == 1L,
            "color_depth must be supported" = is.numeric(color_depth) && length(color_depth) == 1L &&
              color_depth %in% c(0, 16, 256, 16777216))
  if (is.null(theme$styles)) {
    legacy <- theme$colors
    stopifnot("legacy theme must contain named numeric SGR colors" =
                is.character(legacy) && !is.null(names(legacy)) && !anyNA(legacy) &&
                all(grepl("^[0-9;]*$", legacy)))
    built_in <- if (theme$name %in% repl_themes()) theme$name else "midnight"
    replacement <- repl_theme(built_in, color_depth = color_depth)
    legacy <- legacy[names(legacy) %in% names(replacement$styles)]
    replacement$overrides <- as.list(legacy)
    replacement$name <- theme$name
    theme <- replacement
  }
  depth <- if (identical(theme$name, "mono")) 0L else as.integer(color_depth)
  theme$colors <- vapply(theme$styles, .style_sgr, character(1), depth = depth)
  if (depth == 16L) {
    stock <- setdiff(names(.theme_badges16), names(theme$overrides))
    theme$colors[stock] <- .theme_badges16[stock]
  }
  if (length(theme$overrides)) invisible(lapply(names(theme$overrides), function(role) {
    value <- theme$overrides[[role]]
    if (is.character(value) && length(value) == 1L && grepl("^[0-9;]*$", value))
      theme$colors[[role]] <<- .sgr_at_depth(value, depth)
    NULL
  }))
  theme$color_depth <- depth
  stopifnot("theme must contain safe SGR colors" = is.character(theme$colors) &&
              !anyNA(theme$colors) && all(grepl("^[0-9;]*$", theme$colors)))
  theme
}

.theme_badges16 <- c(error_badge = "1;97;41", warning_badge = "1;30;43",
                     message_badge = "1;97;44", success_badge = "1;30;42")

# Standard ANSI colors, the xterm RGB cube, and xterm grayscale ramp.
.theme_ansi_rgb <- local({
  hex <- c("000000", "CD0000", "00CD00", "CDCD00", "0000EE", "CD00CD", "00CDCD", "E5E5E5",
           "7F7F7F", "FF0000", "00FF00", "FFFF00", "5C5CFF", "FF00FF", "00FFFF", "FFFFFF")
  basic <- t(vapply(hex, function(x)
    strtoi(substring(x, c(1L, 3L, 5L), c(2L, 4L, 6L)), 16L), integer(3)))
  cube <- as.matrix(expand.grid(b = c(0, 95, 135, 175, 215, 255),
    g = c(0, 95, 135, 175, 215, 255), r = c(0, 95, 135, 175, 215, 255))[, c("r", "g", "b")])
  gray <- matrix(rep(seq(8, 238, 10), 3L), ncol = 3L)
  out <- rbind(basic, cube, gray)
  stopifnot("terminal color table must have 256 RGB rows" = identical(dim(out), c(256L, 3L)))
  out
})

#' Escape terminal control characters for display
#'
#' @param text A character vector.
#' @return UTF-8 text without executable terminal controls. Newlines are kept;
#'   tabs become four spaces. This helper does not alter Unicode letters.
#' @keywords internal
#' @noRd
.safe_text <- function(text) {
  stopifnot("text must be character" = is.character(text))
  out <- iconv(enc2utf8(text), from = "UTF-8", to = "UTF-8", sub = "\uFFFD")
  out[is.na(out)] <- "NA"
  out <- gsub("\t", "    ", out, fixed = TRUE)
  bad <- grepl("[\\x{0001}-\\x{0008}\\x{000B}-\\x{001F}\\x{007F}-\\x{009F}]",
               out, perl = TRUE)
  if (any(bad)) out[bad] <- vapply(strsplit(out[bad], "", fixed = TRUE), function(chars) {
    points <- utf8ToInt(paste0(chars, collapse = ""))
    unsafe <- (points < 32L & points != 10L) |
      (points >= 127L & points <= 159L)
    chars[unsafe] <- sprintf("\\x%02x", points[unsafe])
    paste0(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  out <- unname(out)
  stopifnot("safe output must preserve vector length" = length(out) == length(text),
            "safe output must be character" = is.character(out))
  out
}

#' Apply a semantic terminal style without auto-detection or color stripping
#'
#' Embedded full resets restore the enclosing style, and every newline reopens
#' it. A compositor can therefore paint a panel row containing syntax colors or
#' condition badges without losing its background after an inner reset.
#'
#' @param text Character text, already made safe for terminal display.
#' @param role One semantic theme role.
#' @param theme A theme returned by [repl_theme()].
#' @param color Whether to emit ANSI attributes.
#' @param color_depth Terminal color depth; defaults to the theme's depth.
#' @return A character vector of the same length as `text`.
#' @keywords internal
#' @noRd
style_text <- function(text, role, theme = repl_theme(), color = TRUE,
                        color_depth = theme$color_depth) {
  stopifnot("text must be character" = is.character(text),
            "role must be a single string" = is.character(role) && length(role) == 1L && !is.na(role),
            "theme must contain the requested color role" = is.list(theme) &&
              is.character(theme$colors) && role %in% names(theme$colors),
            "color must be TRUE or FALSE" = is.logical(color) && length(color) == 1L && !is.na(color))
  if (!length(text)) return(text)
  if (is.null(color_depth)) color_depth <- 16L
  stopifnot("color_depth must be supported" = is.numeric(color_depth) && length(color_depth) == 1L &&
              color_depth %in% c(0, 16, 256, 16777216))
  sgr <- theme$colors[[role]]
  if (!is.null(theme$color_depth) && color_depth != theme$color_depth) {
    if (!is.null(theme$styles[[role]])) {
      sgr <- .style_sgr(theme$styles[[role]], color_depth)
      override <- theme$overrides[[role]]
      if (color_depth == 16L && is.null(override) && role %in% names(.theme_badges16))
        sgr <- .theme_badges16[[role]]
      if (is.character(override) && length(override) == 1L && grepl("^[0-9;]*$", override))
        sgr <- .sgr_at_depth(override, color_depth)
    } else sgr <- .sgr_at_depth(sgr, color_depth)
  }
  stopifnot("style must be numeric SGR attributes" = length(sgr) == 1L &&
              !is.na(sgr) && grepl("^[0-9;]*$", sgr))
  out <- if (!color || color_depth == 0L || !nzchar(sgr) || identical(sgr, "0")) text else {
    start <- paste0("\033[", sgr, "m")
    payload <- gsub("\033[0m", paste0("\033[0m", start), text, fixed = TRUE)
    payload <- gsub("\n", paste0("\033[0m\n", start), payload, fixed = TRUE)
    paste0(start, payload, "\033[0m")
  }
  stopifnot("styled text must preserve vector length" = length(out) == length(text))
  out
}
