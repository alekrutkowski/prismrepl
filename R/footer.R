#' Draw Shared Colored Keyboard Hints
#' @param items Named text; names are keys and values explain their action.
#' @param config Console configuration.
#' @param width Maximum display cells.
#' @return A safe ANSI scalar with two spaces between hints.
#' @noRd
.footer_hints <- function(items, config, width) {
  stopifnot("hints must be named nonmissing text" = is.character(items) && !anyNA(items) &&
              (length(items) == 0L || (!is.null(names(items)) && !anyNA(names(items)))),
            "config must be a list" = is.list(config), "width must be positive" = .is_count(width, 1L))
  keep <- nzchar(names(items)) & nzchar(items)
  items <- items[keep]
  key <- .safe_text(names(items))
  value <- .safe_text(unname(items))
  parts <- vapply(seq_along(items), function(i) paste0(
    style_text(key[[i]], "accent", config$theme, config$color), " ",
    style_text(value[[i]], "status", config$theme, config$color)), character(1L))
  while (length(parts) > 2L && .screen_width(paste(parts, collapse = "  ")) > width)
    parts <- parts[-(length(parts) - 1L)]
  if (length(parts) == 2L && .screen_width(paste(parts, collapse = "  ")) > width) {
    ending <- .screen_width(parts[[2L]])
    if (width >= ending + 4L) parts[[1L]] <- .screen_fit(parts[[1L]], width - ending - 2L)
    else parts <- parts[[2L]]
  }
  out <- if (length(parts)) paste(parts, collapse = "  ") else ""
  # Clip by terminal cells, retaining only complete graphemes and safe styles.
  out <- .screen_fit(out, width)
  stopifnot("footer must be one bounded string" = is.character(out) && length(out) == 1L &&
              .screen_width(out) <= width)
  out
}
