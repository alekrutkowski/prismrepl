#' Read Current R-Process Resident Memory Without Forcing Garbage Collection
#'
#' Includes resident pages used by R, loaded libraries and native allocations.
#' Shared resident pages may be included. This is an operating-system estimate,
#' not virtual memory, a lifetime peak, or the sum of workspace object sizes.
#' The native counter does not start another process or inspect R bindings.
#'
#' @return One numeric byte count, or NA_real_ if unavailable.
#' @noRd
.process_memory <- function() {
  out <- .Call(C_process_memory)
  stopifnot("process memory must be one nonnegative byte count or unavailable" =
              is.double(out) && length(out) == 1L && !is.nan(out) &&
              (is.na(out) || (is.finite(out) && out >= 0)))
  out
}

#' Format Current R-Process RAM for the Status and Workspace Pane
#' @param bytes One nonnegative resident byte count, or NA for unavailable.
#' @return A single status label using MiB or GiB, or an unavailable label.
#' @noRd
.process_memory_label <- function(bytes = .process_memory()) {
  stopifnot("bytes must be one nonnegative number or NA" =
              (is.double(bytes) || is.integer(bytes)) && length(bytes) == 1L &&
              !is.nan(bytes) && (is.na(bytes) || (is.finite(bytes) && bytes >= 0)))
  out <- if (is.na(bytes)) "R RAM: unavailable" else {
    gib <- bytes >= 1024^3
    sprintf("R RAM: %.1f %s", bytes / if (gib) 1024^3 else 1024^2,
      if (gib) "GiB" else "MiB")
  }
  stopifnot("process memory label must be one nonempty string" = .is_text(out))
  out
}
