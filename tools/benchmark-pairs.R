# Reproduce cursor-frame and edited-buffer costs with pair focus on and off.
# Run from the repository root against a compatible installed native package:
# PRISM_BENCH_LIBRARY=/path/to/library Rscript tools/benchmark-pairs.R
# Current R sources are loaded into a private environment above that namespace.
# No terminal output, network requests, preferences, or user history are used.

library_path <- Sys.getenv("PRISM_BENCH_LIBRARY", unset = "")
if (nzchar(library_path)) .libPaths(c(library_path, .libPaths()))
namespace <- asNamespace("prismrepl")
implementation <- new.env(parent = namespace)
invisible(lapply(list.files("R", pattern = "[.]R$", full.names = TRUE),
  sys.source, envir = implementation))
fetch <- function(name) get(name, envir = implementation, inherits = TRUE)
size <- c(width = 132L, height = 40L)

#' Create an Editable Fixture with Cursor Positions on Its Last Line
#' @param code One complete buffer.
#' @return Source, state, alternating edits and nearby delimiter cursors.
make_fixture <- function(code) {
  stopifnot("code must be one nonempty string" = is.character(code) && length(code) == 1L && nzchar(code))
  state <- fetch(".editor_state")(code)
  last_line <- max(c(0L, which(state$chars == "\n")))
  cursors <- which(state$chars %in% c("(", ")", "[", "]", '"') &
    seq_along(state$chars) > last_line) - 1L
  edits <- lapply(c(" ", "  "), function(suffix) {
    edited <- fetch(".editor_state")(paste0(code, suffix))
    edited$cursor <- utils::tail(cursors, 1L)
    edited
  })
  out <- list(source = code, state = state, cursors = cursors, edits = edits)
  stopifnot("fixture must have delimiter cursor positions" = length(out$cursors) > 0L)
  out
}

fixtures <- list(
  short = make_fixture('fit <- stats::lm(y ~ x, data = d)'),
  unicode_2000_lines = make_fixture(paste(rep(
    '\u03b1 <- f("e\u0301\U0001f469\u200d\U0001f4bb", x[[1]], "\u754c")', 2000L), collapse = "\n")))

#' Measure a Bounded Batch of Actual Editor Frames
#' @param fixture Prepared editable fixture.
#' @param mode Fullscreen or inline rendering.
#' @param enabled Whether pair focus is enabled.
#' @param operation Cursor motion or changed-buffer rendering.
#' @param iterations Number of frames in each batch.
#' @param repeats Number of measured batches.
#' @return A data frame containing elapsed time per frame.
measure_frames <- function(fixture, mode, enabled, operation, iterations, repeats = 5L) {
  stopifnot("fixture must contain source and state" = is.list(fixture) && is.list(fixture$state),
            "mode must select an editor layout" = mode %in% c("fullscreen", "inline"),
            "enabled must be logical" = is.logical(enabled) && length(enabled) == 1L && !is.na(enabled),
            "operation must select cursor or edited frames" = operation %in% c("cursor_frame", "edited_frame"),
            "iterations and repeats must be positive integers" = iterations >= 1L &&
              iterations == as.integer(iterations) && repeats >= 1L && repeats == as.integer(repeats))
  cfg <- fetch("repl_config")(settings_file = NULL, persist = FALSE, color = TRUE,
    color_depth = 16777216L, theme = "midnight", ui = mode, sidebar = FALSE,
    match_pairs = enabled)
  cfg$highlight_cache <- new.env(parent = emptyenv())
  if (mode == "fullscreen") {
    cfg$screen <- fetch("screen_state")()
    cfg$screen$active <- TRUE
  }
  frame <- fetch(".editor_display_frame")
  state <- fixture$state
  state$cursor <- utils::tail(fixture$cursors, 1L)
  invisible(frame(state, size, "r> ", "+  ", character(), cfg))
  run_batch <- function() {
    for (i in seq_len(iterations)) {
      if (operation == "edited_frame") state <- fixture$edits[[1L + i %% 2L]]
      else state$cursor <- fixture$cursors[[1L + i %% length(fixture$cursors)]]
      value <- frame(state, size, "r> ", "+  ", character(), cfg)
    }
    invisible(value)
  }
  invisible(run_batch())
  timings <- vapply(seq_len(repeats), function(i) {
    invisible(gc())
    1000 * system.time(run_batch())[["elapsed"]] / iterations
  }, numeric(1))
  out <- data.frame(mode = mode, operation = operation, match_pairs = enabled,
    iterations = iterations, repeats = repeats, median_ms = median(timings),
    min_ms = min(timings), max_ms = max(timings),
    source_chars = nchar(fixture$source, type = "chars"),
    source_bytes = nchar(fixture$source, type = "bytes"),
    pair_cache_bytes = as.numeric(object.size(cfg$highlight_cache$matching_pairs)))
  stopifnot("timings must be finite and nonnegative" = all(is.finite(timings)) && all(timings >= 0))
  out
}

rows <- lapply(names(fixtures), function(name) {
  cases <- expand.grid(mode = c("fullscreen", "inline"), enabled = c(FALSE, TRUE),
    operation = c("cursor_frame", "edited_frame"), stringsAsFactors = FALSE)
  do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
    case <- cases[i, ]
    iterations <- if (case$operation == "cursor_frame") 100L else 12L
    result <- cbind(fixture = name, measure_frames(fixtures[[name]], case$mode,
      case$enabled, case$operation, iterations), stringsAsFactors = FALSE)
    cat(sprintf("%s %s %s pairs=%s: %.3f ms\n", name, case$mode,
      case$operation, case$enabled, result$median_ms))
    result
  }))
})
results <- do.call(rbind, rows)

# Isolate index construction from geometry and syntax highlighting. Each batch
# drops exactly the current pair-index cache, then reconstructs the same buffer.
index_rows <- lapply(names(fixtures), function(name) {
  fixture <- fixtures[[name]]
  cfg <- fetch("repl_config")(settings_file = NULL, persist = FALSE, color = TRUE,
    theme = "midnight", match_pairs = TRUE)
  cfg$highlight_cache <- new.env(parent = emptyenv())
  state <- fixture$state
  state$cursor <- utils::tail(fixture$cursors, 1L)
  focus <- fetch(".editor_pair_focus")
  invisible(focus(state, fixture$source, cfg))
  iterations <- if (name == "short") 100L else 5L
  timings <- replicate(10L, {
    1000 * system.time(for (i in seq_len(iterations)) {
      cfg$highlight_cache$matching_pairs <- NULL
      focus(state, fixture$source, cfg)
    })[["elapsed"]] / iterations
  })
  data.frame(fixture = name, mode = "independent", operation = "pair_index", match_pairs = TRUE,
    iterations = iterations, repeats = 10L, median_ms = median(timings), min_ms = min(timings),
    max_ms = max(timings), source_chars = nchar(fixture$source, type = "chars"),
    source_bytes = nchar(fixture$source, type = "bytes"),
    pair_cache_bytes = as.numeric(object.size(cfg$highlight_cache$matching_pairs)))
})
results <- rbind(results, do.call(rbind, index_rows))
directory <- "tools/validation"
if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
utils::write.csv(results, file.path(directory, "pair-render-benchmark.csv"), row.names = FALSE)
report <- c(paste("Runtime:", R.version.string),
  paste("Platform:", R.version$platform), paste("UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "Current source R code loaded privately above an installed native namespace.",
  "Timings exclude terminal/emulator I/O and use warmed code paths.",
  "Cursor frames use a shared geometry/color/pair cache and move between delimiters.",
  "Edited frames alternate changed buffers, rebuilding geometry, colors and pair indexes.",
  "Pair-index rows isolate the index rebuild after source changes.",
  "Five elapsed-time batches per frame case; values are milliseconds per frame.",
  capture.output(print(results, row.names = FALSE, digits = 5L)))
writeLines(report, file.path(directory, "pair-render-benchmark.log"), useBytes = TRUE)
cat(paste(report, collapse = "\n"), "\n")
