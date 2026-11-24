# Reproducible compositor/editor CPU and emission-count benchmark.
# Run against an INSTALLED package, optionally selecting an isolated library:
#   PRISM_BENCH_LIBRARY=/path/to/library Rscript tools/benchmark-render.R
#   PRISM_BENCH_CSV=tools/validation/performance.csv Rscript tools/benchmark-render.R
# Terminal writes are instrumented: these timings exclude terminal/emulator I/O.
# Compositor operations exclude editor geometry/highlighting; separate editor
# operations include replacement, geometry, highlighting, clipping and hit maps.
# The fixture column distinguishes the existing ASCII cases from a 2,000-line
# Unicode expression with nested calls, quotes, indexing and a combining mark.

library_path <- Sys.getenv("PRISM_BENCH_LIBRARY", unset = "")
if (nzchar(library_path)) .libPaths(c(library_path, .libPaths()))
Sys.setenv(PRISMREPL_CONFIG_FILE = tempfile("prismrepl-benchmark-"))
namespace <- asNamespace("prismrepl")
fetch <- function(name) get(name, envir = namespace, inherits = FALSE)
version <- as.character(utils::packageVersion("prismrepl"))
size <- c(width = 132L, height = 40L)
config <- fetch("repl_config")(theme = "midnight", color = TRUE,
  color_depth = 16777216L, sidebar = TRUE, editor_height = 8L)

#' Construct a Deterministic Transcript Without Measuring Fixture Setup
#' @param entries Number of retained one-line entries.
#' @return Active screen model with fixed sidebar and status metadata.
make_screen <- function(entries) {
  stopifnot("entries must be a positive integer" = entries >= 1L && entries == as.integer(entries))
  screen <- fetch("screen_state")(max_lines = max(4000L, entries))
  screen$active <- TRUE
  screen$status <- "cwd: /project | git: main | objects: 24 | memory: 8.2 MiB"
  screen$sidebar <- list(WORKSPACE = c("iris data.frame 150 x 5", "sample numeric 10000",
    "model lm 4 coefficients"), PACKAGES = c("base", "stats", "utils", "prismrepl"), DIRECTORY = "/project")
  screen$entries <- lapply(seq_len(entries), function(i) list(
    lines = sprintf("Result %05d: a representative output line with several values, 1.23 45.67 89.01", i),
    role = "output", ansi = FALSE, id = as.integer(i)))
  screen$next_id <- as.integer(entries)
  screen$revision <- 1L
  stopifnot("fixture must contain the requested transcript" = length(screen$entries) == entries)
  screen
}

probe <- new.env(parent = namespace)
reset_counts <- function() {
  probe$native <- probe$cat_calls <- probe$pieces <- probe$bytes <- probe$flushes <- 0
}
reset_counts()
probe$cat <- function(..., sep = " ", file = "", fill = FALSE, labels = NULL, append = FALSE) {
  args <- list(...)
  probe$cat_calls <- probe$cat_calls + 1L
  probe$pieces <- probe$pieces + length(args)
  probe$bytes <- probe$bytes + sum(nchar(as.character(unlist(args)), type = "bytes")) +
    max(0L, length(args) - 1L) * nchar(sep, type = "bytes")
  invisible(NULL)
}
probe$flush.console <- function() { probe$flushes <- probe$flushes + 1L; invisible(NULL) }
probe$term_write <- function(text, synchronize = FALSE, cursor = NULL) {
  probe$native <- probe$native + 1L
  probe$pieces <- probe$pieces + 1L
  probe$bytes <- probe$bytes + nchar(text, type = "bytes")
  invisible(NULL)
}
render <- fetch("screen_render")
environment(render) <- probe

#' Measure Repeated Operations with Counts Separate from Timing
#' @param operation Label identifying the measured work.
#' @param iterations Positive repeat count.
#' @param execute Function called with each iteration number.
#' @param entries,logical_lines Fixture dimensions.
#' @param fixture Source-text fixture: ascii or unicode.
#' @return One row of timings, counts and provenance.
measure <- function(operation, iterations, execute, entries = NA_integer_,
                    logical_lines = NA_integer_, fixture = "ascii") {
  stopifnot("operation must be nonempty scalar text" = is.character(operation) &&
              length(operation) == 1L && !is.na(operation) && nzchar(operation),
            "iteration count must be a positive integer" = is.numeric(iterations) &&
              length(iterations) == 1L && is.finite(iterations) &&
              iterations >= 1L && iterations == as.integer(iterations),
            "execute must be a function" = is.function(execute),
            "fixture must be ascii or unicode" = is.character(fixture) &&
              length(fixture) == 1L && !is.na(fixture) && fixture %in% c("ascii", "unicode"))
  reset_counts()
  elapsed <- system.time(invisible(lapply(seq_len(iterations), execute)))[["elapsed"]]
  out <- data.frame(version = version, operation = operation, fixture = fixture, entries = entries,
    logical_lines = logical_lines, width = size[["width"]], height = size[["height"]],
    iterations = iterations, elapsed_seconds = elapsed, mean_ms = elapsed * 1000 / iterations,
    native_write_calls = probe$native, r_cat_calls = probe$cat_calls,
    r_flush_calls = probe$flushes, text_pieces = probe$pieces, emitted_bytes = probe$bytes,
    r_version = R.version.string, platform = R.version$platform,
    writer_instrumented = TRUE, stringsAsFactors = FALSE)
  stopifnot("measurement must have a finite nonnegative duration" = nrow(out) == 1L &&
              is.finite(out$elapsed_seconds) && out$elapsed_seconds >= 0,
            "measurement must retain its fixture and iteration count" =
              identical(out$fixture, fixture) && out$iterations == iterations,
            "emission counters must be finite nonnegative integers" =
              all(vapply(out[c("native_write_calls", "r_cat_calls", "r_flush_calls",
                "text_pieces", "emitted_bytes")], function(value)
                  is.finite(value) && value >= 0 && value == trunc(value), logical(1L))))
  out
}

#' Measure Compositor Work on Fresh and Unchanged Transcript Content
#' @param entries Retained transcript entries.
#' @return Four operation measurements.
measure_render <- function(entries) {
  stopifnot("entries must be positive" = entries >= 1L)
  editor <- list(lines = "r> mean(iris$Sepal.Length)", cursor_row = 0L, cursor_column = 27L)
  invisible(render(make_screen(40L), editor, config, size))
  screens <- lapply(seq_len(5L), function(i) make_screen(entries))
  first <- measure("compositor_first_frame", 5L, function(i) {
    render(screens[[i]], editor, config, size)
    NULL
  }, entries)
  screen <- screens[[5L]]
  cursor <- measure("compositor_cursor", 100L, function(i) {
    frame <- editor
    frame$cursor_column <- 3L + i %% 20L
    render(screen, frame, config, size)
    NULL
  }, entries)
  edit <- measure("compositor_changed_editor_row", 50L, function(i) {
    frame <- editor
    frame$lines <- paste0("r> value + ", i)
    frame$cursor_column <- nchar(frame$lines)
    render(screen, frame, config, size)
    NULL
  }, entries)
  scroll <- measure("compositor_scroll_one_line", 50L, function(i) {
    screen$scroll <- i
    render(screen, editor, config, size)
    NULL
  }, entries)
  out <- rbind(first, cursor, edit, scroll)
  stopifnot("all compositor operations must be measured" = nrow(out) == 4L)
  out
}

#' Measure the Actual Editor Frame Builder on a Small or Large Expression
#' @param logical_lines Number of logical lines in the expression.
#' @param fixture Source-text fixture: ascii or unicode.
#' @return Cursor and insertion measurements including highlighting/geometry.
measure_editor <- function(logical_lines, fixture = "ascii") {
  stopifnot("logical_lines must be a positive integer" = is.numeric(logical_lines) &&
              length(logical_lines) == 1L && is.finite(logical_lines) &&
              logical_lines >= 1L && logical_lines == as.integer(logical_lines),
            "fixture must be ascii or unicode" = is.character(fixture) &&
              length(fixture) == 1L && !is.na(fixture) && fixture %in% c("ascii", "unicode"))
  cfg <- config
  cfg$screen <- make_screen(40L)
  cfg$highlight_cache <- new.env(parent = emptyenv())
  line <- switch(fixture,
    ascii = "alpha <- mean(iris$Sepal.Length) + beta",
    unicode = '\u03b1 <- f(list("e\u0301", x[[1]]), \u03b2)')
  source <- paste(rep(line, logical_lines), collapse = "\n")
  stopifnot("fixture and measured insertions must fit the configured input limits" =
              logical_lines <= cfg$max_lines && nchar(source, type = "bytes") + 20L <= cfg$max_paste)
  state <- fetch(".editor_state")(source)
  display <- fetch(".editor_display_frame")
  invisible(display(state, size, "r> ", "+  ", character(), cfg))
  cursor <- measure("editor_cursor_geometry_highlight", 100L, function(i) {
    state$cursor <- length(state$chars) - i %% 20L
    display(state, size, "r> ", "+  ", character(), cfg)
    NULL
  }, logical_lines = logical_lines, fixture = fixture)
  state$cursor <- length(state$chars)
  replacement <- if (exists(".editor_replace_checked", namespace, inherits = FALSE))
    function(x) fetch(".editor_replace_checked")(x, cfg, text = "x") else
      function(x) fetch(".editor_replace")(x, text = "x")
  edit <- measure("editor_insert_geometry_highlight", 20L, function(i) {
    state <<- replacement(state)
    display(state, size, "r> ", "+  ", character(), cfg)
    NULL
  }, logical_lines = logical_lines, fixture = fixture)
  out <- rbind(cursor, edit)
  stopifnot("both editor operations must be measured for the requested fixture" =
              nrow(out) == 2L && all(out$fixture == fixture) &&
              all(out$logical_lines == logical_lines))
  out
}

results <- do.call(rbind, c(lapply(c(40L, 4000L), measure_render),
  lapply(c(1L, 2000L), measure_editor), list(measure_editor(2000L, fixture = "unicode"))))
print(results[c("version", "operation", "fixture", "entries", "logical_lines", "iterations", "elapsed_seconds",
                "mean_ms", "native_write_calls", "r_cat_calls", "r_flush_calls", "emitted_bytes")], row.names = FALSE)
output <- Sys.getenv("PRISM_BENCH_CSV", unset = "")
if (nzchar(output)) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(results, output, row.names = FALSE)
}
