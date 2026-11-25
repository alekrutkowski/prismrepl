# Binding-inspection and caller timings against an installed prismrepl.
# Run the same script with R 4.5.0 and a newer R, selecting each installation:
#   PRISM_BENCH_LIBRARY=/path/to/library Rscript tools/benchmark-bindings.R
#   PRISM_BENCH_CSV=/path/to/results.csv Rscript tools/benchmark-bindings.R
# Each result is the median of five batches after warm-up. These measurements
# exclude terminal I/O and are observations, not portable performance promises.

library_path <- Sys.getenv("PRISM_BENCH_LIBRARY", unset = "")
if (nzchar(library_path)) .libPaths(c(library_path, .libPaths()))
namespace <- asNamespace("prismrepl")
fetch <- function(name) get(name, envir = namespace, inherits = FALSE)

#' Construct a Representative Workspace Without Evaluating Special Bindings
#' @param count Number of ordinary objects, at least two.
#' @return Environment containing ordinary, active and lazy bindings.
make_workspace <- function(count) {
  stopifnot("count must be an integer of at least two" = is.numeric(count) &&
              length(count) == 1L && is.finite(count) && count >= 2L && count == as.integer(count))
  values <- lapply(seq_len(count), function(i) switch(as.character(i %% 4L),
    "0" = data.frame(value = seq_len(50L), label = rep("sample", 50L)),
    "1" = seq_len(100L), "2" = list(value = 1:10, label = "sample"),
    "3" = function(value, na.rm = FALSE) value))
  workspace <- list2env(stats::setNames(values, sprintf("object%04d", seq_len(count))),
                        parent = baseenv())
  makeActiveBinding("active", function() stop("active binding was evaluated"), workspace)
  delayedAssign("lazy", stop("lazy binding was evaluated"), assign.env = workspace)
  stopifnot("fixture must retain every binding" = length(ls(workspace)) == count + 2L)
  workspace
}

#' Measure an Operation with Warm-Up and Repeated Batches
#' @param label Scalar operation label.
#' @param iterations Positive number of calls per batch.
#' @param operation Zero-argument function to measure.
#' @return One row containing median, minimum and maximum milliseconds per call.
measure <- function(label, iterations, operation) {
  stopifnot("label must be nonempty text" = is.character(label) && length(label) == 1L &&
              !is.na(label) && nzchar(label),
            "iterations must be a positive integer" = is.numeric(iterations) &&
              length(iterations) == 1L && is.finite(iterations) && iterations >= 1L &&
              iterations == as.integer(iterations),
            "operation must be a function" = is.function(operation))
  invisible(lapply(seq_len(10L), function(i) operation()))
  elapsed <- vapply(seq_len(5L), function(batch) {
    gc()
    system.time(invisible(lapply(seq_len(iterations), function(i) {
      operation()
      NULL
    })), gcFirst = FALSE)[["elapsed"]] * 1000 / iterations
  }, numeric(1L))
  result <- data.frame(operation = label, median_ms = stats::median(elapsed),
    min_ms = min(elapsed), max_ms = max(elapsed), iterations = iterations, batches = 5L,
    r_version = as.character(getRversion()), package_version =
      as.character(utils::packageVersion("prismrepl")), platform = R.version$platform)
  stopifnot("measurements must be finite and nonnegative" = all(is.finite(elapsed)) &&
              all(elapsed >= 0), "measurement must identify one operation" = nrow(result) == 1L)
  result
}

workspace <- make_workspace(100L)
large <- make_workspace(1000L)
large_names <- ls(large)
config <- fetch("repl_config")(color = FALSE, settings_file = NULL,
  status = c("directory", "objects", "memory", "packages"))
bindings <- fetch("binding_info")
inventory <- fetch("workspace_objects")
complete <- fetch("complete_r")
explore <- function() fetch(".explorer_load")(fetch(".explorer_state")(
  workspace, limit = 50L)$stack[[1L]])
snapshot <- function() {
  objects <- inventory(workspace)
  fetch(".session_status")(workspace, config, objects)
  fetch(".session_sidebar")(workspace, config, objects)
  invisible(NULL)
}

results <- do.call(rbind, list(
  measure("binding_one_ordinary", 20000L, function() bindings(workspace, "object0001")),
  measure("binding_1002_mixed", 1000L, function() bindings(large, large_names)),
  measure("complete_list_member", 1000L, function() complete("object0002$v", envir = workspace)),
  measure("complete_function_argument", 1000L, function() complete("object0003(", envir = workspace)),
  measure("workspace_102_objects_with_sizes", 20L, function() inventory(workspace)),
  measure("session_snapshot_102_objects", 20L, snapshot),
  measure("explorer_first_50_objects", 20L, explore)))

stopifnot("active and lazy bindings must remain unevaluated" = identical(
  bindings(workspace, c("active", "lazy")), c(active = "active", lazy = "promise")))
print(results, row.names = FALSE)
output <- Sys.getenv("PRISM_BENCH_CSV", unset = "")
if (nzchar(output)) utils::write.csv(results, output, row.names = FALSE)
