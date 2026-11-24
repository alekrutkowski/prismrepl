# Optional development measurements. Run after installing the package.
# These observations are not test thresholds or cross-machine guarantees.
library(prismrepl)
ns <- asNamespace("prismrepl")
measure <- function(label, repetitions, operation) {
  elapsed <- system.time(invisible(lapply(seq_len(repetitions), function(i) operation())))[["elapsed"]]
  data.frame(operation = label, repetitions = repetitions,
    milliseconds = 1000 * elapsed / repetitions, stringsAsFactors = FALSE)
}
config <- repl_config(color = TRUE)
state_small <- ns$.editor_state(paste(rep("x", 1000L), collapse = ""))
state_large <- ns$.editor_state(paste(rep("x <- mean(1:100) # sample", 4000L), collapse = "\n"))
event <- list(type = "text", key = "", text = "z", modifiers = character(), truncated = FALSE)
invisible(help_text("mean", "base"))
out <- do.call(rbind, list(
  measure("Insert in 1000-character buffer", 100L,
    function() ns$.editor_step(state_small, event, character(), .GlobalEnv, config)),
  measure("Render long buffer viewport", 20L,
    function() ns$.editor_frame(state_large, 100L, 30L, "r> ", "+  ", character(), config)),
  measure("Cached base::mean documentation", 20L,
    function() help_text("mean", "base")),
  measure("Preview a data frame", 100L,
    function() preview_object(iris, rows = 12L))))
print(out, row.names = FALSE)
cat("\n", R.version.string, "\n", R.version$platform, "\n", sep = "")
