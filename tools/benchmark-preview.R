# Run with the candidate package installed: Rscript tools/benchmark-preview.R
# This measures one mixed-type fixture, not every object or terminal repaint.
stopifnot("prismrepl must be installed" = requireNamespace("prismrepl", quietly = TRUE))
ns <- asNamespace("prismrepl")
n <- 100000L
value <- data.frame(id = seq_len(n),
  group = factor(rep(c("alpha", "beta", "gamma", "delta"), length.out = n)),
  date = as.Date("2026-01-01") + seq_len(n),
  text = rep(c("caf\u00e9", "\u6771\u4eac", "ordinary"), length.out = n),
  measurement = seq_len(n) / 7,
  long = rep(paste0(strrep("long value ", 100L), "end"), n))
value$text[c(245L, 90001L)] <- "needle caf\u00e9"
node <- get(".explorer_node", ns)(value, "table", 200L)
node$view <- "preview"
node$query <- "needle"
load <- get(".explorer_preview_load", ns)
content <- get(".explorer_content", ns)
first <- system.time(selected <- load(node))[["elapsed"]]
stopifnot("the full scan must find original rows beyond the first batch" =
  identical(selected$indices, c(245L, 90001L)))
cached <- system.time(invisible(lapply(seq_len(1000L), function(i) load(node))))[["elapsed"]]
invisible(content(node, 120L))
repeated <- system.time(invisible(lapply(seq_len(1000L), function(i) content(node, 120L))))[["elapsed"]]
node$query <- "needle caf"
extension <- system.time(load(node))[["elapsed"]]
cat("prismrepl", as.character(utils::packageVersion("prismrepl")),
  "; R", as.character(getRversion()), ";", R.version$platform, "\n")
cat("Fixture: 100000 rows, 6 columns, factor/Date/Unicode/numeric/repeated 1103-character text.\n")
cat("First full scan:", first, "seconds\n")
cat("1000 cached selection loads:", cached, "seconds total\n")
cat("1000 cached content lookups:", repeated, "seconds total\n")
cat("Query extension:", extension, "seconds\n")
cat("These measurements exclude terminal I/O and do not predict arbitrary text distributions.\n")
