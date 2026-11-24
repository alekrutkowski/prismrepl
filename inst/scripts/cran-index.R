# Background data-only CRAN name cache. Invoked only after an explicit Tab or
# :install --refresh request. This worker never installs or attaches a package.
args <- commandArgs(trailingOnly = TRUE)
stopifnot("one task file is required" = length(args) == 1L)
task <- as.list(read.dcf(args[[1L]], all = TRUE)[1L, ])
stopifnot("worker task must contain its paths" =
  all(c("Repository", "Destination", "Done") %in% names(task)))
status <- tryCatch({
  options(timeout = 30, warn = 1)
  available <- utils::available.packages(repos = task$Repository, type = "source", filters = list())
  packages <- sort(unique(available[, "Package"]))
  stopifnot("repository returned no package names" = length(packages) > 0L,
            "repository returned invalid package names" = all(grepl("^[A-Za-z][A-Za-z0-9.]*$", packages)))
  temporary <- tempfile("cran-index-", tmpdir = dirname(task$Destination))
  writeLines(c(paste0("Repository: ", task$Repository), packages), temporary, useBytes = TRUE)
  if (!file.rename(temporary, task$Destination)) {
    if (!file.copy(temporary, task$Destination, overwrite = TRUE)) stop("Cannot save the CRAN index.")
    unlink(temporary)
  }
  "ok"
}, error = function(e) paste0("failed: ", conditionMessage(e)))
writeLines(status, task$Done, useBytes = TRUE)
