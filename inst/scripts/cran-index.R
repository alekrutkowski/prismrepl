# Background data-only CRAN name download. The child writes only inside its
# parent's owned job directory. Only the parent may publish a persistent cache.

#' Download a Repository Index into an Owned Job Directory
#' @param task_file A DCF file containing Repository, Result and Done.
#' @return TRUE for success, FALSE for a reported download failure.
run_cran_index <- function(task_file) {
  stopifnot("one existing task file is required" = is.character(task_file) &&
    length(task_file) == 1L && !is.na(task_file) && file.exists(task_file))
  task <- as.list(read.dcf(task_file, all = TRUE)[1L, ])
  stopifnot("worker task must contain its repository and paths" =
    all(c("Repository", "Result", "Done") %in% names(task)))
  directory <- normalizePath(dirname(task_file), winslash = "/", mustWork = TRUE)
  stopifnot("worker output must stay in the owned job directory" =
    all(vapply(task[c("Result", "Done")], function(path)
      identical(normalizePath(dirname(path), winslash = "/", mustWork = TRUE), directory), logical(1L))))
  staged <- file.path(directory, "index-staged.txt")
  status_staged <- file.path(directory, "status-staged.txt")
  on.exit(unlink(c(staged, status_staged)), add = TRUE)
  status <- tryCatch({
    options(timeout = 30, warn = 1)
    available <- utils::available.packages(repos = task$Repository, type = "source", filters = list())
    packages <- sort(unique(available[, "Package"]))
    stopifnot("repository returned no package names" = length(packages) > 0L,
      "repository returned invalid package names" = !anyNA(packages) &&
        all(grepl("^[A-Za-z][A-Za-z0-9.]*$", packages)))
    writeLines(c(paste0("Repository: ", task$Repository), packages), staged, useBytes = TRUE)
    if (!file.rename(staged, task$Result)) stop("Cannot stage the CRAN index.", call. = FALSE)
    "ok"
  }, error = function(e) paste0("failed: ", conditionMessage(e)))
  writeLines(substr(gsub("[\r\n]", " ", status), 1L, 2000L), status_staged, useBytes = TRUE)
  if (!file.rename(status_staged, task$Done)) stop("Cannot report the CRAN index result.", call. = FALSE)
  out <- identical(status, "ok")
  stopifnot("worker result must be logical" = is.logical(out) && length(out) == 1L && !is.na(out))
  out
}

args <- commandArgs(trailingOnly = TRUE)
stopifnot("one task file is required" = length(args) == 1L)
quit(save = "no", status = if (run_cran_index(args[[1L]])) 0L else 1L)
