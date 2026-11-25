# Run with the desired R installation:
# Rscript --no-init-file --no-site-file tools/check-package.R SOURCE.tar.gz [OUTPUT]
# The standalone prismrepl-check.R download includes both companion helpers.

# Isolate the process boundary for offline orchestration tests.
.prism_check_run <- function(...) processx::run(...)

#' Write a Small Public Audit of the Check Environment
#' @param audit Check evidence without complete profiles or environment contents.
#' @param directory Existing output directory.
#' @return The text audit path, invisibly.
prism_check_write_audit <- function(audit, directory) {
  stopifnot("audit must be a list" = is.list(audit),
    "audit directory must exist" = is.character(directory) && length(directory) == 1L && dir.exists(directory))
  path <- file.path(directory, "preflight.txt")
  summary <- c("prismrepl source check", paste("State:", audit$state),
    paste("Archive:", audit$archive), paste("SHA-256:", audit$sha256),
    paste("R:", audit$r_version), paste("Platform:", audit$platform),
    "", "This driver verifies UTC before starting the standard R CMD check.",
    "It replaces only R's duplicate external-time lookup for this child process.",
    "Compiler flags are unchanged; compiler and future-file checks remain enabled.")
  if (!is.null(audit$profile)) summary <- c(summary, "",
    paste("Original check profile:", if (nzchar(audit$profile$path)) audit$profile$path else "<disabled>"),
    paste("Original profile existed:", audit$profile$exists),
    paste("Original profile fingerprint (MD5):", audit$profile$md5))
  if (!is.null(audit$leaf_origin)) summary <- c(summary, "",
    paste("Leaf-frame flag in active C settings:", audit$leaf_origin[["active"]]),
    paste("Leaf-frame flag inherited from R build:", audit$leaf_origin[["inherited"]]),
    paste("Added inherited flag to checker-known flags:", audit$known_flag_added))
  if (!is.null(audit$clock)) {
    summary <- c(summary, "", paste("Independent UTC verification:", audit$clock$verified),
      paste("Verified at:", audit$clock$verified_at),
      paste("Maximum local-clock difference:", audit$clock$tolerance_seconds, "seconds"),
      "HTTPS certificate verification is enabled. Sources must agree.")
    if (is.data.frame(audit$clock$observations)) summary <- c(summary,
      capture.output(print(audit$clock$observations, row.names = FALSE)))
  }
  if (!is.null(audit$effective_settings)) summary <- c(summary, "",
    "Effective settings verified in a fresh matching R process:",
    paste(names(audit$effective_settings), audit$effective_settings, sep = "="))
  if (!is.null(audit$error)) summary <- c(summary, "", paste("Failure:", audit$error))
  if (!is.null(audit$check_status)) summary <- c(summary, "",
    paste("Checker exit status:", audit$check_status),
    paste("Driver exit status:", audit$status),
    paste("Timestamp section OK:", audit$timestamp_ok),
    paste("Compilation-flags section OK:", audit$compilation_ok),
    paste("Source archive unchanged:", audit$archive_unchanged), audit$check_summary)
  writeLines(enc2utf8(summary), path, useBytes = TRUE)
  saveRDS(audit, file.path(directory, "preflight.rds"))
  stopifnot("the audit must be written" = file.exists(path))
  invisible(path)
}

#' Check the Exact Source Archive with a Verified Local Environment
#'
#' Uses a temporary private copy of the effective check profile. No R
#' installation, user configuration, compiler command or source timestamp is
#' changed. The old external-clock probe is replaced only after independent
#' HTTPS sources verify UTC. R's future-file check still runs with five-minute
#' leeway. The only added known flag must originate in the selected R build.
#'
#' @param archive Existing prismrepl source tarball created by R CMD build.
#' @param output New output directory, or NULL for a unique directory in the
#'   current working directory. An existing directory is never overwritten.
#' @param clock_timeout Per-source clock-request timeout, in seconds.
#' @return A list of status, output directory, check log and audit paths.
prism_check_package <- function(archive, output = NULL, clock_timeout = 10L) {
  stopifnot("R 4.5.0 or newer is required" = getRversion() >= "4.5.0",
    "archive must be one existing file" = is.character(archive) && length(archive) == 1L &&
      !is.na(archive) && file.exists(path.expand(archive)) && !dir.exists(path.expand(archive)),
    "use the unsuffixed prismrepl source tarball" = grepl(
      "^prismrepl_[0-9]+([.-][0-9]+)*[.]tar[.]gz$", basename(archive)),
    "output must be absent or one nonempty path" = is.null(output) ||
      (is.character(output) && length(output) == 1L && !is.na(output) && nzchar(output)),
    "clock timeout must be between one and ten seconds" = is.numeric(clock_timeout) &&
      length(clock_timeout) == 1L && is.finite(clock_timeout) && clock_timeout >= 1 && clock_timeout <= 10,
    "processx is required and is already a prismrepl dependency" = requireNamespace("processx", quietly = TRUE))
  archive <- normalizePath(path.expand(archive), winslash = "/", mustWork = TRUE)
  if (is.null(output)) output <- tempfile(paste0("prismrepl-check-R", getRversion(), "-"), tmpdir = getwd())
  output <- path.expand(output)
  stopifnot("choose a new output directory so previous results remain intact" = !file.exists(output),
    "output directory must be created" = dir.create(output, recursive = TRUE))
  output <- normalizePath(output, winslash = "/", mustWork = TRUE)
  workdir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  staging <- tempfile("prism-check-private-")
  stopifnot("private profile directory must be created" = dir.create(staging, mode = "0700"))
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  audit <- list(state = "preflight", archive = archive,
    sha256 = unname(tools::sha256sum(archive)), r_version = as.character(getRversion()),
    platform = R.version$platform)
  on.exit(prism_check_write_audit(audit, output), add = TRUE)
  cat("Output:", output, "\n")
  cat("Source SHA-256:", audit$sha256, "\n")
  result <- tryCatch({
    context <- prism_check_context(r, staging, workdir = workdir)
    audit$profile <- context$profile[c("path", "exists", "md5")]
    audit$leaf_origin <- prism_check_leaf_origin(context)
    cat("Verifying UTC against independent HTTPS sources...\n")
    audit$clock <- prism_check_clock(timeout = clock_timeout, tolerance = 60)
    stopifnot("fresh independent time verification must succeed" = isTRUE(audit$clock$verified))
    prepared <- prism_check_make_profile(context, staging, clock_verified = TRUE)
    effective <- prism_check_verify_profile(context, prepared, r, staging, workdir = workdir)
    audit$known_flag_added <- prepared$known_flag_added
    fields <- c("_R_CHECK_COMPILATION_FLAGS_", "_R_CHECK_SYSTEM_CLOCK_",
      "_R_CHECK_FUTURE_FILE_TIMESTAMPS_", "_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_")
    audit$effective_settings <- effective$settings[fields]
    audit$state <- "checking"
    prism_check_write_audit(audit, output)
    cat("UTC verified. Compiler flags are unchanged.\n")
    if (isTRUE(prepared$leaf_origin[["inherited"]])) cat(
      "The selected R build supplies -mno-omit-leaf-frame-pointer; it is recognized as inherited.\n")
    if (isTRUE(prepared$leaf_origin[["active"]]) && !isTRUE(prepared$leaf_origin[["inherited"]])) cat(
      "The leaf-frame flag occurs only in local overrides. It will remain subject to R's flag check.\n")
    cat("R's duplicate external-time lookup is replaced for this child only.\n")
    cat("R's future-file and compilation-flags checks remain enabled.\n")
    cat("Running the complete R CMD check --as-cran, including manuals.\n\n")
    log_path <- file.path(output, "console.log")
    connection <- file(log_path, open = "wt", encoding = "UTF-8")
    child <- tryCatch(.prism_check_run(r, c("CMD", "check", "--as-cran", paste0("--output=", output), archive),
      wd = workdir, env = c("current", R_CHECK_ENVIRON = prepared$path),
      echo = TRUE, stderr_to_stdout = TRUE, error_on_status = FALSE,
      cleanup_tree = TRUE, windows_hide_window = TRUE,
      stdout_line_callback = function(line, process) {
        writeLines(line, connection, useBytes = TRUE)
        flush(connection)
      }), finally = close(connection))
    check_log <- file.path(output, "prismrepl.Rcheck", "00check.log")
    lines <- if (file.exists(check_log)) readLines(check_log, warn = FALSE) else character()
    audit$check_status <- child$status
    audit$archive_unchanged <- identical(unname(tools::sha256sum(archive)), audit$sha256)
    audit$timestamp_ok <- any(grepl("^[*] checking for future file timestamps [.][.][.].* OK$", lines))
    audit$compilation_ok <- any(grepl("^[*] checking compilation flags used [.][.][.].* OK$", lines))
    audit$check_summary <- grep("^Status:", lines, value = TRUE)
    failed_section <- any(grepl("^[*] checking .* (ERROR|WARNING)$", lines))
    audit$status <- if (child$status != 0L) child$status else if (!audit$archive_unchanged ||
      !audit$timestamp_ok || !audit$compilation_ok || failed_section) 1L else 0L
    audit$state <- if (audit$status == 0L) "completed" else "failed"
    if (!audit$archive_unchanged) audit$error <- "The source archive changed during the check."
    else if (!audit$timestamp_ok || !audit$compilation_ok) audit$error <-
      "Review 00check.log: both target check sections must report OK; no failed check was hidden."
    list(status = as.integer(audit$status), output = output, check_log = check_log,
      audit = file.path(output, "preflight.txt"))
  }, error = function(error) {
    audit$state <<- "preflight or process failure"
    audit$error <<- conditionMessage(error)
    if (inherits(error, "prism_clock_error") && is.data.frame(error$observations))
      audit$clock <<- list(verified = FALSE, verified_at = "unverified", tolerance_seconds = 60,
        observations = error$observations)
    stop(error)
  })
  stopifnot("the check must return a scalar status and persistent evidence paths" =
    is.list(result) && is.integer(result$status) && length(result$status) == 1L &&
      dir.exists(result$output) && is.character(result$check_log))
  cat("\nCheck log:", result$check_log, "\nAudit:", result$audit, "\n")
  invisible(result)
}

if (sys.nframe() == 0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
  companions <- c(prism_check_clock = "check-clock.R", prism_check_context = "check-profile.R")
  invisible(lapply(names(companions), function(name) {
    if (!exists(name, mode = "function")) source(file.path(dirname(script), companions[[name]]))
  }))
  arguments <- commandArgs(trailingOnly = TRUE)
  status <- tryCatch({
    stopifnot("Usage: Rscript prismrepl-check.R SOURCE.tar.gz [NEW_OUTPUT_DIRECTORY]" =
      length(arguments) %in% 1:2)
    prism_check_package(arguments[[1L]], if (length(arguments) == 2L) arguments[[2L]] else NULL)$status
  }, error = function(error) {
    message("Check stopped: ", conditionMessage(error))
    1L
  })
  quit(save = "no", status = status, runLast = FALSE)
}
