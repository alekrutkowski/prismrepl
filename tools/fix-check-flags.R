# Explicit one-time repair for an inherited Ubuntu compiler flag.
# This developer tool is excluded from the CRAN package. It is never run by
# package installation, loading, examples or tests.
# Usage: Rscript --no-init-file --no-site-file prismrepl-fix-check-flags.R
#        Rscript --no-init-file --no-site-file prismrepl-fix-check-flags.R --undo

#' Create an Empty Private File Before Writing Any Profile Content
#' @param path A fresh path owned by this operation.
#' @return The same path invisibly.
.prism_flag_private <- function(path) {
  stopifnot("private path must be one fresh filename" = is.character(path) &&
    length(path) == 1L && !is.na(path) && !file.exists(path),
    "the empty private file must be created" = file.create(path),
    "the empty file must be private before any content is written" = Sys.chmod(path, "0600"))
  if (.Platform$OS.type != "windows") stopifnot("private file must deny group and other access" =
    bitwAnd(as.integer(file.info(path)$mode), 63L) == 0L)
  invisible(path)
}

#' Read a Profile Without Changing Its Encoding or Line Endings
#' @param path One file path; a nonexistent file has no bytes.
#' @return A raw vector.
.prism_flag_bytes <- function(path) {
  stopifnot("path must be one string" = is.character(path) && length(path) == 1L && !is.na(path))
  out <- if (file.exists(path)) readBin(path, "raw", n = file.info(path)$size) else raw()
  stopifnot("profile content must be raw bytes" = is.raw(out))
  out
}

#' Fingerprint Exact Bytes for Change Detection
#' @param bytes A raw vector.
#' @return One hexadecimal SHA-256 string.
.prism_flag_hash <- function(bytes) {
  stopifnot("bytes must be raw" = is.raw(bytes))
  path <- tempfile("prism-flag-hash-")
  on.exit(unlink(path), add = TRUE)
  .prism_flag_private(path)
  writeBin(bytes, path)
  out <- unname(tools::sha256sum(path))
  stopifnot("hash must be one SHA-256 string" = is.character(out) && length(out) == 1L && nchar(out) == 64L)
  out
}

#' Separate This Tool's Unmodified Block From All Other Profile Bytes
#' @param bytes Original profile bytes.
#' @return Original bytes without the block, and optional block metadata.
.prism_flag_split <- function(bytes) {
  stopifnot("profile must be bytes without NULs" = is.raw(bytes) && !any(bytes == as.raw(0L)))
  begin <- "\n# BEGIN prismrepl inherited compiler flag v1\n"
  end <- "# END prismrepl inherited compiler flag v1\n"
  text <- rawToChar(bytes)
  starts <- gregexpr(begin, text, fixed = TRUE, useBytes = TRUE)[[1L]]
  ends <- gregexpr(end, text, fixed = TRUE, useBytes = TRUE)[[1L]]
  if (starts[[1L]] == -1L && ends[[1L]] == -1L) return(list(bytes = bytes, block = NULL))
  stopifnot("the managed block is incomplete or duplicated; review the profile without rewriting it" =
    length(starts) == 1L && length(ends) == 1L && starts[[1L]] > 0L && ends[[1L]] > starts[[1L]])
  first <- starts[[1L]]
  last <- ends[[1L]] + nchar(end, type = "bytes") - 1L
  block <- bytes[seq.int(first, last)]
  lines <- strsplit(rawToChar(block), "\n", fixed = TRUE)[[1L]]
  stopifnot("the managed block has been edited; no automatic changes will be made" =
    length(lines) == 11L && startsWith(lines[[3L]], "# Content SHA-256: ") &&
    lines[[6L]] %in% c("# Original profile existed: TRUE", "# Original profile existed: FALSE"))
  body <- charToRaw(paste0(paste(lines[4L:10L], collapse = "\n"), "\n"))
  stopifnot("the managed block has been edited; no automatic changes will be made" =
    identical(sub("# Content SHA-256: ", "", lines[[3L]], fixed = TRUE), .prism_flag_hash(body)))
  remaining <- c(if (first > 1L) bytes[seq_len(first - 1L)],
    if (last < length(bytes)) bytes[seq.int(last + 1L, length(bytes))])
  list(bytes = remaining, block = block,
    originally_existed = identical(lines[[6L]], "# Original profile existed: TRUE"))
}

#' Compose a Provenance-Labeled Check-Only Declaration
#' @param context Fresh compiler and profile evidence.
#' @param originally_existed Whether the profile predated this tool.
#' @return A raw block that changes only compiler-flag check settings.
.prism_flag_block <- function(context, originally_existed) {
  scope <- c(context$runtime$version, context$runtime$platform, context$runtime$home)
  stopifnot("originally_existed must be logical" = is.logical(originally_existed) &&
    length(originally_existed) == 1L && !is.na(originally_existed),
    "R build metadata must fit on single lines" = is.character(scope) &&
    length(scope) == 3L && !anyNA(scope) && !any(grepl("[\r\n]", scope)))
  body <- c(paste0("# Selected R: ", scope[[1L]], "; ", scope[[2L]]),
    paste0("# R home: ", scope[[3L]]),
    paste0("# Original profile existed: ", originally_existed),
    paste0("# Default compiler SHA-256: ", .prism_flag_hash(serialize(context$config$default, NULL, version = 2L))),
    "# Re-run this repair when changing R builds; the user profile is shared.",
    '_R_CHECK_COMPILATION_FLAGS_KNOWN_="${_R_CHECK_COMPILATION_FLAGS_KNOWN_} -mno-omit-leaf-frame-pointer"',
    "_R_CHECK_COMPILATION_FLAGS_=TRUE")
  body <- charToRaw(enc2utf8(paste0(paste(body, collapse = "\n"), "\n")))
  out <- c(charToRaw(paste0("\n# BEGIN prismrepl inherited compiler flag v1\n",
    "# Content SHA-256: ", .prism_flag_hash(body), "\n")), body,
    charToRaw("# END prismrepl inherited compiler flag v1\n"))
  stopifnot("managed block must be raw" = is.raw(out))
  out
}

#' Replace One Explicitly Selected Profile With a Recoverable Write
#' @param path Selected check profile.
#' @param bytes Replacement raw bytes, or NULL to remove an empty tool-created file.
#' @param previous Bytes observed before writing.
#' @param existed Whether the file existed before writing.
#' @return Backup path, or NULL for a new profile.
.prism_flag_replace <- function(path, bytes, previous, existed) {
  stopifnot("the profile changed during inspection; retry without concurrent edits" =
    identical(file.exists(path), existed) && identical(.prism_flag_bytes(path), previous),
    "replacement must be raw or absent" = is.null(bytes) || is.raw(bytes))
  parent <- dirname(path)
  if (!dir.exists(parent)) stopifnot("the user profile directory must be created" =
    dir.create(parent, recursive = TRUE, mode = "0700"))
  stopifnot("the selected check-profile directory must be writable" = file.access(parent, 2L) == 0L)
  backup <- NULL
  if (existed) {
    backup <- tempfile(paste0(basename(path), ".prismrepl-backup-"), tmpdir = parent)
    .prism_flag_private(backup)
    stopifnot("an exact backup must be saved before editing" =
      file.copy(path, backup, overwrite = TRUE, copy.mode = FALSE) &&
        identical(.prism_flag_bytes(backup), previous))
  }
  if (is.null(bytes)) {
    stopifnot("the tool-created empty profile must be removable" = unlink(path) == 0L && !file.exists(path))
  } else {
    staged <- tempfile(".prismrepl-profile-", tmpdir = parent)
    on.exit(unlink(staged), add = TRUE)
    .prism_flag_private(staged)
    writeBin(bytes, staged)
    invisible(Sys.chmod(staged, if (existed) file.info(path)$mode else "0600"))
    stopifnot("the staged profile bytes must be complete before replacement" =
      identical(.prism_flag_bytes(staged), bytes),
      "the selected profile changed before replacement; retry without concurrent edits" =
        identical(file.exists(path), existed) && identical(.prism_flag_bytes(path), previous))
    # Rename within the same directory is atomic on the target Ubuntu/WSL
    # filesystem. Refuse a non-atomic overwrite rather than risk partial data.
    if (!isTRUE(suppressWarnings(file.rename(staged, path)))) stop(
      "The profile could not be atomically replaced. No non-atomic overwrite was attempted. Backup: ",
      if (is.null(backup)) "not needed for a new profile" else backup, call. = FALSE)
    stopifnot("written bytes must match the prepared profile" = identical(.prism_flag_bytes(path), bytes))
  }
  backup
}

#' Repair Ordinary R CMD Check's Declaration of an Inherited Build Flag
#'
#' This explicit developer action updates only the effective user check profile.
#' It does not change compilation, optimization, clock or timestamp settings.
#' The profile is shared across R builds: rerun after switching or rebuilding R.
#'
#' @param action Apply the repair, remove only this tool's unchanged block, or inspect.
#' @param r Selected R executable; defaults to this R installation.
#' @param workdir Working directory for the ordinary check.
#' @return A list of status, profile, backup and verified R-build scope.
prism_fix_check_flags <- function(action = "apply", r = NULL, workdir = getwd()) {
  stopifnot("action must be apply, undo or inspect" = is.character(action) &&
    length(action) == 1L && action %in% c("apply", "undo", "inspect"))
  if (is.null(r)) r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  directory <- tempfile("prism-flag-repair-")
  stopifnot("private probe directory must be created" = dir.create(directory, mode = "0700"))
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  context <- prism_check_context(r, directory, workdir = workdir)
  path <- context$profile$path
  stopifnot("R_CHECK_ENVIRON disables the check profile; unset it before applying this repair" = nzchar(path))
  origin <- prism_check_leaf_origin(context)
  previous <- .prism_flag_bytes(path)
  parts <- .prism_flag_split(previous)
  inherited <- isTRUE(origin[["inherited"]])
  status <- if (action == "inspect") "inspected" else if (action == "undo") "removed" else
    if (inherited) "repaired" else "not needed"
  if (action == "inspect") return(list(status = status, profile = path, backup = NULL,
    runtime = context$runtime, origin = origin, managed = !is.null(parts$block)))
  if (action == "apply" && !inherited && isTRUE(origin[["active"]]) && is.null(parts$block)) stop(
    "The flag is present only in local overrides, not R's build defaults. Review user/site Makevars; no check setting was changed.",
    call. = FALSE)
  originally_existed <- if (is.null(parts$block)) context$profile$exists else parts$originally_existed
  replacement <- if (action == "apply" && inherited)
    c(parts$bytes, .prism_flag_block(context, originally_existed)) else parts$bytes
  if (!length(replacement) && !originally_existed) replacement <- NULL
  if (identical(previous, replacement) || (is.null(replacement) && !context$profile$exists) ||
      (action == "undo" && is.null(parts$block))) {
    return(list(status = if (action == "apply" && inherited) "already repaired" else "unchanged",
      profile = path, backup = NULL, runtime = context$runtime, origin = origin))
  }
  backup <- .prism_flag_replace(path, replacement, previous, context$profile$exists)
  verified <- FALSE
  on.exit(if (!verified && identical(.prism_flag_bytes(path), if (is.null(replacement)) raw() else replacement)) {
    if (context$profile$exists) writeBin(previous, path) else unlink(path)
  }, add = TRUE)
  after <- prism_check_context(r, directory, workdir = workdir)
  clocks <- c("_R_CHECK_SYSTEM_CLOCK_", "_R_CHECK_FUTURE_FILE_TIMESTAMPS_", "_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_")
  tokens <- function(value) if (is.na(value)) character() else strsplit(value, "[[:space:]]+")[[1L]]
  known_before <- tokens(context$settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]])
  known_after <- tokens(after$settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]])
  stopifnot("the selected R build and compiler configuration must be unchanged" =
    identical(context$runtime, after$runtime) && identical(context$config, after$config),
    "Makevars selectors and check-profile selection must be unchanged" =
      identical(context$makevars, after$makevars) && identical(context$profile$path, after$profile$path) &&
      identical(context$profile$after_read, after$profile$after_read),
    "all clock and file-timestamp check settings must be unchanged" =
      identical(context$settings[clocks], after$settings[clocks]),
    "existing unrelated known flags must be preserved" =
      all(setdiff(known_before, "-mno-omit-leaf-frame-pointer") %in% known_after),
    "the verified inherited flag must be recognized with flag checking enabled" =
      action != "apply" || !inherited || ("-mno-omit-leaf-frame-pointer" %in% known_after &&
        identical(after$settings[["_R_CHECK_COMPILATION_FLAGS_"]], "TRUE")))
  verified <- TRUE
  out <- list(status = status, profile = path, backup = backup, runtime = after$runtime, origin = origin)
  stopifnot("repair must return its status and verified profile" = is.list(out) && nzchar(out$status) && nzchar(out$profile))
  out
}

if (sys.nframe() == 0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
  if (!exists("prism_check_context", mode = "function")) source(file.path(dirname(script), "check-profile.R"))
  args <- commandArgs(trailingOnly = TRUE)
  status <- tryCatch({
    stopifnot("Usage: Rscript prismrepl-fix-check-flags.R [--apply|--inspect|--undo]" =
      length(args) == 0L || (length(args) == 1L && args %in% c("--apply", "--inspect", "--undo")))
    result <- prism_fix_check_flags(if (!length(args)) "apply" else substring(args, 3L))
    cat("Status:", result$status, "\nCheck profile:", result$profile, "\n")
    if (!is.null(result$backup)) cat("Original bytes backed up:", result$backup, "\n")
    cat(c(paste("Verified R build:", result$runtime$version, "at", result$runtime$home),
      paste("Inherited leaf-frame flag:", result$origin[["inherited"]]),
      "Use your ordinary R CMD check --as-cran command after a successful repair.",
      "Compiler flags and all clock/timestamp checks are unchanged.",
      "The user check profile is shared: rerun this tool when changing R builds."), sep = "\n")
    0L
  }, error = function(e) { message("Repair stopped: ", conditionMessage(e)); 1L })
  quit(save = "no", status = status, runLast = FALSE)
}
