# Developer-only helpers for a scoped, audited R CMD check invocation.
# This file is excluded from the CRAN package by .Rbuildignore.

#' Probe the Environment of the Standard Checker in a Fresh R Process
#' @param request_path Private serialized request from prism_check_context().
#' @return NULL invisibly; the result is written to the requested private path.
#' @noRd
.prism_check_worker <- function(request_path) {
  request <- readRDS(request_path)
  before_home <- R.home()
  requested <- Sys.getenv("R_CHECK_ENVIRON", unset = NA_character_)
  selected <- requested
  if (is.na(selected)) {
    candidates <- c(if (nzchar(.Platform$r_arch))
      paste0("~/.R/check.Renviron.", .Platform$r_arch), "~/.R/check.Renviron")
    existing <- Filter(file.exists, candidates)
    selected <- if (length(existing)) existing[[1L]] else "~/.R/check.Renviron"
  }
  selected <- if (nzchar(selected)) normalizePath(path.expand(selected),
    winslash = "/", mustWork = FALSE) else ""
  if (!is.null(request$expected_profile)) stopifnot(
    "R startup replaced the requested check profile; inspect R_CHECK_ENVIRON in startup files" =
      identical(selected, request$expected_profile))
  exists <- nzchar(selected) && file.exists(selected)
  if (exists) {
    stopifnot("the selected check profile must be a readable regular file" =
      !dir.exists(selected) && file.access(selected, 4L) == 0L,
      "the selected check profile must load successfully" = readRenviron(selected))
  }
  stopifnot("the check profile must not change the selected R installation" = identical(before_home, R.home()))
  fields <- c("_R_CHECK_COMPILATION_FLAGS_", "_R_CHECK_COMPILATION_FLAGS_KNOWN_",
    "_R_CHECK_SYSTEM_CLOCK_", "_R_CHECK_FUTURE_FILE_TIMESTAMPS_",
    "_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_", "PRISMREPL_CHECK_PROFILE_TOKEN")
  query <- function(variable, defaults = FALSE) {
    args <- c("CMD", "config", if (defaults) c("--no-user-files", "--no-site-files"), variable)
    output <- suppressWarnings(system2(request$r, args, stdout = TRUE, stderr = TRUE,
      timeout = request$command_timeout))
    status <- attr(output, "status")
    list(status = if (is.null(status)) 0L else as.integer(status), output = as.character(output))
  }
  variables <- c("CC", "CFLAGS", "CPPFLAGS", "CPICFLAGS")
  config <- list(active = stats::setNames(lapply(variables, query), variables),
    default = stats::setNames(lapply(variables, query, defaults = TRUE), variables))
  stopifnot("compiler configuration queries must succeed" = all(vapply(
    c(config$active, config$default), function(x) identical(x$status, 0L), logical(1L))))
  result <- list(r = normalizePath(request$r, winslash = "/", mustWork = TRUE),
    runtime = list(version = as.character(getRversion()), home = before_home,
      architecture = .Platform$r_arch, platform = R.version$platform),
    profile = list(requested = requested, path = selected, exists = exists,
      after_read = Sys.getenv("R_CHECK_ENVIRON", unset = NA_character_),
      md5 = if (exists) unname(tools::md5sum(selected)) else NA_character_),
    working_directory = normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    settings = Sys.getenv(fields, unset = NA_character_),
    makevars = Sys.getenv(c("R_MAKEVARS_USER", "R_MAKEVARS_SITE"), unset = NA_character_),
    config = config)
  saveRDS(result, request$result)
  invisible(NULL)
}

#' Observe Startup, the Effective Check Profile and Compiler Provenance
#' @param r Absolute path to the same R executable used for the check.
#' @param directory Existing private staging directory owned by the caller.
#' @param profile NULL preserves normal profile selection; a string explicitly
#'   selects that profile, with an empty string disabling the original profile.
#' @param timeout Maximum subprocess time in seconds.
#' @param workdir Exact working directory of the later R CMD check process.
#' @return A list of runtime identity, profile metadata, selected settings and
#'   active/default C compiler configuration. No complete environment or profile
#'   contents are returned. The calling process is not modified.
prism_check_context <- function(r, directory, profile = NULL, timeout = 30,
    workdir = getwd()) {
  stopifnot("r must name an existing executable" = is.character(r) && length(r) == 1L &&
    !is.na(r) && file.exists(r) && !dir.exists(r),
    "directory must exist" = is.character(directory) && length(directory) == 1L && dir.exists(directory),
    "workdir must exist" = is.character(workdir) && length(workdir) == 1L && dir.exists(workdir),
    "profile must be absent or one string" = is.null(profile) ||
      (is.character(profile) && length(profile) == 1L && !is.na(profile)),
    "timeout must be positive" = is.numeric(timeout) && length(timeout) == 1L &&
      is.finite(timeout) && timeout > 0,
    "processx is required for bounded subprocesses" = requireNamespace("processx", quietly = TRUE))
  r <- normalizePath(r, winslash = "/", mustWork = TRUE)
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  workdir <- normalizePath(workdir, winslash = "/", mustWork = TRUE)
  staging <- tempfile("profile-probe-", tmpdir = directory)
  stopifnot("private probe directory must be created" = dir.create(staging, mode = "0700"))
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  script <- file.path(staging, "probe.R")
  request_path <- file.path(staging, "request.rds")
  result_path <- file.path(staging, "result.rds")
  expected <- if (is.null(profile)) NULL else if (nzchar(profile))
    normalizePath(path.expand(profile), winslash = "/", mustWork = FALSE) else ""
  request <- list(r = r, expected_profile = expected, result = result_path,
    command_timeout = max(1L, as.integer(timeout / 10)))
  writeLines(c("worker <-", deparse(.prism_check_worker, width.cutoff = 100L),
    "worker(commandArgs(trailingOnly = TRUE)[[1L]])"), script, useBytes = TRUE)
  saveRDS(request, request_path)
  invisible(Sys.chmod(c(script, request_path), "0600"))
  environment <- c("current", R_DEFAULT_PACKAGES = "", LC_COLLATE = "C",
    if (!is.null(expected)) c(R_CHECK_ENVIRON = expected))
  result <- processx::run(r, c("--no-restore", "--no-save", "--no-echo",
    paste0("--file=", script), "--args", request_path), env = environment,
    wd = workdir, timeout = timeout, error_on_status = FALSE, cleanup_tree = TRUE)
  stopifnot("the matching R startup/profile probe must finish successfully" =
    identical(result$status, 0L) && file.exists(result_path))
  out <- readRDS(result_path)
  stopifnot("probe must return complete runtime and configuration metadata" = is.list(out) &&
    identical(out$r, r) && identical(out$working_directory, workdir) &&
    is.list(out$runtime) && is.list(out$profile) &&
    is.character(out$settings) && is.list(out$config))
  out
}

#' Identify the Leaf-Frame Flag in Active and Build-Default C Settings
#' @param context Result of prism_check_context().
#' @return Named logical values for active and inherited flag occurrences.
prism_check_leaf_origin <- function(context) {
  stopifnot("context must contain active and default configuration" =
    is.list(context$config$active) && is.list(context$config$default))
  found <- function(config) {
    text <- unlist(lapply(config, `[[`, "output"), use.names = FALSE)
    tokens <- unlist(strsplit(text, "[[:space:]]+"), use.names = FALSE)
    tokens <- gsub("^['\"]|['\"]$", "", tokens)
    "-mno-omit-leaf-frame-pointer" %in% tokens
  }
  out <- c(active = found(context$config$active), inherited = found(context$config$default))
  stopifnot("flag provenance must contain two nonmissing logical values" = is.logical(out) &&
    length(out) == 2L && !anyNA(out))
  out
}

#' Compose a Private Check Profile While Preserving Existing Profile Bytes
#' @param context Result of prism_check_context().
#' @param directory Existing private staging directory owned by the caller.
#' @param clock_verified TRUE only after the caller has independently verified
#'   fresh UTC; then only the checker's duplicate remote-clock lookup is disabled.
#' @return Path, marker token, provenance and profile metadata. The caller owns
#'   cleanup of the returned temporary file. No user or R installation file changes.
prism_check_make_profile <- function(context, directory, clock_verified = FALSE) {
  stopifnot("context must contain profile metadata and settings" =
    is.list(context$profile) && is.character(context$settings),
    "directory must exist" = is.character(directory) && length(directory) == 1L && dir.exists(directory),
    "clock_verified must be logical" = is.logical(clock_verified) && length(clock_verified) == 1L && !is.na(clock_verified))
  origin <- prism_check_leaf_origin(context)
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  original <- context$profile$path
  exists_now <- nzchar(original) && file.exists(original)
  stopifnot("the original profile must not change after preflight" =
    identical(exists_now, context$profile$exists) && (!exists_now ||
      identical(unname(tools::md5sum(original)), context$profile$md5)))
  path <- tempfile("check-profile-", tmpdir = directory, fileext = ".Renviron")
  stopifnot("the temporary profile must be created" = file.create(path))
  invisible(Sys.chmod(path, "0600"))
  if (exists_now) stopifnot("the original profile bytes must be preserved" =
    file.copy(original, path, overwrite = TRUE, copy.mode = FALSE) &&
      identical(unname(tools::md5sum(path)), context$profile$md5))
  token <- basename(tempfile("prism-profile-token-"))
  known <- context$settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]]
  known <- if (is.na(known)) character() else strsplit(known, "[[:space:]]+")[[1L]]
  add_leaf <- isTRUE(origin[["inherited"]]) && !"-mno-omit-leaf-frame-pointer" %in% known
  declarations <- c("# One-process prismrepl check profile; original settings are above.",
    if (add_leaf) '_R_CHECK_COMPILATION_FLAGS_KNOWN_="${_R_CHECK_COMPILATION_FLAGS_KNOWN_} -mno-omit-leaf-frame-pointer"',
    "_R_CHECK_COMPILATION_FLAGS_=TRUE", "_R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE",
    "_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_=5m",
    paste0("_R_CHECK_SYSTEM_CLOCK_=", if (clock_verified) "FALSE" else "TRUE"),
    paste0('PRISMREPL_CHECK_PROFILE_TOKEN="', token, '"'))
  connection <- file(path, open = "ab")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(paste0("\n", paste(declarations, collapse = "\n"), "\n")), connection)
  out <- list(path = normalizePath(path, winslash = "/", mustWork = TRUE), token = token,
    known_flag_added = add_leaf, leaf_origin = origin, clock_verified = clock_verified,
    source_profile = context$profile)
  stopifnot("prepared profile must exist and carry its marker" = file.exists(out$path) &&
    is.character(out$token) && length(out$token) == 1L && nzchar(out$token))
  out
}

#' Verify the Composed Profile After the Same R Startup Sequence
#' @param context Original context returned by prism_check_context().
#' @param prepared Result of prism_check_make_profile().
#' @param r Same R executable used for the actual check.
#' @param directory Private staging directory.
#' @param timeout Maximum probe time in seconds.
#' @param workdir Exact working directory of the later R CMD check process.
#' @return Effective context after verifying profile selection, compiler identity,
#'   preserved known flags, timestamp checking and the private profile marker.
prism_check_verify_profile <- function(context, prepared, r, directory, timeout = 30,
    workdir = getwd()) {
  stopifnot("prepared profile must be an existing file" = is.list(prepared) &&
    is.character(prepared$path) && length(prepared$path) == 1L && file.exists(prepared$path))
  out <- prism_check_context(r, directory, profile = prepared$path, timeout = timeout,
    workdir = workdir)
  expected_clock <- if (prepared$clock_verified) "FALSE" else "TRUE"
  settings <- out$settings
  tokens <- function(value) if (is.na(value)) character() else strsplit(value, "[[:space:]]+")[[1L]]
  previous_known <- tokens(context$settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]])
  current_known <- tokens(settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]])
  stopifnot("the selected R runtime must remain unchanged" = identical(out$runtime, context$runtime),
    "the check working directory must remain unchanged" = identical(out$working_directory, context$working_directory),
    "the complete compiler configuration must remain unchanged" = identical(out$config, context$config),
    "the selected Makevars files must remain unchanged" = identical(out$makevars, context$makevars),
    "the requested private profile must actually load after startup" =
      identical(settings[["PRISMREPL_CHECK_PROFILE_TOKEN"]], prepared$token),
    "the copied profile must not redirect R_CHECK_ENVIRON for nested checks" =
      identical(out$profile$after_read, prepared$path),
    "compilation flag checking must remain enabled" = identical(settings[["_R_CHECK_COMPILATION_FLAGS_"]], "TRUE"),
    "future file timestamp checking must remain enabled" = identical(settings[["_R_CHECK_FUTURE_FILE_TIMESTAMPS_"]], "TRUE"),
    "future file timestamps must retain R's five-minute leeway" = identical(settings[["_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_"]], "5m"),
    "the requested external-clock policy must be effective" = identical(settings[["_R_CHECK_SYSTEM_CLOCK_"]], expected_clock),
    "existing known flags must be preserved" = all(previous_known %in% current_known),
    "a verified inherited leaf-frame flag must be recognized" =
      !prepared$leaf_origin[["inherited"]] || "-mno-omit-leaf-frame-pointer" %in% current_known)
  out
}
