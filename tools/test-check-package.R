# Run with Rscript tools/test-check-package.R.
# These offline tests replace process, profile and clock boundaries. They do
# not execute a package check or make network requests; the real helpers have
# separate tests. All fixture files belong to a private temporary directory.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
driver <- file.path(dirname(normalizePath(script)), "check-package.R")

#' Test Check-Driver Orchestration Without Network or Compiler Side Effects
#' @return NULL invisibly after all named assertions pass.
test_check_package <- function() {
  directory <- tempfile("prism-driver-tests-")
  stopifnot("private test directory must be created" = dir.create(directory, mode = "0700"))
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  initial_workdir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  initial_environment <- Sys.getenv(c("R_CHECK_ENVIRON", "R_ENVIRON", "R_MAKEVARS_USER",
    "_R_CHECK_COMPILATION_FLAGS_KNOWN_", "_R_CHECK_SYSTEM_CLOCK_"), unset = NA_character_)
  success_lines <- c("* checking for future file timestamps ... OK",
    "* checking compilation flags used ... OK", "* checking tests ... OK", "Status: OK")

  run_case <- function(name, status = 0L, lines = success_lines, clock_error = FALSE,
      clock_unverified = FALSE, profile_error = FALSE, process_error = FALSE,
      mutate_archive = FALSE, omit_log = FALSE, existing_output = FALSE) {
    folder <- file.path(directory, name)
    stopifnot("case directory must be created" = dir.create(folder))
    archive <- file.path(folder, "prismrepl_0.6.5.tar.gz")
    writeBin(charToRaw("private source archive fixture\n"), archive)
    sha256 <- unname(tools::sha256sum(archive))
    output <- file.path(folder, "results with spaces")
    if (existing_output) {
      stopifnot("existing output fixture must be created" = dir.create(output))
      writeLines("keep this previous result", file.path(output, "keep.txt"))
    }
    state <- new.env(parent = emptyenv())
    state$calls <- character()
    state$staging <- NA_character_
    state$profile <- NA_character_
    state$process_calls <- 0L
    scope <- new.env(parent = environment())
    source(driver, local = scope)
    scope$prism_check_context <- function(r, directory, workdir) {
      state$calls <- c(state$calls, "context")
      state$staging <- directory
      state$workdir <- workdir
      stopifnot("preflight must preserve caller working-directory semantics" =
        identical(workdir, initial_workdir))
      list(profile = list(path = "", exists = FALSE, md5 = NA_character_))
    }
    scope$prism_check_leaf_origin <- function(context) c(active = FALSE, inherited = FALSE)
    scope$prism_check_clock <- function(timeout, tolerance) {
      state$calls <- c(state$calls, "clock")
      observations <- data.frame(source = "offline fixture", accepted = FALSE,
        reason = "synthetic unavailable source", stringsAsFactors = FALSE)
      if (clock_error) stop(structure(list(message = "Synthetic unavailable UTC evidence",
        call = NULL, observations = observations),
        class = c("prism_clock_error", "error", "condition")))
      list(verified = !clock_unverified, verified_at = "offline-test-only",
        tolerance_seconds = tolerance, observations = observations)
    }
    scope$prism_check_make_profile <- function(context, directory, clock_verified) {
      state$calls <- c(state$calls, "profile")
      stopifnot("profile must follow positive UTC verification" = isTRUE(clock_verified))
      state$profile <- file.path(directory, "private.Renviron")
      writeLines("# private offline profile fixture", state$profile)
      list(path = state$profile, known_flag_added = FALSE,
        leaf_origin = c(active = FALSE, inherited = FALSE))
    }
    scope$prism_check_verify_profile <- function(context, prepared, r, directory, workdir) {
      state$calls <- c(state$calls, "verify")
      stopifnot("profile verification must use the actual checker working directory" =
        identical(workdir, initial_workdir))
      if (profile_error) stop("Synthetic startup replaced the private profile", call. = FALSE)
      list(settings = c(`_R_CHECK_COMPILATION_FLAGS_` = "TRUE",
        `_R_CHECK_SYSTEM_CLOCK_` = "FALSE", `_R_CHECK_FUTURE_FILE_TIMESTAMPS_` = "TRUE",
        `_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_` = "5m"))
    }
    scope$.prism_check_run <- function(command, args, ...) {
      state$calls <- c(state$calls, "check")
      state$process_calls <- state$process_calls + 1L
      options <- list(...)
      stopifnot("the standard check must run from the original working directory" =
        identical(options$wd, initial_workdir),
        "the check must use the selected R installation" = identical(command,
          file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")),
        "the complete as-cran command must receive the exact source archive" = identical(args,
          c("CMD", "check", "--as-cran", paste0("--output=", normalizePath(output,
            winslash = "/", mustWork = TRUE)), normalizePath(archive, winslash = "/", mustWork = TRUE))),
        "child environment must retain the parent and only select its private profile" =
          identical(options$env, c("current", R_CHECK_ENVIRON = state$profile)),
        "the private profile must exist when the checker starts" = file.exists(state$profile),
        "merged logging and process-tree cleanup must stay enabled" =
          isTRUE(options$stderr_to_stdout) && isTRUE(options$cleanup_tree),
        "nonzero child status must remain observable" = identical(options$error_on_status, FALSE))
      options$stdout_line_callback("checking: R \u2265 4.5.0", NULL)
      if (process_error) stop("Synthetic child-process failure", call. = FALSE)
      if (mutate_archive) writeBin(charToRaw("changed source fixture\n"), archive)
      if (!omit_log) {
        check_directory <- file.path(output, "prismrepl.Rcheck")
        stopifnot("synthetic check directory must be created" = dir.create(check_directory))
        writeLines(lines, file.path(check_directory, "00check.log"))
      }
      list(status = as.integer(status))
    }
    connections_before <- nrow(showConnections(all = TRUE))
    transcript <- capture.output(result <- tryCatch(
      scope$prism_check_package(archive, output), error = function(error) error))
    audit_path <- file.path(output, "preflight.rds")
    audit <- if (file.exists(audit_path)) readRDS(audit_path) else NULL
    stopifnot("every exit must remove its private staging directory" =
      is.na(state$staging) || !dir.exists(state$staging),
      "every exit must remove its private check profile" =
        is.na(state$profile) || !file.exists(state$profile),
      "every exit must close its log connection" =
        identical(nrow(showConnections(all = TRUE)), connections_before),
      "the driver must leave the caller's working directory unchanged" =
        identical(normalizePath(getwd(), winslash = "/"), initial_workdir),
      "the driver must leave the caller's selected environment unchanged" =
        identical(Sys.getenv(names(initial_environment), unset = NA_character_), initial_environment))
    list(result = result, audit = audit, state = state, output = output,
      sha256 = sha256, transcript = transcript, archive = archive)
  }

  failed_clock <- run_case("unavailable-clock", clock_error = TRUE)
  stopifnot("unavailable UTC evidence must stop before creating a check profile or child" =
    inherits(failed_clock$result, "prism_clock_error") &&
      identical(failed_clock$state$calls, c("context", "clock")) &&
      failed_clock$state$process_calls == 0L,
    "failed UTC evidence must be retained transparently" =
      identical(failed_clock$audit$clock$verified, FALSE) &&
      identical(failed_clock$audit$clock$observations$reason, "synthetic unavailable source"),
    "failed preflight must retain the identity of the untouched archive" =
      identical(failed_clock$audit$sha256, unname(tools::sha256sum(failed_clock$archive))))
  unverified_clock <- run_case("unverified-clock", clock_unverified = TRUE)
  stopifnot("a clock result without positive verification must not start a checker" =
    inherits(unverified_clock$result, "error") && unverified_clock$state$process_calls == 0L)
  failed_profile <- run_case("changed-profile", profile_error = TRUE)
  stopifnot("a failed profile verification must stop before the checker" =
    inherits(failed_profile$result, "error") && failed_profile$state$process_calls == 0L)

  success <- run_case("success")
  stopifnot("a valid completed check must return success" = identical(success$result$status, 0L),
    "success must follow verification before starting the checker" =
      identical(success$state$calls, c("context", "clock", "profile", "verify", "check")),
    "successful evidence must identify the exact unchanged source" =
      identical(success$audit$sha256, success$sha256) && isTRUE(success$audit$archive_unchanged),
    "both target check sections must be present and OK" =
      isTRUE(success$audit$timestamp_ok) && isTRUE(success$audit$compilation_ok),
    "merged console output must persist as UTF-8" = identical(readLines(
      file.path(success$output, "console.log"), encoding = "UTF-8"), "checking: R \u2265 4.5.0"),
    "the human-readable audit must remain available" = file.exists(success$result$audit))
  child_failure <- run_case("child-status", status = 7L)
  stopifnot("the real child exit status must be propagated" =
    identical(child_failure$result$status, 7L) && identical(child_failure$audit$check_status, 7L),
    "a failed checker must retain its original log" = file.exists(child_failure$result$check_log))

  failed_sections <- lapply(list(
    timestamp = sub("future file timestamps ... OK", "future file timestamps ... NOTE", success_lines, fixed = TRUE),
    compiler = sub("compilation flags used ... OK", "compilation flags used ... NOTE", success_lines, fixed = TRUE),
    warning = c(success_lines, "* checking PDF version of manual ... WARNING")),
    function(lines) run_case(paste0("section-", length(list.files(directory))), lines = lines))
  stopifnot("a target NOTE or genuine check warning must never become driver success" =
    all(vapply(failed_sections, function(case) identical(case$result$status, 1L), logical(1L))))
  missing_log <- run_case("missing-check-log", omit_log = TRUE)
  stopifnot("a zero child status without actual target sections must fail" =
    identical(missing_log$result$status, 1L) && !missing_log$audit$timestamp_ok && !missing_log$audit$compilation_ok)
  genuine_note <- run_case("new-submission-note", lines = c(
    "* checking CRAN incoming feasibility ... NOTE", "New submission", success_lines[1:3], "Status: 1 NOTE"))
  stopifnot("unrelated genuine NOTEs must remain visible in the audit" =
    identical(genuine_note$result$status, 0L) && identical(genuine_note$audit$check_summary, "Status: 1 NOTE"))
  mutated <- run_case("changed-archive", mutate_archive = TRUE)
  stopifnot("source mutation during checking must fail even with otherwise OK sections" =
    identical(mutated$result$status, 1L) && identical(mutated$audit$archive_unchanged, FALSE) &&
      identical(mutated$audit$sha256, mutated$sha256))

  process_failure <- run_case("process-exception", process_error = TRUE)
  stopifnot("a process exception must retain a failure audit and partial console log" =
    inherits(process_failure$result, "error") &&
      identical(process_failure$audit$error, "Synthetic child-process failure") &&
      length(readLines(file.path(process_failure$output, "console.log"))) == 1L)
  existing <- run_case("existing-output", existing_output = TRUE)
  stopifnot("existing output must be refused before any probe or child" =
    inherits(existing$result, "error") && !length(existing$state$calls),
    "refusing existing output must preserve all previous results" =
      identical(readLines(file.path(existing$output, "keep.txt")), "keep this previous result") &&
      identical(list.files(existing$output), "keep.txt"))
  cat("PASS: R", as.character(getRversion()),
    "offline driver checks: UTC/profile fail-closed, complete check arguments,\n",
    "matching startup directory, merged UTF-8 logs, SHA-256 identity, target sections,\n",
    "child exit status, warning preservation, audit, no overwrite and cleanup.\n")
  invisible(NULL)
}

test_check_package()
