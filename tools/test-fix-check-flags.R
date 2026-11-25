# Offline regression tests for the explicit persistent check-profile repair.
# Every write uses an owned temporary file selected through R_CHECK_ENVIRON.
# Compiler provenance is injected here; separate native fixture checks exercise
# a private R build whose actual Makeconf supplies the inherited flag.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "check-profile.R"))
source(file.path(dirname(normalizePath(script)), "fix-check-flags.R"))

test_fix_check_flags <- function() {
  directory <- tempfile("prism-persistent-flags-test-")
  stopifnot("private test directory must exist" = dir.create(directory, mode = "0700"))
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  prior <- Sys.getenv("R_CHECK_ENVIRON", unset = NA_character_)
  on.exit(if (is.na(prior)) Sys.unsetenv("R_CHECK_ENVIRON") else Sys.setenv(R_CHECK_ENVIRON = prior), add = TRUE)
  original_probe <- prism_check_context
  inherit <- active <- TRUE
  prism_check_context <- function(...) {
    # Force the private test selection and let the worker reject any startup
    # override before a test could reach an actual user check profile.
    result <- original_probe(..., profile = Sys.getenv("R_CHECK_ENVIRON", unset = ""))
    strip <- function(entry) {
      entry$output <- gsub("(^|[[:space:]])-mno-omit-leaf-frame-pointer([[:space:]]|$)", " ", entry$output)
      entry
    }
    result$config$default <- lapply(result$config$default, strip)
    result$config$active <- lapply(result$config$active, strip)
    if (inherit) result$config$default$CFLAGS$output <- paste(result$config$default$CFLAGS$output,
      "-mno-omit-leaf-frame-pointer")
    if (active) result$config$active$CFLAGS$output <- paste(result$config$active$CFLAGS$output,
      "-mno-omit-leaf-frame-pointer")
    result
  }
  repair <- prism_fix_check_flags
  environment(repair) <- environment()
  failed <- function(fun, message) {
    error <- tryCatch({ fun(); NULL }, error = identity)
    stopifnot("the expected safe refusal must occur" = inherits(error, "error") &&
      grepl(message, conditionMessage(error), fixed = TRUE))
  }
  profile <- file.path(directory, "check profile.Renviron")
  original <- charToRaw(paste0("# Existing UTF-8 comment: caf\u00e9.\r\n",
    '_R_CHECK_COMPILATION_FLAGS_KNOWN_="${_R_CHECK_COMPILATION_FLAGS_KNOWN_} -Wpedantic"\r\n',
    "_R_CHECK_SYSTEM_CLOCK_=TRUE\r\n_R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE\r\n",
    "_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_=5m\r\n"))
  writeBin(original, profile)
  Sys.chmod(profile, "0600")
  Sys.setenv(R_CHECK_ENVIRON = profile)
  before_mode <- file.info(profile)$mode
  inspected <- repair("inspect")
  stopifnot("inspection must not alter the profile" = identical(original, .prism_flag_bytes(profile)),
    "inspection must report inherited provenance" = isTRUE(inspected$origin[["inherited"]]))
  repaired <- repair()
  installed <- .prism_flag_bytes(profile)
  stopifnot("apply must create a verified repair" = identical(repaired$status, "repaired"),
    "the original byte prefix must be preserved" = identical(installed[seq_along(original)], original),
    "the backup must contain the exact original bytes" = identical(.prism_flag_bytes(repaired$backup), original),
    "profile file permissions must be preserved" = identical(before_mode, file.info(profile)$mode))
  count <- length(list.files(directory, all.files = TRUE))
  again <- repair()
  stopifnot("repeated apply must be idempotent" = identical(again$status, "already repaired") &&
    is.null(again$backup) && identical(installed, .prism_flag_bytes(profile)) &&
    identical(count, length(list.files(directory, all.files = TRUE))))
  tail <- charToRaw("# A later user comment stays after undo.\nPRISM_FLAG_TEST_TAIL=keep\n")
  writeBin(c(installed, tail), profile)
  undo <- repair("undo")
  stopifnot("undo must preserve every non-owned byte, including later user additions" =
    identical(.prism_flag_bytes(profile), c(original, tail)),
    "undo must save the exact pre-undo bytes" = identical(.prism_flag_bytes(undo$backup), c(installed, tail)))
  unchanged <- repair("undo")
  stopifnot("undo without a managed block must not write" = identical(unchanged$status, "unchanged") && is.null(unchanged$backup))

  invisible(repair())
  altered <- sub("_R_CHECK_COMPILATION_FLAGS_=TRUE", "_R_CHECK_COMPILATION_FLAGS_=FALSE",
    rawToChar(.prism_flag_bytes(profile)), fixed = TRUE)
  writeBin(charToRaw(altered), profile)
  failed(function() repair("undo"), "managed block has been edited")
  stopifnot("a modified managed block must remain untouched after refusal" =
    identical(.prism_flag_bytes(profile), charToRaw(altered)))
  writeBin(original, profile)
  inherit <- FALSE
  failed(function() repair(), "present only in local overrides")
  stopifnot("a local-only flag must not change the profile" = identical(.prism_flag_bytes(profile), original))
  active <- FALSE
  none <- repair()
  stopifnot("a build without the flag must need no changes" = identical(none$status, "unchanged") &&
    identical(.prism_flag_bytes(profile), original))
  inherit <- active <- TRUE
  invisible(repair())
  inherit <- active <- FALSE
  removed <- repair()
  stopifnot("revalidating another build must remove this tool's obsolete declaration" =
    identical(removed$status, "not needed") && identical(.prism_flag_bytes(profile), original))

  new_profile <- file.path(directory, "new-user-directory", "check.Renviron")
  Sys.setenv(R_CHECK_ENVIRON = new_profile)
  inherit <- active <- TRUE
  created <- repair()
  stopifnot("a new profile must be created without a fictitious old backup" =
    file.exists(new_profile) && is.null(created$backup))
  invisible(repair("undo"))
  stopifnot("undo must remove an otherwise empty tool-created file" = !file.exists(new_profile))
  Sys.setenv(R_CHECK_ENVIRON = "")
  failed(function() repair(), "R_CHECK_ENVIRON disables")

  prefix <- c(charToRaw("# Original Latin-1 byte: "), as.raw(233L), charToRaw("\r\n"))
  sample <- list(runtime = list(version = "4.5.0", platform = "test", home = "/private/test"),
    config = list(default = list(CC = list(output = "cc"))))
  block <- .prism_flag_block(sample, TRUE)
  stopifnot("byte-based block removal must preserve non-UTF-8 comments" =
    identical(.prism_flag_split(c(prefix, block, tail))$bytes, c(prefix, tail)))
  failed(function() .prism_flag_split(c(block, block)), "incomplete or duplicated")
  cat("PASS: R", as.character(getRversion()),
    "persistent profile repair, exact byte preservation, backups, idempotence,\n",
    "undo with later edits, edited-block refusal, inherited-only guard, cross-build\n",
    "revalidation, new-profile cleanup, disabled-profile refusal and unchanged clocks/compiler.\n")
  invisible(NULL)
}

test_fix_check_flags()
