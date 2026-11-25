# Run from a checkout with Rscript tools/test-check-profile.R.
# This test uses private files and processes. It changes no user or R files.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "check-profile.R"))

#' Verify Scoped Checker Profiles in Private Temporary Files
#' @return NULL invisibly after all assertions pass.
test_check_profile <- function() {
  directory <- tempfile("prism-profile-tests-")
  stopifnot("private test directory must be created" = dir.create(directory, mode = "0700"))
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  before <- Sys.getenv(c("R_CHECK_ENVIRON", "R_ENVIRON", "R_MAKEVARS_USER",
    "_R_CHECK_COMPILATION_FLAGS_KNOWN_", "_R_CHECK_SYSTEM_CLOCK_"), unset = NA_character_)
  empty <- prism_check_context(r, directory, profile = "")
  stopifnot("explicit empty selection must disable the original check profile" =
    identical(empty$profile$path, "") && !empty$profile$exists)

  makevars <- file.path(directory, "user flags with spaces.Makevars")
  writeLines("CFLAGS += -DPRISM_PROFILE_USER_FLAGS=1", makevars)
  profile <- file.path(directory, "original.Renviron")
  content <- c("# Preserve these exact bytes and expansion semantics.",
    '_R_CHECK_COMPILATION_FLAGS_KNOWN_="${_R_CHECK_COMPILATION_FLAGS_KNOWN_} -Wconversion"',
    "_R_CHECK_FUTURE_FILE_TIMESTAMPS_=FALSE",
    "_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_=2d",
    paste0('R_MAKEVARS_USER="', normalizePath(makevars, winslash = "/"), '"'))
  writeLines(content, profile, useBytes = TRUE)
  original_bytes <- readBin(profile, "raw", n = file.info(profile)$size)
  context <- prism_check_context(r, directory, profile = profile)
  stopifnot("original profile settings must affect the matching fresh process" =
    identical(context$settings[["_R_CHECK_FUTURE_FILE_TIMESTAMPS_"]], "FALSE"),
    "check-profile Makevars must be included in active compiler evidence" =
      any(grepl("-DPRISM_PROFILE_USER_FLAGS=1", context$config$active$CFLAGS$output, fixed = TRUE)),
    "build-default evidence must ignore the user Makevars override" =
      !any(grepl("-DPRISM_PROFILE_USER_FLAGS=1", context$config$default$CFLAGS$output, fixed = TRUE)))
  prepared <- prism_check_make_profile(context, directory, clock_verified = TRUE)
  effective <- prism_check_verify_profile(context, prepared, r, directory)
  stopifnot("original profile bytes must remain unchanged" =
    identical(original_bytes, readBin(profile, "raw", n = file.info(profile)$size)),
    "copied original bytes must remain the complete prefix" =
      identical(original_bytes, readBin(prepared$path, "raw", n = length(original_bytes))),
    "existing known flags must survive the final startup/profile sequence" =
      grepl("-Wconversion", effective$settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]], fixed = TRUE),
    "future file timestamp checking must remain active" =
      identical(effective$settings[["_R_CHECK_FUTURE_FILE_TIMESTAMPS_"]], "TRUE"),
    "future file timestamp leeway must use the normal five minutes" =
      identical(effective$settings[["_R_CHECK_FUTURE_FILE_TIMESTAMPS_LEEWAY_"]], "5m"))
  if (.Platform$OS.type != "windows") stopifnot("the temporary profile must be private" =
    bitwAnd(as.integer(file.info(prepared$path)$mode), 63L) == 0L)

  unverified <- prism_check_make_profile(context, directory, clock_verified = FALSE)
  effective_unverified <- prism_check_verify_profile(context, unverified, r, directory)
  stopifnot("an unverified clock must keep R's remote clock probe enabled" =
    identical(effective_unverified$settings[["_R_CHECK_SYSTEM_CLOCK_"]], "TRUE"))
  cat("_R_CHECK_FUTURE_FILE_TIMESTAMPS_=FALSE\n", file = prepared$path, append = TRUE)
  failed <- tryCatch({ prism_check_verify_profile(context, prepared, r, directory); FALSE },
    error = function(e) TRUE)
  stopifnot("a later timestamp-check override must be rejected" = failed)

  writeLines("CFLAGS += -mno-omit-leaf-frame-pointer", makevars)
  local_only <- prism_check_context(r, directory, profile = profile)
  # The user's Ubuntu build may genuinely include this flag by default. Use
  # controlled provenance for the negative branch while retaining the real
  # check-profile selection and Makevars evidence above.
  local_only$config$default <- lapply(local_only$config$default, function(value) {
    value$output <- gsub("(^|[[:space:]])-mno-omit-leaf-frame-pointer([[:space:]]|$)",
      " ", value$output)
    value
  })
  local_only$settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]] <- gsub(
    "(^|[[:space:]])-mno-omit-leaf-frame-pointer([[:space:]]|$)", " ",
    local_only$settings[["_R_CHECK_COMPILATION_FLAGS_KNOWN_"]])
  origin <- prism_check_leaf_origin(local_only)
  stopifnot("user-only compiler flags must not be relabeled as inherited" =
    isTRUE(origin[["active"]]) && !origin[["inherited"]])
  local_profile <- prism_check_make_profile(local_only, directory, clock_verified = TRUE)
  stopifnot("user-only leaf-frame flags must not be added to known defaults" = !local_profile$known_flag_added)
  inherited <- local_only
  inherited$config$default$CC$output <- paste(inherited$config$default$CC$output,
    "-mno-omit-leaf-frame-pointer")
  stopifnot("flags embedded in the default compiler command must be recognized" =
    prism_check_leaf_origin(inherited)[["inherited"]])
  inherited_profile <- prism_check_make_profile(inherited, directory, clock_verified = TRUE)
  stopifnot("verified inherited flags must receive a scoped declaration" = inherited_profile$known_flag_added)

  overwritten <- file.path(directory, "overwrite.Renviron")
  writeLines('R_CHECK_ENVIRON=""', overwritten)
  previous_site <- Sys.getenv("R_ENVIRON", unset = NA_character_)
  on.exit(if (is.na(previous_site)) Sys.unsetenv("R_ENVIRON") else Sys.setenv(R_ENVIRON = previous_site), add = TRUE)
  Sys.setenv(R_ENVIRON = overwritten)
  blocked <- tryCatch({ prism_check_context(r, directory, profile = profile); FALSE },
    error = function(e) TRUE)
  if (is.na(previous_site)) Sys.unsetenv("R_ENVIRON") else Sys.setenv(R_ENVIRON = previous_site)
  scoped_directory <- file.path(directory, "different working directory")
  stopifnot("the alternate check working directory must exist" = dir.create(scoped_directory))
  alternate <- prism_check_context(r, directory, profile = profile, workdir = scoped_directory)
  alternate_profile <- prism_check_make_profile(alternate, directory, clock_verified = TRUE)
  alternate_effective <- prism_check_verify_profile(alternate, alternate_profile, r,
    directory, workdir = scoped_directory)
  stopifnot("both preflight processes must use the later check's exact working directory" =
    identical(alternate_effective$working_directory, normalizePath(scoped_directory, winslash = "/")))
  stopifnot("a startup override of the requested profile must fail closed" = blocked,
    "probe/profile helpers must not leak environment changes into their caller" =
      identical(before, Sys.getenv(names(before), unset = NA_character_)))
  cat("PASS: R", as.character(getRversion()),
    "scoped profiles, original bytes, inherited expansion, compiler/Makevars identity,\n",
    "private permissions, active timestamp check, five-minute leeway, provenance guard,\n",
    "startup override rejection and unchanged parent environment.\n")
  invisible(NULL)
}

test_check_profile()
