# Run from the repository root after the complete package check passes.

#' Build One Verified Native Release Archive in an Isolated Temporary Library
#' @param root Package repository directory.
#' @param source_archive Optional path to the exact source archive already
#'   checked with R CMD check. Defaults to PRISMREPL_SOURCE_ARCHIVE when set;
#'   an empty value builds a fresh source archive with R CMD build.
#' @return The binary archive's absolute path, invisibly.
build_release <- function(root = ".", source_archive = Sys.getenv("PRISMREPL_SOURCE_ARCHIVE", unset = "")) {
  stopifnot("root must be an existing package directory" = is.character(root) &&
              length(root) == 1L && !is.na(root) && dir.exists(root),
            "source_archive must be one nonmissing path or empty text" =
              is.character(source_archive) && length(source_archive) == 1L && !is.na(source_archive))
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  source(file.path(root, "tools", "verify-platform.R"), local = TRUE)
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  stopifnot("DESCRIPTION must identify exactly one package" = nrow(description) == 1L &&
              all(c("Package", "Version") %in% colnames(description)))
  package <- unname(description[1L, "Package"])
  version <- unname(description[1L, "Version"])
  tarball <- paste0(package, "_", version, ".tar.gz")
  if (nzchar(source_archive)) {
    stopifnot("the checked source archive must be an existing file" =
                file.exists(source_archive) && !dir.exists(source_archive),
              "the checked source archive filename must match Package and Version" =
                identical(basename(source_archive), tarball))
    source_archive <- normalizePath(source_archive, winslash = "/", mustWork = TRUE)
  }
  ref <- Sys.getenv("GITHUB_REF")
  if (startsWith(ref, "refs/tags/")) {
    tag <- substring(ref, nchar("refs/tags/") + 1L)
    ref_name <- Sys.getenv("GITHUB_REF_NAME")
    stopifnot("tag must match DESCRIPTION version" = identical(tag, paste0("v", version)),
              "GitHub tag variables must agree" = !nzchar(ref_name) || identical(ref_name, tag))
  }
  commit <- Sys.getenv("GITHUB_SHA")
  stopifnot("GITHUB_SHA must be empty or a complete commit hash" = !nzchar(commit) ||
              grepl("^([0-9a-fA-F]{40}|[0-9a-fA-F]{64})$", commit))
  platform <- if (.Platform$OS.type == "windows") "windows" else
    if (Sys.info()[["sysname"]] == "Darwin") "macos" else "linux"
  stopifnot("release platform must be Windows, macOS or Linux" =
              .Platform$OS.type == "windows" || Sys.info()[["sysname"]] %in% c("Darwin", "Linux"))
  r_series <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
  r_exe <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  python_candidates <- unique(unname(Sys.which(c("python3", "python"))))
  python_candidates <- python_candidates[nzchar(python_candidates)]
  python_ready <- vapply(python_candidates, function(executable) isTRUE(tryCatch(
    suppressWarnings(system2(executable, c("-c", shQuote(
      "import sys; sys.exit(sys.version_info < (3, 8))")), stdout = FALSE, stderr = FALSE,
      timeout = 10L)) == 0L, error = function(e) FALSE)), logical(1L))
  stopifnot("release tooling requires Python 3.8 or newer (python3 or python)" = any(python_ready))
  python <- python_candidates[[which(python_ready)[[1L]]]]

  #' Run a Checked R Build Command
  #' @param args Separate command arguments, quoted where necessary.
  #' @return Zero invisibly on success.
  run <- function(args) {
    stopifnot("build arguments must be character" = is.character(args) && !anyNA(args))
    status <- system2(r_exe, args)
    stopifnot("R build command must succeed" = identical(status, 0L))
    invisible(status)
  }

  staging <- tempfile("prism-native-build-")
  stopifnot("temporary build directory must be created" = dir.create(staging))
  original <- getwd()
  on.exit({ setwd(original); unlink(staging, recursive = TRUE) }, add = TRUE)
  setwd(staging)
  if (nzchar(source_archive)) {
    stopifnot("the exact checked source archive must be staged" = file.copy(source_archive, tarball))
  } else run(c("CMD", "build", "--no-build-vignettes", shQuote(root)))
  stopifnot("source tarball must exist" = file.exists(tarball))
  checked <- system2(python, c(shQuote(file.path(root, "tools", "check-source-archive.py")),
    shQuote(tarball), "--description", shQuote(file.path(root, "DESCRIPTION"))))
  stopifnot("the exact source tarball must pass archive validation before compilation" = identical(checked, 0L))
  library <- file.path(staging, "library")
  stopifnot("temporary package library must be created" = dir.create(library))
  run(c("CMD", "INSTALL", "--preclean", "--build", shQuote(paste0("--library=", library)),
        shQuote(tarball)))
  binary <- setdiff(list.files(pattern = "[.](tar[.]gz|tgz|zip)$"), tarball)
  stopifnot("one compiled package archive must be produced" = length(binary) == 1L)
  extension <- if (grepl("[.]zip$", binary)) ".zip" else if (grepl("[.]tgz$", binary)) ".tgz" else ".tar.gz"
  stopifnot("archive type must match the operating system" =
              identical(extension, switch(platform, windows = ".zip", macos = ".tgz", linux = ".tar.gz")))
  outname <- paste0(package, "_", version, "-R", r_series, "-", platform, "-", actual, extension)
  manifest_name <- paste0(outname, ".manifest")
  include_source <- platform == "linux" && actual == "x86_64"
  expected_files <- c(outname, manifest_name, if (include_source) tarball)
  dist <- file.path(root, "dist")
  if (!dir.exists(dist)) stopifnot("dist directory must be created" = dir.create(dist))
  existing <- list.files(dist, all.files = TRUE, no.. = TRUE)
  stopifnot("dist contains other build artifacts; move them before building" =
              all(existing %in% expected_files))
  stopifnot("binary must be copied to dist" = file.copy(binary, file.path(dist, outname), overwrite = TRUE))
  manifest <- c(package = package, version = version, R = as.character(getRversion()),
    platform = platform, architecture = actual, r_platform = R.version$platform,
    operating_system = paste(Sys.info()[c("sysname", "release", "version")], collapse = " "),
    binary = outname, commit = commit)
  write.dcf(as.data.frame(as.list(manifest), check.names = FALSE), file.path(dist, manifest_name))
  if (include_source)
    stopifnot("source package must be copied" = file.copy(tarball, file.path(dist, tarball), overwrite = TRUE))
  out <- file.path(dist, outname)
  stopifnot("release archive must exist" = file.exists(out),
            "release manifest must exist" = file.exists(file.path(dist, manifest_name)))
  cat("Built", outname, "\n")
  invisible(out)
}

build_release()
