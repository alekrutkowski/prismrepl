# Run against the installed package: Rscript tools/test-cran-worker.R
# Development only: Rscript tools/test-cran-worker.R --source
# All repository reads use a temporary file:// repository, never the network.

#' Exercise Real Index Processes and Their Ownership Without Network Access
#' @param source Load this checkout without compiling when TRUE.
#' @return TRUE invisibly after all lifecycle assertions succeed.
check_cran_workers <- function(source = FALSE) {
  stopifnot("source must be one logical value" = is.logical(source) &&
    length(source) == 1L && !is.na(source))
  source_root <- if (source) normalizePath(".", winslash = "/", mustWork = TRUE) else ""
  if (source) pkgload::load_all(source_root, quiet = TRUE, compile = FALSE)
  namespace <- asNamespace("prismrepl")
  start <- get(".start_cran_index", namespace)
  poll <- get(".poll_cran_index", namespace)
  cleanup <- get(".cleanup_cran_workers", namespace)
  read_index <- get(".read_cran_index", namespace)
  owner <- get(".package_index", namespace)
  directory <- tempfile("prism worker \u00e9 ")
  stopifnot("probe directory must be created" = dir.create(directory))
  on.exit({
    cleanup(wait = TRUE)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  cleanup(wait = TRUE)
  repository <- file.path(directory, "repository with spaces")
  contrib <- file.path(repository, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  packages <- c("prismFixtureAlpha", "prismFixture.beta")
  write.dcf(data.frame(Package = packages, Version = "1.0.0", License = "MIT"),
    file.path(contrib, "PACKAGES"))
  repository_path <- normalizePath(repository, winslash = "/", mustWork = TRUE)
  # available.packages() maps file:// directly to a local path, with spaces intact.
  repository_url <- paste0("file://", if (startsWith(repository_path, "/")) "" else "/",
    repository_path)
  destination <- file.path(directory, "cache", "index.txt")

  #' Wait for a Child to Finish, with a Bounded Diagnostic Failure
  #' @param worker Owned worker.
  #' @return NULL invisibly.
  completed <- function(worker) {
    stopifnot("worker must be an environment" = is.environment(worker))
    worker$process$wait(timeout = 10000L)
    if (worker$process$is_alive()) stop("Offline worker did not finish within ten seconds.")
    stopifnot("completed child must have an exit status" =
      length(worker$process$get_exit_status()) == 1L)
    invisible(NULL)
  }

  success <- start(repository_url, destination)
  completed(success)
  if (!identical(success$process$get_exit_status(), 0L))
    stop(paste(readLines(success$log, n = 30L, warn = FALSE), collapse = "\n"))
  stopifnot("child must not publish the persistent cache" = !file.exists(destination),
    "child must stage a validated result" =
      identical(read_index(success$result, repository_url)$names, sort(packages)))
  poll()
  stopifnot("parent must publish validated local-repository names" =
      identical(read_index(destination, repository_url)$names, sort(packages)),
    "success must release its process files" = !dir.exists(success$directory),
    "success must release active ownership" = is.null(owner$worker))
  original <- readBin(destination, "raw", n = file.info(destination)$size)
  cat("PASS: real local-repository download, child-only staging, parent publication and cleanup.\n")

  # An empty local PACKAGES index is a deterministic failure with no URL access.
  writeLines(character(), file.path(contrib, "PACKAGES"))
  failure <- start(repository_url, destination)
  completed(failure)
  stopifnot("empty repository must fail in the child" =
    !identical(failure$process$get_exit_status(), 0L))
  poll()
  stopifnot("failure must retain the earlier valid cache" =
      identical(readBin(destination, "raw", n = file.info(destination)$size), original),
    "failure must be recorded" = isTRUE(owner$failed),
    "failure must remove the job directory" = !dir.exists(failure$directory))
  cat("PASS: real child failure preserves the earlier cache and removes scratch files.\n")

  slow_script <- file.path(directory, "slow worker.R")
  writeLines(c(
    'args <- commandArgs(trailingOnly = TRUE)',
    'task <- as.list(read.dcf(args[[1L]], all = TRUE)[1L, ])',
    'writeLines("ready", file.path(dirname(args[[1L]]), "ready"))',
    'Sys.sleep(30)',
    'writeLines(c(paste0("Repository: ", task$Repository), "latePackage"), task$Result)',
    'writeLines("ok", task$Done)'
  ), slow_script)

  #' Start a Deterministic Slow Worker and Wait Only for Its Ready Marker
  #' @return Owned worker environment.
  slow_worker <- function() {
    worker <- start(repository_url, destination, script = slow_script)
    deadline <- as.numeric(Sys.time()) + 10
    while (!file.exists(file.path(worker$directory, "ready")) &&
           worker$process$is_alive() && as.numeric(Sys.time()) < deadline) Sys.sleep(0.01)
    stopifnot("slow child must reach its ready marker" =
      file.exists(file.path(worker$directory, "ready")) && worker$process$is_alive())
    worker
  }

  cancelled <- slow_worker()
  cleanup(wait = TRUE)
  stopifnot("cancelled child must be stopped before its directory is removed" =
      !cancelled$process$is_alive() && !dir.exists(cancelled$directory),
    "cancellation must preserve the published cache" =
      identical(readBin(destination, "raw", n = file.info(destination)$size), original))
  cat("PASS: explicit cancellation kills and reaps a live child before removing its directory.\n")

  timed_out <- slow_worker()
  elapsed <- system.time(poll(now = timed_out$started + 121))[["elapsed"]]
  cleanup(wait = TRUE)
  stopifnot("timeout must stop the child and remove scratch" =
      !timed_out$process$is_alive() && !dir.exists(timed_out$directory),
    "timeout must be recorded" = identical(owner$failure, "timed out"))
  cat(sprintf("PASS: real timeout cancellation; completion poll took %.3f seconds.\n", elapsed))

  obsolete <- slow_worker()
  owner$key <- "a different repository and destination"
  poll()
  cleanup(wait = TRUE)
  stopifnot("repository change must stop the obsolete process" =
      !obsolete$process$is_alive() && !dir.exists(obsolete$directory),
    "an obsolete job must not overwrite the published cache" =
      identical(readBin(destination, "raw", n = file.info(destination)$size), original))
  cat("PASS: repository switching cancels the previous job without publishing late output.\n")

  exit_script <- file.path(directory, "parent exit.R")
  writeLines(c(
    'task <- as.list(read.dcf(commandArgs(trailingOnly = TRUE)[[1L]], all = TRUE)[1L, ])',
    'if (nzchar(task$Source)) pkgload::load_all(task$Source, quiet = TRUE, compile = FALSE)',
    'ns <- asNamespace("prismrepl")',
    'w <- get(".start_cran_index", ns)(task$Repository, task$Destination, script = task$Script)',
    'deadline <- as.numeric(Sys.time()) + 10',
    'while (!file.exists(file.path(w$directory, "ready")) && w$process$is_alive() &&',
    '       as.numeric(Sys.time()) < deadline) Sys.sleep(0.01)',
    'stopifnot(file.exists(file.path(w$directory, "ready")), w$process$is_alive())',
    'pid <- w$process$get_pid()',
    'saveRDS(list(directory = w$directory, pid = pid,',
    '  created = ps::ps_create_time(ps::ps_handle(pid))), task$Manifest)',
    'if (task$Mode == "unload") {',
    '  unloadNamespace("prismrepl")',
    '  stopifnot(!w$process$is_alive(), !dir.exists(w$directory))',
    '}',
    'quit(save = "no", status = 0L)'
  ), exit_script)
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  invisible(lapply(c("unload", "exit"), function(mode) {
    task <- file.path(directory, paste0(mode, ".dcf"))
    manifest <- file.path(directory, paste0(mode, ".rds"))
    write.dcf(data.frame(Source = source_root, Repository = repository_url,
      Destination = destination, Script = slow_script, Manifest = manifest, Mode = mode),
      task, width = 100000L)
    result <- processx::run(executable, c("--vanilla", exit_script, task),
      timeout = 15, error_on_status = FALSE, windows_hide_window = TRUE)
    if (!identical(result$status, 0L)) stop(paste(result$stdout, result$stderr, sep = "\n"))
    info <- readRDS(manifest)
    alive <- tryCatch(ps::ps_is_running(ps::ps_handle(info$pid, time = info$created)),
      error = function(e) FALSE)
    stopifnot("namespace unload or normal R exit must stop its child" = !alive,
      "namespace unload or normal R exit must remove worker scratch" = !dir.exists(info$directory))
    cat(sprintf("PASS: %s cleanup stops the actual child and removes its directory.\n", mode))
  }))
  stopifnot("probe must finish with no active or retired jobs" =
    is.null(owner$worker) && length(owner$retired) == 0L)
  invisible(TRUE)
}

check_cran_workers(source = "--source" %in% commandArgs(trailingOnly = TRUE))
