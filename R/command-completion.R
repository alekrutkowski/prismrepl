.package_index <- new.env(parent = emptyenv())

#' Resolve Repository Settings Without Opening a Mirror Menu
#' @return Named repository URLs, honoring existing non-placeholder settings.
#' @noRd
.installation_repositories <- function() {
  out <- getOption("repos")
  if (!is.character(out) || !length(out)) out <- c(CRAN = "https://cloud.r-project.org")
  out[is.na(out) | !nzchar(out) | out == "@CRAN@"] <- "https://cloud.r-project.org"
  stopifnot("repositories must be nonempty text" = is.character(out) && length(out) > 0L &&
              !anyNA(out) && all(nzchar(out)))
  out
}

#' Locate the CRAN Repository Used for Name Completion
#' @return One repository URL.
#' @noRd
.cran_repository <- function() {
  repositories <- .installation_repositories()
  out <- if ("CRAN" %in% names(repositories)) unname(repositories[["CRAN"]]) else
    "https://cloud.r-project.org"
  stopifnot("CRAN repository must be a URL" = .is_text(out))
  sub("/+$", "", out)
}

#' Read a Bounded Plain-Text Package Index
#' @param path Index path.
#' @param repository Expected repository.
#' @return Package names and modification time, or NULL for an invalid file.
#' @noRd
.read_cran_index <- function(path, repository) {
  stopifnot("path and repository must be text" = .is_text(path) && .is_text(repository))
  if (!file.exists(path)) return(NULL)
  info <- file.info(path)
  if (is.na(info$size) || info$size > 4 * 1024^2 || isTRUE(info$isdir)) return(NULL)
  lines <- tryCatch(readLines(path, n = 100002L, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  if (length(lines) < 2L || length(lines) > 100001L ||
      !identical(lines[[1L]], paste0("Repository: ", repository))) return(NULL)
  packages <- lines[-1L]
  if (anyNA(packages) || any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", packages))) return(NULL)
  out <- list(names = sort(unique(packages)), modified = as.numeric(info$mtime))
  stopifnot("cached packages must be text" = is.character(out$names) && !anyNA(out$names))
  out
}

#' Reuse Parsed CRAN Names Until the Cache File Changes
#' @param path Plain-text cache path.
#' @param repository Expected repository URL.
#' @return The validated index or NULL. Steady-state completion only stats the
#'   file; it does not re-read or re-sort thousands of package names.
#' @noRd
.cached_cran_index <- function(path, repository) {
  stopifnot("path and repository must be text" = .is_text(path) && .is_text(repository))
  info <- file.info(path)
  signature <- list(path = path, repository = repository,
    size = info$size, modified = as.numeric(info$mtime), directory = info$isdir)
  if (!identical(.package_index$cache_signature, signature)) {
    .package_index$cached <- .read_cran_index(path, repository)
    .package_index$cache_signature <- signature
  }
  out <- .package_index$cached
  stopifnot("in-memory index must be NULL or validated names" = is.null(out) ||
              (is.list(out) && is.character(out$names) && !anyNA(out$names)))
  out
}

#' Test Whether an Owned Index Process Is Still Running
#' @param worker An owned worker environment.
#' @return A logical scalar. An inspection error conservatively means alive.
#' @noRd
.cran_worker_alive <- function(worker) {
  stopifnot("worker must be an environment" = is.environment(worker))
  out <- !is.null(worker$process) &&
    tryCatch(isTRUE(worker$process$is_alive()), error = function(e) TRUE)
  stopifnot("worker liveness must be logical" = .is_flag(out))
  out
}

#' Stop and Reap an Index Worker Before Removing Its Files
#' @param worker An owned worker environment.
#' @param wait Allow a bounded wait during shutdown, never during completion.
#' @return Whether the process has exited and its directory was removed.
#' @noRd
.dispose_cran_worker <- function(worker, wait = FALSE) {
  stopifnot("worker must own a directory" = is.environment(worker) && .is_text(worker$directory),
            "wait must be logical" = .is_flag(wait))
  if (.cran_worker_alive(worker))
    try(worker$process$kill(grace = if (wait) 0.1 else 0), silent = TRUE)
  if (!is.null(worker$process))
    try(worker$process$wait(timeout = if (wait) 1000L else 0L), silent = TRUE)
  if (.cran_worker_alive(worker)) return(FALSE)
  unlink(worker$directory, recursive = TRUE)
  out <- !dir.exists(worker$directory)
  stopifnot("worker disposal must return logical" = .is_flag(out))
  out
}

#' Reap Previously Cancelled Index Processes Without Blocking Tab
#' @param wait Allow a bounded shutdown wait.
#' @param state The private index ownership environment.
#' @return The number of jobs still awaiting cleanup, invisibly.
#' @noRd
.reap_cran_workers <- function(wait = FALSE, state = .package_index) {
  stopifnot("wait must be logical" = .is_flag(wait), "state must be an environment" = is.environment(state))
  workers <- state$retired
  if (length(workers)) {
    removed <- vapply(workers, function(worker)
      tryCatch(.dispose_cran_worker(worker, wait), error = function(e) FALSE), logical(1L))
    state$retired <- workers[!removed]
  }
  out <- length(state$retired)
  stopifnot("remaining job count must be nonnegative" = .is_count(out))
  invisible(out)
}

#' Cancel All Owned Index Jobs and Retain Ownership Until Reaped
#' @param wait Allow a bounded shutdown wait.
#' @param state The private index ownership environment.
#' @return The number of jobs still awaiting cleanup, invisibly.
#' @noRd
.cleanup_cran_workers <- function(wait = FALSE, state = .package_index) {
  stopifnot("wait must be logical" = .is_flag(wait), "state must be an environment" = is.environment(state))
  suspendInterrupts({
    if (is.environment(state$worker)) {
      state$retired <- c(state$retired, list(state$worker))
      state$worker <- NULL
    }
    out <- .reap_cran_workers(wait, state)
  })
  stopifnot("remaining job count must be nonnegative" = .is_count(out))
  invisible(out)
}

#' Register Normal-Exit Cleanup for the Index Owner
#' @param state The private index ownership environment.
#' @return NULL invisibly.
#' @noRd
.register_cran_cleanup <- function(state = .package_index) {
  stopifnot("state must be an environment" = is.environment(state))
  if (!isTRUE(state$finalizer_registered)) {
    reg.finalizer(state, function(owner)
      try(.cleanup_cran_workers(wait = TRUE, state = owner), silent = TRUE), onexit = TRUE)
    state$finalizer_registered <- TRUE
  }
  stopifnot("index cleanup must be registered" = isTRUE(state$finalizer_registered))
  invisible(NULL)
}

#' Launch a Supervised Index Process Without Shell Argument Parsing
#' @param executable Rscript executable.
#' @param script Worker script path.
#' @param task Task file path.
#' @param log Combined output path.
#' @return A processx process handle.
#' @noRd
.new_cran_process <- function(executable, script, task, log) {
  stopifnot("worker paths must be nonempty strings" =
    all(vapply(list(executable, script, task, log), .is_text, logical(1L))))
  out <- processx::process$new(executable, c("--vanilla", script, task),
    stdout = log, stderr = "2>&1", cleanup = TRUE, supervise = TRUE,
    windows_hide_window = TRUE)
  stopifnot("worker must have a managed process handle" =
    is.environment(out) && is.function(out$is_alive) && is.function(out$kill))
  out
}

#' Start and Own One Background CRAN Index Download
#' @param repository Repository URL.
#' @param destination Plain index file, written only by the parent.
#' @param script Installed worker script; replaceable for offline lifecycle tests.
#' @return An owned worker environment invisibly.
#' @noRd
.start_cran_index <- function(repository, destination,
                              script = system.file("scripts", "cran-index.R", package = "prismrepl")) {
  stopifnot("repository and destination must be text" = .is_text(repository) && .is_text(destination),
            "only one index download may be active" = is.null(.package_index$worker))
  stopifnot("the CRAN index worker must be installed" = .is_text(script) && file.exists(script))
  .register_cran_cleanup()
  worker <- new.env(parent = emptyenv())
  worker$directory <- tempfile("prism-cran-")
  worker$task <- file.path(worker$directory, "task.dcf")
  worker$log <- file.path(worker$directory, "worker.log")
  worker$done <- file.path(worker$directory, "done.txt")
  worker$result <- file.path(worker$directory, "index.txt")
  worker$repository <- repository
  worker$destination <- destination
  worker$key <- paste(repository, destination, sep = "\n")
  worker$started <- as.numeric(Sys.time())
  transferred <- FALSE
  on.exit(if (!transferred) {
    .package_index$retired <- c(.package_index$retired, list(worker))
    .reap_cran_workers(wait = TRUE)
  }, add = TRUE)
  if (!dir.create(worker$directory, mode = "0700"))
    stop("Cannot create the package-index worker directory.", call. = FALSE)
  write.dcf(data.frame(Repository = repository, Result = worker$result, Done = worker$done,
                       stringsAsFactors = FALSE), file = worker$task, width = 100000L)
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  # No interrupt may strand a returned process between creation and ownership.
  suspendInterrupts({
    worker$process <- .new_cran_process(executable, script, worker$task, worker$log)
    .package_index$key <- worker$key
    .package_index$worker <- worker
    transferred <- TRUE
  })
  stopifnot("the active worker must own its process and scratch directory" =
    identical(.package_index$worker, worker) && dir.exists(worker$directory))
  invisible(worker)
}

#' Replace a Validated Cache Through a Staged File
#' @param packages Validated package names.
#' @param repository Repository URL.
#' @param destination Cache file path.
#' @return The destination invisibly.
#' @details POSIX rename replaces the file atomically. Windows may need two
#'   renames with a brief missing destination; an error rolls the old file back.
#'   A failed rollback retains its recovery file and reports its location.
#' @noRd
.write_cran_cache <- function(packages, repository, destination) {
  stopifnot("package names must be valid nonempty text" = is.character(packages) &&
              length(packages) > 0L && !anyNA(packages) &&
              all(grepl("^[A-Za-z][A-Za-z0-9.]*$", packages)),
            "repository and destination must be text" = .is_text(repository) && .is_text(destination))
  directory <- dirname(destination)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE))
    stop("Cannot create the package-name cache directory.", call. = FALSE)
  if (dir.exists(destination)) stop("The package-name cache path is a directory.", call. = FALSE)
  staged <- tempfile(".prism-cran-index-", tmpdir = directory)
  backup <- tempfile(".prism-cran-backup-", tmpdir = directory)
  moved <- saved <- FALSE
  on.exit({
    restored <- TRUE
    if (moved && !saved && file.exists(backup)) {
      restored <- suppressWarnings(file.rename(backup, destination))
    }
    unlink(staged)
    if (saved || !moved) unlink(backup)
    if (!isTRUE(restored)) warning(paste("The previous CRAN cache is retained at", backup), call. = FALSE)
  }, add = TRUE)
  writeLines(c(paste0("Repository: ", repository), packages), staged, useBytes = TRUE)
  suspendInterrupts({
    saved <- suppressWarnings(file.rename(staged, destination))
    if (!saved && file.exists(destination)) {
      moved <- suppressWarnings(file.rename(destination, backup))
      if (moved) saved <- suppressWarnings(file.rename(staged, destination))
    }
  })
  if (!saved) stop("Cannot publish the CRAN index; the previous cache was retained for recovery.", call. = FALSE)
  stopifnot("the published index must exist" = file.exists(destination))
  invisible(destination)
}

#' Publish Only the Completed Result Still Owned by This Repository
#' @param worker The completed worker environment.
#' @param state The private index ownership environment.
#' @return TRUE for publication, FALSE for an obsolete job.
#' @noRd
.publish_cran_index <- function(worker, state = .package_index) {
  stopifnot("worker and state must be environments" = is.environment(worker) && is.environment(state))
  if (!identical(state$worker, worker) || !identical(state$key, worker$key)) return(FALSE)
  stopifnot("a running worker cannot publish" = !.cran_worker_alive(worker))
  index <- .read_cran_index(worker$result, worker$repository)
  if (is.null(index)) stop("The downloaded CRAN index failed validation.", call. = FALSE)
  .write_cran_cache(index$names, worker$repository, worker$destination)
  state$cache_signature <- NULL
  stopifnot("published cache must be a file" = file.exists(worker$destination))
  TRUE
}

#' Poll a Managed Index Job Without Waiting for Network I/O
#' @param state The private index ownership environment.
#' @param now Current time in seconds, injectable for timeout tests.
#' @return NULL invisibly.
#' @noRd
.poll_cran_index <- function(state = .package_index, now = as.numeric(Sys.time())) {
  stopifnot("state must be an environment" = is.environment(state),
            "now must be finite scalar seconds" = is.numeric(now) && length(now) == 1L && is.finite(now))
  worker <- state$worker
  if (!is.environment(worker)) return(invisible(NULL))
  if (!identical(worker$key, state$key)) {
    .cleanup_cran_workers(state = state)
    return(invisible(NULL))
  }
  if (.cran_worker_alive(worker)) {
    if (now - worker$started > 120) {
      state$failed <- TRUE
      state$failure <- "timed out"
      .cleanup_cran_workers(state = state)
    }
    return(invisible(NULL))
  }
  result <- tryCatch({
    exit <- worker$process$get_exit_status()
    status <- if (file.exists(worker$done) && file.info(worker$done)$size <= 4096)
      readLines(worker$done, n = 1L, warn = FALSE, encoding = "UTF-8") else character()
    if (!identical(exit, 0L) || !identical(status, "ok"))
      stop(if (length(status)) status else "The CRAN index worker exited without a result.", call. = FALSE)
    .publish_cran_index(worker, state)
    "ok"
  }, error = function(e) conditionMessage(e))
  state$failed <- !identical(result, "ok")
  state$failure <- if (state$failed) .safe_text(result) else ""
  .cleanup_cran_workers(state = state)
  stopifnot("completion must release the active job" = is.null(state$worker))
  invisible(NULL)
}

#' Obtain CRAN Names Without Blocking Keystrokes on Network I/O
#' @param refresh Explicitly request a refreshed index.
#' @param start Start a worker if no fresh cache is available.
#' @return A list of package names and a short completion status.
#' @noRd
.cran_package_names <- function(refresh = FALSE, start = TRUE) {
  stopifnot("refresh and start must be logical" = .is_flag(refresh) && .is_flag(start))
  repository <- .cran_repository()
  path <- file.path(tools::R_user_dir("prismrepl", "cache"), "cran-packages.txt")
  key <- paste(repository, path, sep = "\n")
  .reap_cran_workers()
  if (!identical(.package_index$key, key)) {
    .cleanup_cran_workers()
    .package_index$key <- key
    .package_index$failed <- FALSE
    .package_index$failure <- ""
  }
  .poll_cran_index()
  cached <- .cached_cran_index(path, repository)
  stale <- is.null(cached) || as.numeric(Sys.time()) - cached$modified > 24 * 3600
  if (refresh) .package_index$failed <- FALSE
  if (start && (refresh || stale) && is.null(.package_index$worker) && !isTRUE(.package_index$failed)) {
    attempt <- tryCatch(.start_cran_index(repository, path), error = identity)
    if (inherits(attempt, "error")) {
      .package_index$failed <- TRUE
      .package_index$failure <- .safe_text(conditionMessage(attempt))
    }
  }
  downloading <- is.environment(.package_index$worker)
  status <- if (downloading) "Updating CRAN package names in the background; press Tab again shortly." else
    if (isTRUE(.package_index$failed)) paste0("CRAN index unavailable", if (!is.null(cached)) "; using cached names" else "",
      ". Use :install --refresh to retry.") else if (is.null(cached)) "No cached CRAN package index." else ""
  out <- list(names = if (is.null(cached)) character() else cached$names, status = status)
  stopifnot("CRAN completion must return names and status" = is.character(out$names) &&
              !anyNA(out$names) && is.character(out$status) && length(out$status) == 1L)
  out
}

#' Complete an Entire Unquoted File Path
#' @param before Input prefix ending at the cursor.
#' @param offset Zero-based start of the literal path.
#' @param cursor Zero-based cursor.
#' @param directories Only return directories.
#' @param limit Maximum candidates.
#' @return A standard completion span.
#' @noRd
.complete_command_path <- function(before, offset, cursor, directories, limit) {
  stopifnot("input must be scalar text" = is.character(before) && length(before) == 1L && !is.na(before),
            "offset and cursor must be ordered" = .is_count(offset) && .is_count(cursor, offset),
            "directories must be logical" = .is_flag(directories), "limit must be positive" = .is_count(limit, 1L))
  literal <- substr(before, offset + 1L, cursor)
  quote <- if (substr(literal, 1L, 1L) %in% c("'", '"', "`")) substr(literal, 1L, 1L) else ""
  path <- if (nzchar(quote)) substring(literal, 2L) else literal
  if (nzchar(quote) && endsWith(path, quote)) path <- substr(path, 1L, nchar(path) - 1L)
  # Win32 APIs accept one backslash; no R string parsing is involved here.
  separator <- if (.Platform$OS.type == "windows" && grepl("\\", path, fixed = TRUE)) "\\" else "/"
  normalized <- if (.Platform$OS.type == "windows") gsub("\\", "/", path, fixed = TRUE) else path
  slash <- gregexpr("/", normalized, fixed = TRUE)[[1L]]
  last <- if (slash[[1L]] < 0L) 0L else utils::tail(slash, 1L)
  prefix <- if (last) substr(normalized, 1L, last) else ""
  leaf <- substring(normalized, last + 1L)
  directory <- if (nzchar(prefix)) path.expand(prefix) else "."
  candidates <- tryCatch(list.files(directory, all.files = startsWith(leaf, "."),
    no.. = TRUE, full.names = FALSE), error = function(e) character())
  candidates <- candidates[startsWith(candidates, leaf)]
  isdir <- dir.exists(file.path(directory, candidates))
  if (directories) { candidates <- candidates[isdir]; isdir <- rep(TRUE, length(candidates)) }
  order <- order(!isdir, tolower(candidates), candidates)
  candidates <- candidates[order]
  isdir <- isdir[order]
  candidates <- paste0(prefix, candidates, ifelse(isdir, "/", ""))
  if (separator == "\\") candidates <- gsub("/", "\\", candidates, fixed = TRUE)
  candidates <- paste0(quote, candidates, ifelse(isdir | !nzchar(quote), "", quote))
  out <- list(start = as.integer(offset), end = as.integer(cursor), candidates = utils::head(candidates, limit))
  stopifnot("path candidates must be text" = is.character(out$candidates) && !anyNA(out$candidates))
  out
}

#' Complete Console Commands and Their Arguments
#' @param text Full editor text.
#' @param cursor Code-point cursor position.
#' @param envir Workspace environment.
#' @param limit Maximum candidates.
#' @return A completion record, or NULL for ordinary R input.
#' @noRd
.complete_command <- function(text, cursor, envir, limit, commands_file = repl_commands_file()) {
  stopifnot("text must be a scalar string" = is.character(text) && length(text) == 1L && !is.na(text),
            "cursor must be in the text" = .is_count(cursor) && cursor <= nchar(text),
            "workspace must be an environment" = is.environment(envir), "limit must be positive" = .is_count(limit, 1L))
  before <- substr(text, 1L, cursor)
  if (!grepl("^[[:blank:]]*:", before)) return(NULL)
  catalog <- .command_catalog(commands_file)
  leading <- nchar(before) - nchar(sub("^[[:blank:]]*", "", before))
  head <- substring(before, leading + 1L)
  if (!grepl("[[:blank:]]", head)) {
    token <- tolower(substring(head, 2L))
    pool <- c(catalog$id, catalog$alias[nzchar(catalog$alias)])
    pool <- paste0(":", sort(unique(pool[startsWith(pool, token)])))
    return(list(start = as.integer(leading), end = as.integer(cursor), candidates = utils::head(pool, limit)))
  }
  command <- tolower(sub("^:([^[:blank:]]+).*", "\\1", head))
  id <- match(command, catalog$id)
  if (is.na(id)) id <- match(command, catalog$alias)
  command <- if (!is.na(id)) catalog$id[[id]] else command
  delimiter <- regexpr("[[:blank:]]+", head)
  offset <- leading + as.integer(delimiter) + attr(delimiter, "match.length") - 1L
  if (command %in% c("source", "cd", "files"))
    return(.complete_command_path(before, offset, cursor, command == "cd", limit))
  args <- substr(before, offset + 1L, cursor)
  found <- stringi::stri_locate_last_regex(args, "[^[:blank:]]+$")
  start <- if (is.na(found[[1L]])) cursor else offset + found[[1L]] - 1L
  token <- substr(before, start + 1L, cursor)
  note <- ""
  pool <- switch(command,
    commands = .custom_command_catalog(commands_file)$id,
    attach = base::.packages(all.available = TRUE),
    docs = base::.packages(all.available = TRUE),
    install = {
      index <- .cran_package_names()
      note <- index$status
      c(index$names, "--refresh")
    },
    detach = sub("^package:", "", search()[startsWith(search(), "package:")]),
    view = ls(envir, all.names = TRUE), objects = ls(envir, all.names = TRUE),
    rm = ls(envir, all.names = TRUE), edit = .completion_names(envir),
    theme = c("auto", repl_themes()), status = c("process_memory", "directory", "git", "objects", "packages", "memory", "none"),
    ui = c("fullscreen", "inline"), sidebar = c("on", "off"), mouse = c("on", "off"),
    rainbow = c("on", "off", "toggle"), autopair = c("on", "off", "toggle"),
    match = c("on", "off", "toggle"),
    ai = c(repl_ai_providers()$provider, "custom"),
    settings = "reset", explain = c("copy", "minimal"), pipe = c("|>", "%>%"),
    bind = c(names(.default_keybindings()), "reset"), unbind = names(.default_keybindings()), character())
  pool <- sort(unique(pool[!is.na(pool) & startsWith(tolower(pool), tolower(token))]))
  out <- list(start = as.integer(start), end = as.integer(cursor), candidates = utils::head(pool, limit), status = note)
  stopifnot("command completion must have valid candidates" = is.character(out$candidates) &&
              !anyNA(out$candidates) && out$start <= out$end && out$end <= nchar(text))
  out
}
