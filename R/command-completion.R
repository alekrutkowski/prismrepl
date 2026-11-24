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

#' Start One Background CRAN Index Download
#' @param repository Repository URL.
#' @param destination Plain index file.
#' @return Worker task paths invisibly.
#' @noRd
.start_cran_index <- function(repository, destination) {
  stopifnot("repository and destination must be text" = .is_text(repository) && .is_text(destination))
  directory <- dirname(destination)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE))
    stop("Cannot create the package-name cache directory.", call. = FALSE)
  script <- system.file("scripts", "cran-index.R", package = "prismrepl")
  stopifnot("the CRAN index worker must be installed" = nzchar(script) && file.exists(script))
  task <- tempfile("prism-cran-", fileext = ".dcf")
  log <- paste0(task, ".log")
  done <- paste0(task, ".done")
  write.dcf(data.frame(Repository = repository, Destination = destination, Done = done,
                       stringsAsFactors = FALSE), file = task)
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  status <- system2(executable, c("--vanilla", shQuote(script), shQuote(task)),
                    stdout = log, stderr = log, wait = FALSE)
  if (!identical(as.integer(status), 0L)) stop("Cannot start the CRAN package index download.", call. = FALSE)
  out <- list(task = task, log = log, done = done, started = as.numeric(Sys.time()))
  stopifnot("worker must have completion and log paths" = .is_text(out$done) && .is_text(out$log))
  invisible(out)
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
  if (!identical(.package_index$key, key)) {
    .package_index$key <- key
    .package_index$worker <- NULL
    .package_index$failed <- FALSE
    .package_index$failure <- ""
  }
  worker <- .package_index$worker
  if (is.list(worker) && (file.exists(worker$done) || as.numeric(Sys.time()) - worker$started > 120)) {
    result <- if (file.exists(worker$done)) tryCatch(readLines(worker$done, n = 1L, warn = FALSE),
      error = function(e) "failed") else "timed out"
    .package_index$failed <- !identical(result, "ok")
    .package_index$failure <- if (.package_index$failed) .safe_text(paste(result, collapse = " ")) else ""
    unlink(c(worker$task, worker$log, worker$done))
    .package_index$worker <- NULL
  }
  cached <- .cached_cran_index(path, repository)
  stale <- is.null(cached) || as.numeric(Sys.time()) - cached$modified > 24 * 3600
  if (refresh) .package_index$failed <- FALSE
  if (start && (refresh || stale) && is.null(.package_index$worker) && !isTRUE(.package_index$failed)) {
    attempt <- tryCatch(.start_cran_index(repository, path), error = identity)
    if (inherits(attempt, "error")) {
      .package_index$failed <- TRUE
      .package_index$failure <- .safe_text(conditionMessage(attempt))
    } else .package_index$worker <- attempt
  }
  downloading <- is.list(.package_index$worker)
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
.complete_command <- function(text, cursor, envir, limit) {
  stopifnot("text must be a scalar string" = is.character(text) && length(text) == 1L && !is.na(text),
            "cursor must be in the text" = .is_count(cursor) && cursor <= nchar(text),
            "workspace must be an environment" = is.environment(envir), "limit must be positive" = .is_count(limit, 1L))
  before <- substr(text, 1L, cursor)
  if (!grepl("^[[:blank:]]*:", before)) return(NULL)
  catalog <- .command_catalog()
  leading <- nchar(before) - nchar(sub("^[[:blank:]]*", "", before))
  head <- substring(before, leading + 1L)
  if (!grepl("[[:blank:]]", head)) {
    token <- tolower(substring(head, 2L))
    pool <- c(catalog$id, catalog$alias)
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
    attach = rownames(utils::installed.packages(fields = NULL)),
    docs = rownames(utils::installed.packages(fields = NULL)),
    install = {
      index <- .cran_package_names()
      note <- index$status
      c(index$names, "--refresh")
    },
    detach = sub("^package:", "", search()[startsWith(search(), "package:")]),
    view = ls(envir, all.names = TRUE), objects = ls(envir, all.names = TRUE),
    rm = ls(envir, all.names = TRUE), edit = .completion_names(envir),
    theme = c("auto", repl_themes()), status = c("directory", "git", "objects", "packages", "memory", "none"),
    ui = c("fullscreen", "inline"), sidebar = c("on", "off"), mouse = c("on", "off"),
    rainbow = c("on", "off", "toggle"), autopair = c("on", "off", "toggle"),
    ai = c(repl_ai_providers()$provider, "custom"),
    settings = "reset", explain = c("copy", "minimal"), pipe = c("|>", "%>%"),
    bind = c(names(.default_keybindings()), "reset"), unbind = names(.default_keybindings()), character())
  pool <- sort(unique(pool[!is.na(pool) & startsWith(tolower(pool), tolower(token))]))
  out <- list(start = as.integer(start), end = as.integer(cursor), candidates = utils::head(pool, limit), status = note)
  stopifnot("command completion must have valid candidates" = is.character(out$candidates) &&
              !anyNA(out$candidates) && out$start <= out$end && out$end <= nchar(text))
  out
}
