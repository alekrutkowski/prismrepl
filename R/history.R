#' Read the command history
#'
#' History stores whole expressions, including embedded newlines. Reading never
#' creates a directory or changes a file. Persistence is an explicit opt-in:
#' only [save_repl_history()] writes history.
#'
#' @param path A history file, or `NULL` for the package's user data directory.
#' @param limit Maximum number of expressions to retain.
#' @return A UTF-8 character vector, oldest expression first.
#' @export
#' @examples
#' repl_history(tempfile())
repl_history <- function(path = NULL, limit = 1000L) {
  stopifnot("path must be NULL or one non-missing string" =
              is.null(path) || (is.character(path) && length(path) == 1L &&
                                 !is.na(path) && nzchar(path)),
            "limit must be a positive whole number" =
              is.numeric(limit) && length(limit) == 1L && !is.na(limit) &&
              is.finite(limit) && limit >= 1 && limit == as.integer(limit))
  path <- .history_path(path)
  result <- if (!file.exists(path)) character() else {
    info <- file.info(path)
    if (is.na(info$size) || isTRUE(info$isdir) || info$size > 10 * 1024^2)
      stop("History must be a regular file smaller than 10 MiB.", call. = FALSE)
    saved <- readRDS(path)
    stopifnot("history file has an unsupported format" =
                is.list(saved) && identical(saved$version, 1L) &&
                is.character(saved$entries) && !anyNA(saved$entries))
    .history_clean(saved$entries, limit)
  }
  stopifnot("history result must contain valid text" =
              is.character(result) && !anyNA(result) &&
              all(utf8::utf8_valid(result)) && length(result) <= limit)
  result
}

#' Save the command history
#'
#' Save complete expressions in a versioned RDS file. A directory lock prevents
#' concurrent writers from overwriting each other. Existing expressions are
#' merged and duplicate expressions retain their most recent position. The
#' file is replaced through a temporary file in the same directory. If a
#' process terminates while holding the lock, remove its `.lock` directory
#' after checking that no session is writing that file.
#'
#' History can contain passwords or other private data entered as R code.
#' Saving is optional and is never enabled merely by loading this package.
#'
#' @param history A character vector of complete expressions.
#' @param path Destination file. `NULL` uses the package's user data directory.
#' @param limit Maximum number of expressions to retain.
#' @return Invisibly, `TRUE` if saved or `FALSE` if another writer holds the lock.
#' @export
#' @examples
#' path <- tempfile()
#' save_repl_history(c("x <- 1", "x + 1"), path)
#' repl_history(path)
#' unlink(path)
save_repl_history <- function(history, path = NULL, limit = 1000L) {
  stopifnot("history must be a non-missing character vector" =
              is.character(history) && !anyNA(history),
            "history must have valid text encoding" = all(utf8::utf8_valid(history)),
            "path must be NULL or one non-missing string" =
              is.null(path) || (is.character(path) && length(path) == 1L &&
                                 !is.na(path) && nzchar(path)),
            "limit must be a positive whole number" =
              is.numeric(limit) && length(limit) == 1L && !is.na(limit) &&
              is.finite(limit) && limit >= 1 && limit == as.integer(limit))
  path <- .history_path(path)
  directory <- dirname(path)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, mode = "0700"))
    stop("Could not create the history directory.", call. = FALSE)
  lock <- paste0(path, ".lock")
  if (!dir.create(lock, showWarnings = FALSE, mode = "0700")) {
    warning("History was not saved because another writer holds its lock.", call. = FALSE)
    return(invisible(FALSE))
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  entries <- .history_clean(c(repl_history(path, limit), history), limit)
  temporary <- tempfile(".prismrepl-history-", tmpdir = directory)
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(list(version = 1L, entries = entries), temporary, version = 2)
  Sys.chmod(temporary, mode = "0600")
  result <- suppressWarnings(file.rename(temporary, path))
  if (!result && file.exists(path)) {
    backup <- tempfile(".prismrepl-history-backup-", tmpdir = directory)
    if (!file.rename(path, backup))
      stop("Could not replace the history file.", call. = FALSE)
    on.exit(if (file.exists(backup) && !file.exists(path)) file.rename(backup, path), add = TRUE)
    result <- file.rename(temporary, path)
    if (result) unlink(backup) else {
      file.rename(backup, path)
      stop("Could not replace the history file; the original was retained.", call. = FALSE)
    }
  }
  if (!result) stop("Could not write the history file.", call. = FALSE)
  stopifnot("history save result must be TRUE" = isTRUE(result))
  invisible(result)
}

#' Resolve a history path
#' @keywords internal
#' @noRd
.history_path <- function(path) {
  stopifnot("path must be NULL or a string" =
              is.null(path) || (is.character(path) && length(path) == 1L && !is.na(path)))
  result <- if (is.null(path))
    file.path(tools::R_user_dir("prismrepl", "data"), "history.rds") else path.expand(path)
  stopifnot("history path must be non-empty" = is.character(result) &&
              length(result) == 1L && !is.na(result) && nzchar(result))
  result
}

#' Bound and deduplicate history
#' @keywords internal
#' @noRd
.history_clean <- function(history, limit) {
  stopifnot("history must contain strings" = is.character(history) && !anyNA(history),
            "limit must be positive" = length(limit) == 1L && is.finite(limit) && limit > 0)
  result <- enc2utf8(history)
  result <- gsub("\r\n?", "\n", result)
  result <- result[nzchar(trimws(result)) & nchar(result, type = "bytes") <= 1024^2]
  result <- utils::tail(result[!duplicated(result, fromLast = TRUE)], limit)
  result <- rev(rev(result)[cumsum(nchar(rev(result), type = "bytes")) <= 8 * 1024^2])
  stopifnot("cleaned history must be bounded" = is.character(result) &&
              !anyNA(result) && length(result) <= limit)
  unname(result)
}
