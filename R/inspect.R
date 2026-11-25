#' Inspect objects without evaluating deferred bindings
#'
#' Inventory reads ordinary bindings, but never forces promises or invokes
#' active bindings. It does not call user-defined print, length, or dim methods.
#' Object sizes are approximate sizes from [utils::object.size()], not process
#' memory or unique retained memory. Shared storage can be counted more than
#' once and referenced environments or external resources are not included.
#' Set `sizes = FALSE` for a cheaper refresh in a large workspace.
#'
#' @param envir The environment to inspect or modify.
#' @param all.names Include names beginning with a dot.
#' @param sizes Calculate approximate object sizes in bytes.
#' @return A data frame with columns `name`, `type`, `class`, `length`,
#'   `dimensions`, `attributes`, `bytes`, `locked`, and `binding`. Deferred or
#'   active bindings have unavailable metadata represented by `NA` or empty text.
#' @export
#' @examples
#' e <- list2env(list(values = 1:10, table = head(iris)))
#' workspace_objects(e)
workspace_objects <- function(envir = .GlobalEnv, all.names = FALSE,
                              sizes = TRUE) {
  stopifnot("envir must be an environment" = is.environment(envir),
            "all.names must be TRUE or FALSE" = is.logical(all.names) &&
              length(all.names) == 1L && !is.na(all.names),
            "sizes must be TRUE or FALSE" = is.logical(sizes) &&
              length(sizes) == 1L && !is.na(sizes))
  nms <- ls(envir, all.names = all.names, sorted = TRUE)
  empty <- data.frame(name = character(), type = character(), class = character(),
                      length = numeric(), dimensions = character(),
                      attributes = character(), bytes = numeric(),
                      locked = logical(), binding = character(),
                      stringsAsFactors = FALSE, check.names = FALSE)
  states <- binding_info(envir, nms)
  out <- if (!length(nms)) empty else do.call(rbind, lapply(seq_along(nms), function(i) {
    nm <- nms[[i]]
    state <- states[[i]]
    if (state != "value") return(data.frame(
      name = nm, type = state, class = "", length = NA_real_, dimensions = "",
      attributes = "", bytes = NA_real_, locked = bindingIsLocked(nm, envir),
      binding = state, stringsAsFactors = FALSE, check.names = FALSE))
    x <- get(nm, envir, inherits = FALSE)
    attrs <- attributes(x)
    cls <- class(x)
    dims <- attr(x, "dim", exact = TRUE)
    raw_length <- if (isS4(x)) NA_real_ else
      tryCatch(as.double(length(if (is.object(x)) unclass(x) else x)),
               error = function(e) NA_real_)
    if (inherits(x, "data.frame") && !isS4(x)) {
      dims <- c(length(attr(x, "row.names", exact = TRUE)), raw_length)
    }
    bytes <- if (sizes) tryCatch(as.double(utils::object.size(x)),
                                error = function(e) NA_real_) else NA_real_
    data.frame(name = nm, type = typeof(x), class = paste(cls, collapse = "/"),
               length = raw_length,
               dimensions = if (is.null(dims)) "" else paste(dims, collapse = " x "),
               attributes = paste(names(attrs), collapse = ", "), bytes = bytes,
               locked = bindingIsLocked(nm, envir), binding = state,
               stringsAsFactors = FALSE, check.names = FALSE)
  }))
  rownames(out) <- NULL
  stopifnot("inventory must have one row per binding" = nrow(out) == length(nms),
            "inventory must have a stable schema" = identical(names(out), names(empty)),
            "available sizes must be nonnegative" = all(is.na(out$bytes) | out$bytes >= 0))
  out
}

#' List attached packages and loaded namespaces
#'
#' A namespace can remain loaded after its package is detached. The distinction
#' is shown explicitly because namespace unloading can invalidate other objects.
#'
#' @return A data frame with `package`, `version`, `attached`, and `loaded`.
#' @export
#' @examples
#' head(loaded_packages())
loaded_packages <- function() {
  nms <- sort(loadedNamespaces())
  attached <- sub("^package:", "", search()[startsWith(search(), "package:")])
  out <- data.frame(package = nms,
    version = vapply(nms, function(pkg) tryCatch(
      as.character(utils::packageVersion(pkg)), error = function(e) ""), character(1)),
    attached = nms %in% attached, loaded = rep(TRUE, length(nms)),
    stringsAsFactors = FALSE, row.names = NULL)
  stopifnot("packages must have one row per loaded namespace" = nrow(out) == length(nms),
            "package names must be unique" = !anyDuplicated(out$package))
  out
}

#' Remove a selected batch of workspace bindings
#'
#' All names are checked before anything is removed. Missing names, locked
#' bindings, and locked environments cause an error. Active or deferred bindings
#' can be removed without evaluating them. This operation cannot be undone.
#'
#' @param names Character vector of exact object names.
#' @param envir Environment containing the bindings.
#' @return Invisibly, the unique names removed.
#' @export
#' @examples
#' e <- list2env(list(x = 1, y = 2))
#' remove_objects(c("x", "y"), e)
remove_objects <- function(names, envir = .GlobalEnv) {
  stopifnot("names must be nonmissing character strings" = is.character(names) &&
              !anyNA(names) && all(nzchar(names)),
            "envir must be an environment" = is.environment(envir),
            "environment must not be locked" = !environmentIsLocked(envir))
  nms <- unique(names)
  stopifnot("all selected objects must exist in this environment" =
              all(vapply(nms, exists, logical(1), envir = envir, inherits = FALSE)),
            "selected bindings must not be locked" =
              !any(vapply(nms, bindingIsLocked, logical(1), env = envir)))
  rm(list = nms, envir = envir)
  stopifnot("all selected objects must have been removed" =
              !any(vapply(nms, exists, logical(1), envir = envir, inherits = FALSE)))
  invisible(nms)
}

#' Detach selected packages while retaining loaded namespaces
#'
#' Detachment removes packages from the search path. Namespaces are deliberately
#' kept loaded, as other packages or existing objects may still need them. Core
#' default packages and prismrepl cannot be detached with this helper. Packages
#' that are already detached are reported and left unchanged.
#'
#' @param packages Character vector of package names, optionally prefixed with
#'   `package:`.
#' @return A data frame with `package`, `status`, and `detail`; failures for an
#'   individual package are reported without hiding successful detachments.
#' @export
#' @examples
#' detach_packages(character())
detach_packages <- function(packages) {
  stopifnot("packages must be nonmissing character strings" =
              is.character(packages) && !anyNA(packages) && all(nzchar(packages)))
  nms <- unique(sub("^package:", "", packages))
  stopifnot("package names must not be empty" = all(nzchar(nms)))
  protected <- c("base", "stats", "utils", "methods", "graphics", "grDevices",
                 "datasets", "prismrepl")
  stopifnot("core default packages and prismrepl cannot be detached here" =
              !any(nms %in% protected))
  out <- if (!length(nms)) data.frame(package = character(), status = character(),
    detail = character(), stringsAsFactors = FALSE) else do.call(rbind, lapply(nms, function(pkg) {
      entry <- paste0("package:", pkg)
      if (!entry %in% search()) return(data.frame(package = pkg,
        status = "not attached", detail = "Namespace, if loaded, is unchanged.",
        stringsAsFactors = FALSE))
      error <- tryCatch({
        detach(entry, character.only = TRUE, unload = FALSE, force = FALSE)
        NULL
      }, error = function(e) conditionMessage(e))
      data.frame(package = pkg, status = if (is.null(error)) "detached" else "failed",
                 detail = if (is.null(error)) "Namespace remains loaded." else error,
                 stringsAsFactors = FALSE)
    }))
  rownames(out) <- NULL
  stopifnot("each package must have one result" = nrow(out) == length(nms),
            "statuses must be recognized" =
              all(out$status %in% c("not attached", "detached", "failed")))
  out
}

#' Produce a bounded object preview
#'
#' Returns terminal-safe text for vectors, tables, matrices, lists, functions,
#' environments, and other objects. Table previews contain at most `rows` rows
#' and `columns` columns. Lists use a bounded structural summary. A custom
#' object's explicit preview may invoke its `str`, `format`, or subset methods;
#' passive workspace inventory does not. The result is text, with no graphics.
#'
#' @param x The object to preview.
#' @param width Maximum display width, in terminal columns.
#' @param rows Maximum rows of tabular data or elements of a vector to preview.
#' @param depth Maximum list depth for structural summaries.
#' @param columns Maximum number of table or matrix columns.
#' @return A character vector of lines, limited to at most `rows * 4 + 12` lines.
#' @export
#' @examples
#' cat(preview_object(iris, rows = 4), sep = "\n")
#' preview_object(list(a = 1:10, b = list(text = "Unicode: \u03b1\u03b2")))
preview_object <- function(x, width = 80L, rows = 8L, depth = 2L,
                           columns = 8L) {
  stopifnot("width must be a finite scalar at least 20" = is.numeric(width) &&
              length(width) == 1L && is.finite(width) && width >= 20,
            "rows must be a positive integer" = is.numeric(rows) &&
              length(rows) == 1L && is.finite(rows) && rows >= 1 && rows == floor(rows),
            "depth must be a nonnegative integer" = is.numeric(depth) &&
              length(depth) == 1L && is.finite(depth) && depth >= 0 && depth == floor(depth),
            "columns must be a positive integer" = is.numeric(columns) &&
              length(columns) == 1L && is.finite(columns) && columns >= 1 &&
              columns == floor(columns))
  width <- as.integer(min(width, 10000L))
  rows <- as.integer(min(rows, 1000L))
  columns <- as.integer(min(columns, 100L))
  depth <- as.integer(min(depth, 10L))
  header <- paste0(paste(class(x), collapse = "/"), "  <", typeof(x), ">")
  lines <- tryCatch({
    if (is.data.frame(x) || is.matrix(x)) {
      dims <- dim(x)
      nr <- min(rows, dims[[1L]])
      nc <- min(columns, dims[[2L]])
      sample <- x[seq_len(nr), seq_len(nc), drop = FALSE]
      body <- utils::capture.output(print(sample, row.names = TRUE,
        max = max(1L, nr * nc), right = FALSE))
      c(paste0(header, "  ", dims[[1L]], " x ", dims[[2L]]), body,
        if (dims[[1L]] > nr || dims[[2L]] > nc)
          sprintf("Showing %d of %d rows and %d of %d columns.",
                  nr, dims[[1L]], nc, dims[[2L]]))
    } else if (is.environment(x)) {
      nms <- ls(x, all.names = TRUE, sorted = TRUE)
      c(header, paste0("Environment: ", environmentName(x)),
        paste0(length(nms), " bindings; values are not evaluated."),
        head(nms, rows), if (length(nms) > rows) "...")
    } else if (is.function(x)) {
      c(header, head(deparse(x, width.cutoff = min(500L, width)), rows * 2L))
    } else if (is.atomic(x) && is.null(dim(x))) {
      n <- length(x)
      body <- utils::capture.output(print(head(x, rows), max = rows))
      c(paste0(header, "  length ", n), body,
        if (n > rows) paste0("Showing ", rows, " of ", n, " elements."))
    } else {
      c(header, utils::capture.output(utils::str(x, max.level = depth,
        vec.len = min(rows, 10L), list.len = rows, give.attr = TRUE,
        strict.width = "cut", width = width)))
    }
  }, error = function(e) c(header, paste0("Preview unavailable: ", conditionMessage(e))))
  lines <- .safe_text(lines)
  lines <- unlist(strsplit(lines, "\n", fixed = TRUE), use.names = FALSE)
  max_lines <- rows * 4L + 12L
  if (length(lines) > max_lines) lines <- c(head(lines, max_lines - 1L), "...")
  out <- vapply(lines, .preview_clip, character(1), width = width, USE.NAMES = FALSE)
  stopifnot("preview must be text without missing values" = is.character(out) && !anyNA(out),
            "preview must respect its line budget" = length(out) <= max_lines)
  out
}

#' Clip text at a grapheme boundary
#' @param text A scalar display string.
#' @param width Width in terminal columns.
#' @return One string no wider than `width` under the utf8 width model.
#' @keywords internal
#' @noRd
.preview_clip <- function(text, width) {
  stopifnot("text must be one string" = is.character(text) && length(text) == 1L && !is.na(text),
            "width must be positive" = is.numeric(width) && length(width) == 1L && width > 0)
  if (utf8::utf8_width(text, encode = FALSE, utf8 = TRUE) <= width) return(text)
  chars <- stringi::stri_split_boundaries(text, type = "character")[[1L]]
  widths <- utf8::utf8_width(chars, encode = FALSE, utf8 = TRUE)
  out <- paste0(paste0(chars[cumsum(widths) <= max(0, width - 3L)], collapse = ""), "...")
  stopifnot("clipped text must be a scalar" = length(out) == 1L)
  out
}

#' Read local Git working-tree information
#'
#' Uses installed Git to run local `rev-parse` and `status` commands with a
#' short timeout. It does not invoke fetch, pull, or push. Git may invoke
#' helpers configured by the repository or user. Dirty status counts tracked
#' files; untracked files are excluded to keep the prompt responsive. Call
#' after commands and cache the result instead of calling on every key press.
#'
#' @param path Directory to inspect.
#' @return A list with `available`, `is_repo`, `root`, `branch`, `dirty`, `ahead`,
#'   `behind`, `tracked_changes`, and `error`. Once a repository is identified,
#'   `is_repo` and `root` are retained if its status is unavailable. Missing
#'   metadata is represented by empty strings or `NA`.
#' @export
#' @examples
#' git_info(tempdir())
git_info <- function(path = getwd()) {
  stopifnot("path must name one existing directory" = is.character(path) &&
              length(path) == 1L && !is.na(path) && dir.exists(path))
  git <- Sys.which("git")
  out <- list(available = nzchar(git), is_repo = FALSE, root = "", branch = "",
              dirty = NA, ahead = NA_integer_, behind = NA_integer_,
              tracked_changes = NA_integer_, error = "")
  if (!out$available) return(out)
  args <- c("-C", shQuote(normalizePath(path, winslash = "/", mustWork = TRUE)),
            "-c", "color.ui=false")
  run <- function(extra) {
    ans <- suppressWarnings(tryCatch(system2(git, c(args, extra),
      stdout = TRUE, stderr = FALSE, timeout = 2), error = function(e) structure(
        character(), status = 1L)))
    if (!is.null(attr(ans, "status")) && attr(ans, "status") != 0L) character() else ans
  }
  root <- run(c("rev-parse", "--show-toplevel"))
  if (!length(root)) return(out)
  out$is_repo <- TRUE
  out$root <- root[[1L]]
  status <- run(c("--no-optional-locks", "status", "--porcelain=v1", "--branch",
                  "--untracked-files=no"))
  if (!length(status)) {
    out$error <- "Git status unavailable or timed out."
    return(out)
  }
  branchline <- sub("^## ", "", status[[1L]])
  branch <- sub("\\.\\.\\..*$", "", branchline)
  branch <- sub("^(No commits yet on |Initial commit on )", "", branch)
  changes <- status[!startsWith(status, "## ")]
  count <- function(word) {
    hit <- regexec(paste0(word, " ([0-9]+)"), branchline)
    result <- regmatches(branchline, hit)[[1L]]
    if (length(result) == 2L) as.integer(result[[2L]]) else 0L
  }
  out$branch <- branch
  out$dirty <- length(changes) > 0L
  out$ahead <- count("ahead")
  out$behind <- count("behind")
  out$tracked_changes <- length(changes)
  stopifnot("Git result must report scalar state" = length(out$is_repo) == 1L &&
              length(out$branch) == 1L,
            "change count must be nonnegative" = out$tracked_changes >= 0)
  out
}
