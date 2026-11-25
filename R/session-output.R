#' Capture Ordinary R Output for the Full-Screen Transcript
#'
#' A temporary file keeps large explicit prints out of the R heap. Each drain
#' reads at most one MiB, and the screen bounds its retained transcript. Conditions
#' drain preceding output before adding their own styled blocks. Rendering happens
#' outside the owned sink, so terminal control sequences never enter that file.
#'
#' @param action A function accepting the local display configuration.
#' @param config Configuration containing an active screen environment.
#' @param paint Repaint the screen at condition boundaries, at most 30 times per
#'   second. The completed expression is always rendered by the input loop.
#' @param max_bytes Maximum bytes read from each pending output segment.
#' @return The value returned by `action`.
#' @keywords internal
#' @noRd
.with_screen_output <- function(action, config, paint = TRUE, max_bytes = 1024L * 1024L) {
  stopifnot("action must be a function" = is.function(action),
            "config must contain a screen" = inherits(config, "prismrepl_config") &&
              is.environment(config$screen),
            "paint must be a logical scalar" = .is_flag(paint),
            "max_bytes must be an integer of at least 256" = .is_count(max_bytes, 256L))
  screen <- config$screen
  path <- tempfile("prismrepl-output-", fileext = ".txt")
  writer <- file(path, open = "wb", encoding = "UTF-8")
  depth <- sink.number()
  offset <- 0
  last_paint <- -Inf
  owned <- FALSE
  on.exit({
    if (owned && sink.number() > depth) {
      while (sink.number() > depth) sink()
    }
    try(close(writer), silent = TRUE)
    unlink(path)
  }, add = TRUE)
  drain <- function() {
    flush(writer)
    total <- file.info(path)$size
    if (is.na(total) || total <= offset) return(invisible(NULL))
    start <- max(offset, total - max_bytes)
    truncated <- start > offset
    reader <- file(path, open = "rb")
    bytes <- tryCatch({
      seek(reader, where = start, origin = "start")
      readBin(reader, what = "raw", n = as.integer(total - start))
    }, finally = close(reader))
    offset <<- total
    bytes <- bytes[bytes != as.raw(0L)]
    text <- iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = "\uFFFD")
    if (is.na(text)) text <- "[Output contained invalid text.]"
    text <- gsub("\r\n", "\n", text, fixed = TRUE)
    text <- gsub("\r", "\n", text, fixed = TRUE)
    if (truncated) {
      text <- sub("^[^\n]*\n", "", text)
      screen_append(screen, paste0("[Earlier output omitted; retained the last ",
        format(max_bytes, big.mark = ",", scientific = FALSE, trim = TRUE), " bytes.]"), "warning")
    }
    # Keep terminal commands from printed objects out of the screen compositor.
    lines <- strsplit(.safe_text(cli::ansi_strip(text)), "\n", fixed = TRUE)[[1L]]
    if (length(lines)) screen_append(screen, lines, "output")
    invisible(NULL)
  }
  repaint <- function() {
    if (!paint || !isTRUE(screen$active) || sink.number() != depth + 1L) return(invisible(NULL))
    now <- proc.time()[["elapsed"]]
    if (now - last_paint < 1 / 30) return(invisible(NULL))
    last_paint <<- now
    sink()
    owned <<- FALSE
    on.exit({ sink(writer); owned <<- TRUE }, add = TRUE)
    screen_render(screen, config = config)
    invisible(NULL)
  }
  config$output_hook <- function(lines, role) {
    drain()
    screen_append(screen, lines, role, ansi = TRUE)
    repaint()
    invisible(lines)
  }
  sink(writer)
  owned <- TRUE
  answer <- action(config)
  drain()
  if (sink.number() > depth) {
    while (sink.number() > depth) sink()
  }
  owned <- FALSE
  stopifnot("output capture must restore the output sink depth" = sink.number() == depth)
  answer
}

#' Evaluate Code and Update Full-Screen Session State
#' @param code Source text.
#' @param envir Evaluation environment.
#' @param config Console configuration.
#' @return A structured evaluation result.
#' @keywords internal
#' @noRd
.evaluate_session <- function(code, envir, config) {
  stopifnot("code must be nonmissing text" = is.character(code) && !anyNA(code),
            "envir must be an environment" = is.environment(envir),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  if (!is.environment(config$screen) || !isTRUE(config$screen$active))
    return(evaluate_r(code, envir, config, display = TRUE))
  screen <- config$screen
  screen$mode <- "RUNNING"
  screen$last_editor <- NULL
  screen_render(screen, config = config)
  completed <- FALSE
  on.exit({ if (!completed) screen$mode <- "READY" }, add = TRUE)
  result <- .with_screen_output(function(cfg) evaluate_r(code, envir, cfg, display = TRUE), config)
  screen$mode <- if (result$interrupted) "INTERRUPTED" else if (!is.null(result$error)) "ERROR"
    else if (length(result$warnings)) "WARNING" else "READY"
  screen$elapsed <- result$elapsed
  completed <- TRUE
  stopifnot("session evaluation must return structured conditions" = is.list(result) &&
              all(c("error", "warnings", "messages", "elapsed") %in% names(result)))
  result
}

#' Prepare a Compact Sidebar Without Evaluating Workspace Bindings
#' @param envir Evaluation environment.
#' @param config Console configuration.
#' @param inventory Optional shared workspace snapshot for this refresh.
#' @param process_memory Optional resident-byte snapshot shared with the status.
#' @return A named list of character vectors.
#' @keywords internal
#' @noRd
.session_sidebar <- function(envir, config, inventory = NULL, process_memory = NULL) {
  stopifnot("envir must be an environment" = is.environment(envir),
            "config must be a console configuration" = inherits(config, "prismrepl_config"),
            "inventory must be NULL or a data frame" = is.null(inventory) || is.data.frame(inventory))
  if (!isTRUE(config$sidebar)) return(list())
  objects <- if (is.null(inventory)) workspace_objects(envir, all.names = FALSE,
    sizes = "memory" %in% config$status) else inventory
  object_lines <- if (!nrow(objects)) "No workspace objects" else {
    idx <- seq_len(min(nrow(objects), 16L))
    labels <- vapply(idx, function(i) {
      kind <- if (nzchar(objects$class[i])) objects$class[i] else objects$type[i]
      detail <- paste(c(kind, if (nzchar(objects$dimensions[i])) objects$dimensions[i]), collapse = " ")
      size <- if ("memory" %in% config$status && !is.na(objects$bytes[i]))
        paste0(" ~", format(structure(objects$bytes[i], class = "object_size"), units = "auto")) else ""
      paste0(objects$name[i], "  ", detail, size)
    }, character(1L))
    c(labels, if (nrow(objects) > length(idx)) paste0("+", nrow(objects) - length(idx), " more (:objects)"))
  }
  packages <- sub("^package:", "", search()[startsWith(search(), "package:")])
  object_commands <- c(paste0(":view ", vapply(utils::head(objects$name, 16L), .completion_name, character(1L))),
    if (nrow(objects) > 16L) ":objects")
  if (!nrow(objects)) object_commands <- ""
  memory_line <- if ("process_memory" %in% config$status) .process_memory_label(
    if (is.null(process_memory)) .process_memory() else process_memory) else character()
  workspace <- c(memory_line, object_lines, ":objects inspect  :rm select")
  attr(workspace, "commands") <- c(rep("", length(memory_line)), object_commands, ":objects")
  attr(workspace, "hints") <- c(":objects" = "inspect", ":rm" = "select")
  package_lines <- c(packages, ":packages list  :detach select")
  attr(package_lines, "commands") <- c(paste0(":docs ", packages), ":packages")
  attr(package_lines, "hints") <- c(":packages" = "list", ":detach" = "select")
  files <- tryCatch(.file_entries(getwd(), limit = 50L), error = function(e) NULL)
  file_lines <- if (is.null(files)) "Directory unavailable" else
    paste0(files$name, ifelse(files$directory, "/", ""))
  file_commands <- if (is.null(files)) "" else paste0(ifelse(files$directory, ":cd ", ":files "), files$path)
  file_lines <- c(file_lines, ":files browse")
  attr(file_lines, "commands") <- c(file_commands, ":files")
  attr(file_lines, "hints") <- c(":files" = "browse")
  out <- list(WORKSPACE = workspace, PACKAGES = package_lines, FILES = file_lines)
  stopifnot("sidebar groups must be named character vectors" = is.list(out) &&
              !is.null(names(out)) && all(vapply(out, is.character, logical(1L))))
  out
}

#' Read a Batch Confirmation in the Current Interface
#' @param config Console configuration.
#' @return A logical scalar.
#' @keywords internal
#' @noRd
.confirm_console <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  answer <- if (is.environment(config$screen) && isTRUE(config$screen$active)) {
    config$initial_text <- ""
    entry <- read_expression(prompt = "Confirm [y/N]> ", history = character(), config = config)
    if (identical(entry$action, "submit")) entry$text else ""
  } else readline("Continue? [y/N] ")
  out <- identical(tolower(trimws(answer)), "y")
  stopifnot("confirmation must be logical" = .is_flag(out))
  out
}
