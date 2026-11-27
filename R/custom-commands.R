.custom_command_state <- new.env(parent = emptyenv())

#' Locate Persistent Custom Console Commands
#'
#' Custom commands are stored separately from the data-only appearance
#' preferences. The default is `commands.rds` beside the preferences file.
#' `PRISMREPL_COMMANDS_FILE` overrides that location. A `NULL` settings path
#' returns `NULL`, selecting commands kept only in the current R process,
#' even when the environment override is set. Locating a file creates nothing.
#'
#' @param settings_file Preferences path used to select the directory, or
#'   `NULL` for session-only commands.
#' @return One command-registry path, or `NULL`.
#' @export
#' @examples
#' repl_commands_file()
#' repl_commands_file(NULL)
repl_commands_file <- function(settings_file = repl_settings_file()) {
  stopifnot("settings_file must be NULL or a nonempty path" =
              is.null(settings_file) || .is_text(settings_file))
  if (is.null(settings_file)) return(NULL)
  out <- Sys.getenv("PRISMREPL_COMMANDS_FILE", unset = "")
  if (!nzchar(out)) out <- file.path(dirname(path.expand(settings_file)), "commands.rds")
  out <- path.expand(out)
  stopifnot("commands path must be nonempty" = .is_text(out))
  out
}

#' Define Persistent Custom Colon Commands
#'
#' Register an ordinary R function as a colon command. Names are case
#' insensitive, may start with a colon, and otherwise contain ASCII letters,
#' digits, underscores or hyphens, beginning with a letter. An optional alias
#' follows the same rules; a short alias is usually easiest to type. Built-in
#' commands and aliases cannot be replaced. Replacing a custom definition
#' requires `replace = TRUE`.
#'
#' Every supplied argument is a character vector of length one. Functions with
#' exactly one formal argument, without `...`, receive the complete command
#' remainder, including internal spaces. Other functions receive whitespace
#' separated arguments, with single quotes, double quotes or backticks grouping
#' text containing spaces. No R expressions or shell substitutions are
#' evaluated in the arguments. Omitted arguments retain normal R defaults and
#' missing-argument behavior. A function containing `...` uses ordinary R
#' positional argument matching.
#'
#' Function definitions and their locally captured environments are saved
#' using R serialization. References to the global environment remain global
#' references: ordinary workspace objects are not copied into the registry.
#' Prefer `package::function()` for package dependencies and capture constants
#' explicitly with a function factory or `local()`. Live connections, external
#' pointers and other operating-system resources are not portable saved state.
#'
#' Functions are restored only when invoked; listing commands, completion and
#' the command palette inspect metadata without loading their function bodies.
#' Registration in the current process retains the original function object.
#' Changes to a captured environment during execution are not automatically
#' saved; register the function again to save an updated definition.
#'
#' The file is created or changed only by an explicit registration or removal.
#' A staged replacement preserves an existing registry if writing fails.
#' A short-lived directory lock prevents concurrent writers from replacing one
#' another's changes. If a process terminates while holding the lock, remove its
#' `.lock` directory only after confirming that no session is saving that file.
#' Registry files are limited to 16 MiB, including captured data. `file = NULL`
#' selects a separate registry that lasts only for the current R process.
#'
#' @param name Command name, optionally prefixed with a colon. Removal also
#'   accepts a custom alias.
#' @param fun An ordinary R function, including functions with no arguments or
#'   `...`. Wrap a primitive in `function(...)` before registering it.
#' @param description One-line description displayed and searched in help.
#' @param alias Optional single alternative command name.
#' @param file Registry path, normally [repl_commands_file()]. `NULL` selects
#'   session-only commands without reading or writing a file.
#' @param replace Allow replacing a custom command with the same canonical name.
#' @param external Temporarily restore the ordinary terminal while this command
#'   runs, for programs requiring terminal input. Defaults to `FALSE`.
#' @return `repl_add_command()` returns the command's one-row metadata data frame
#'   invisibly. `repl_custom_commands()` returns metadata for all custom
#'   commands, with `name`, `alias`, `usage`, `description`, `persistent` and
#'   `external` columns. `repl_remove_command()` returns the removed canonical
#'   name invisibly.
#' @export
#' @examples
#' path <- tempfile(fileext = ".rds")
#' repl_add_command("hello", function(name) paste("Hello,", name),
#'                  description = "Greet somebody", file = path)
#' repl_custom_commands(file = path)
#' repl_remove_command("hello", file = path)
#' unlink(path)
repl_add_command <- function(name, fun, description = "", alias = NULL,
                             file = repl_commands_file(), replace = FALSE,
                             external = FALSE) {
  name <- .custom_command_name(name)
  alias <- if (is.null(alias)) "" else .custom_command_name(alias)
  stopifnot("fun must be an ordinary R function; wrap primitives in function(...)" =
              is.function(fun) && !is.primitive(fun),
            "description must be one non-missing character string" =
              is.character(description) && length(description) == 1L && !is.na(description),
            "description must be one line without terminal control characters" =
              !grepl("[[:cntrl:]]", description),
            "description must be at most 2000 characters" = nchar(description) <= 2000L,
            "alias must differ from the command name" = !identical(name, alias),
            "replace and external must be logical" = .is_flag(replace) && .is_flag(external))
  description <- enc2utf8(description)
  if (!nzchar(description)) description <- "Custom R command."
  entries <- .custom_command_entries(file, strict = TRUE)
  stopifnot("custom command already exists; use replace = TRUE" =
              replace || !name %in% names(entries))
  signature <- .custom_command_state[[.custom_command_key(file)]]$signature
  parameters <- names(formals(fun))
  if (is.null(parameters)) parameters <- character()
  required <- unname(vapply(formals(fun), function(value)
    identical(value, quote(expr = )), logical(1L)))
  required[parameters == "..."] <- FALSE
  entry <- list(name = name, alias = alias, description = description,
    parameters = parameters, required = required, external = external,
    function_raw = serialize(fun, NULL, version = 3L))
  entries[[name]] <- entry
  entries <- .validate_custom_command_entries(entries)
  .write_custom_command_entries(entries, file, signature)
  key <- .custom_command_key(file)
  cache <- .custom_command_state[[key]]
  cache$functions[[name]] <- fun
  .custom_command_state[[key]] <- cache
  out <- .custom_command_metadata(entries[name], persistent = !is.null(file))
  stopifnot("registered command must have one metadata row" =
              is.data.frame(out) && nrow(out) == 1L && identical(out$name, name))
  invisible(out)
}

#' @rdname repl_add_command
#' @export
repl_custom_commands <- function(file = repl_commands_file()) {
  out <- .custom_command_metadata(.custom_command_entries(file, strict = TRUE),
                                  persistent = !is.null(file))
  stopifnot("custom commands must have metadata columns" = is.data.frame(out) &&
              identical(names(out), c("name", "alias", "usage", "description", "persistent", "external")))
  out
}

#' @rdname repl_add_command
#' @export
repl_remove_command <- function(name, file = repl_commands_file()) {
  name <- .custom_command_name(name)
  entries <- .custom_command_entries(file, strict = TRUE)
  keys <- names(entries)
  aliases <- vapply(entries, `[[`, character(1L), "alias")
  index <- match(name, keys)
  if (is.na(index)) index <- match(name, aliases)
  stopifnot("unknown custom command; built-in commands cannot be removed" = !is.na(index))
  out <- keys[[index]]
  signature <- .custom_command_state[[.custom_command_key(file)]]$signature
  .write_custom_command_entries(entries[-index], file, signature)
  stopifnot("removed command must have a canonical name" = .is_text(out))
  invisible(out)
}

#' Normalize a Custom Command Identifier
#' @param name One identifier, optionally prefixed by a colon.
#' @return A validated lowercase identifier.
#' @noRd
.custom_command_name <- function(name) {
  stopifnot("command name must be a nonempty character scalar" = .is_text(name))
  out <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", sub("^:", "", name))
  stopifnot("command names must start with an ASCII letter and contain only letters, digits, _ or -" =
              grepl("^[a-z][a-z0-9_-]*$", out),
            "command names must contain at most 64 characters" = nchar(out) <= 64L)
  out
}

#' Select a Stable Cache Key Without Creating Directories
#' @param file Registry path or NULL.
#' @return An absolute path or a reserved session-only key.
#' @noRd
.custom_command_key <- function(file) {
  stopifnot("file must be NULL or a nonempty path" = is.null(file) || .is_text(file))
  if (is.null(file)) return("<session>")
  path <- path.expand(file)
  directory <- normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
  out <- file.path(directory, basename(path))
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", out)) out <- file.path(getwd(), out)
  out <- normalizePath(out, winslash = "/", mustWork = FALSE)
  stopifnot("registry cache key must be nonempty" = .is_text(out))
  out
}

#' Read a Registry File Signature Without Reading Its Contents
#' @param file An absolute registry path.
#' @return File existence, type, size and modification metadata.
#' @noRd
.custom_command_signature <- function(file) {
  stopifnot("file must be a nonempty path" = .is_text(file))
  info <- file.info(file)
  out <- list(exists = file.exists(file), directory = isTRUE(info$isdir),
    size = unname(info$size), modified = as.numeric(info$mtime), changed = as.numeric(info$ctime))
  stopifnot("registry signature must record existence" = .is_flag(out$exists))
  out
}

#' Validate Stored Metadata Without Restoring Functions
#' @param entries Named registry records.
#' @return Validated records with their canonical names retained.
#' @noRd
.validate_custom_command_entries <- function(entries) {
  stopifnot("custom commands must be a named list" = is.list(entries) &&
              (!length(entries) || (!is.null(names(entries)) && !anyNA(names(entries)) &&
                !anyDuplicated(names(entries)))))
  fields <- c("name", "alias", "description", "parameters", "required", "external", "function_raw")
  invisible(lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    stopifnot("invalid custom command record" = is.list(entry) &&
                length(entry) == length(fields) && setequal(names(entry), fields),
              "invalid stored command name" = identical(entry$name, names(entries)[[index]]) &&
                identical(entry$name, .custom_command_name(entry$name)),
              "invalid stored command alias" = is.character(entry$alias) && length(entry$alias) == 1L &&
                !is.na(entry$alias) && (!nzchar(entry$alias) ||
                  identical(entry$alias, .custom_command_name(entry$alias))),
              "invalid stored command description" = .is_text(entry$description) &&
                nchar(entry$description) <= 2000L && !grepl("[[:cntrl:]]", entry$description),
              "invalid stored parameter names" = is.character(entry$parameters) &&
                !anyNA(entry$parameters) && all(nzchar(entry$parameters)) && !anyDuplicated(entry$parameters),
              "invalid stored required arguments" = is.logical(entry$required) &&
                !anyNA(entry$required) && length(entry$required) == length(entry$parameters) &&
                !any(entry$required[entry$parameters == "..."]),
              "invalid stored external mode" = .is_flag(entry$external),
              "invalid stored function data" = is.raw(entry$function_raw) &&
                length(entry$function_raw) > 0L && length(entry$function_raw) <= 16 * 1024^2)
    NULL
  }))
  tokens <- c(names(entries), vapply(entries, `[[`, character(1L), "alias"))
  tokens <- tokens[nzchar(tokens)]
  builtin <- .builtin_command_catalog()
  stopifnot("custom command names and aliases must not collide" = !anyDuplicated(tokens),
            "custom commands cannot replace built-in commands or aliases" =
              !any(tokens %in% c(builtin$id, builtin$alias)))
  entries
}

#' Retain Live Function Objects When Their Stored Definition Is Unchanged
#' @param entries Newly read validated records.
#' @param previous Previous cache record or NULL.
#' @return A named list of reusable function objects.
#' @noRd
.custom_command_functions <- function(entries, previous) {
  stopifnot("entries must be a list" = is.list(entries),
            "previous cache must be NULL or a list" = is.null(previous) || is.list(previous))
  keys <- intersect(names(entries), names(previous$functions))
  keep <- vapply(keys, function(key) identical(entries[[key]]$function_raw,
    previous$entries[[key]]$function_raw), logical(1L))
  out <- previous$functions[keys[keep]]
  if (is.null(out)) out <- list()
  stopifnot("cached values must be functions" = is.list(out) && all(vapply(out, is.function, logical(1L))))
  out
}

#' Read Cached Custom Metadata Without Executing User Code
#' @param file Registry path or NULL for process-local commands.
#' @param strict Read afresh and report invalid data as an error.
#' @return A named list of command records.
#' @noRd
.custom_command_entries <- function(file, strict = FALSE) {
  stopifnot("strict must be logical" = .is_flag(strict))
  key <- .custom_command_key(file)
  previous <- .custom_command_state[[key]]
  if (is.null(file)) {
    if (is.null(previous)) .custom_command_state[[key]] <- list(entries = list(),
      functions = list(), signature = NULL, error = NULL, warned = FALSE)
    return(.custom_command_state[[key]]$entries)
  }
  signature <- .custom_command_signature(key)
  if (strict || is.null(previous) || !identical(previous$signature, signature)) {
    problem <- NULL
    entries <- tryCatch({
      if (!signature$exists) list() else {
        stopifnot("command registry must be a regular file of at most 16 MiB" =
                    !signature$directory && is.finite(signature$size) &&
                    signature$size > 0 && signature$size <= 16 * 1024^2)
        data <- withCallingHandlers(readRDS(key), warning = function(warning)
          stop(conditionMessage(warning), call. = FALSE))
        stopifnot("command registry must contain one version-1 manifest" = is.list(data) &&
                    identical(names(data), c("version", "commands")) && identical(data$version, 1L))
        .validate_custom_command_entries(data$commands)
      }
    }, error = function(error) {
      problem <<- paste0("Custom commands could not be read from ", key, ": ", conditionMessage(error))
      list()
    })
    cache <- list(entries = entries, functions = .custom_command_functions(entries, previous),
      signature = signature, error = problem, warned = !is.null(previous) &&
        identical(previous$signature, signature) && identical(previous$error, problem) && isTRUE(previous$warned))
    .custom_command_state[[key]] <- cache
  }
  cache <- .custom_command_state[[key]]
  if (!is.null(cache$error)) {
    if (strict) stop(cache$error, call. = FALSE)
    if (!isTRUE(cache$warned)) {
      cache$warned <- TRUE
      .custom_command_state[[key]] <- cache
      warning(paste(cache$error, "Using built-in commands only."), call. = FALSE)
    }
  }
  out <- cache$entries
  stopifnot("custom command metadata must be a list" = is.list(out))
  out
}

#' Save Explicit Registry Changes with a Staged Replacement
#' @param entries Validated command records.
#' @param file Registry path or NULL.
#' @param signature Previously read file signature.
#' @return NULL invisibly.
#' @noRd
.write_custom_command_entries <- function(entries, file, signature) {
  entries <- .validate_custom_command_entries(entries)
  key <- .custom_command_key(file)
  previous <- .custom_command_state[[key]]
  if (!is.null(file)) {
    directory <- dirname(key)
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, mode = "0700"))
      stop("Could not create the custom-command directory.", call. = FALSE)
    lock <- paste0(key, ".lock")
    if (!dir.create(lock, showWarnings = FALSE, mode = "0700"))
      stop(paste0("Custom-command writer lock is unavailable at ", lock,
        "; retry when no other session is saving."), call. = FALSE)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    staged <- tempfile(".prism-commands-", tmpdir = directory)
    on.exit(unlink(staged), add = TRUE)
    stopifnot("could not create a staged command registry" = file.create(staged))
    Sys.chmod(staged, mode = "0600")
    saveRDS(list(version = 1L, commands = entries), staged, version = 3L, compress = FALSE)
    stopifnot("command registry exceeds 16 MiB; reduce captured data" =
                file.info(staged)$size <= 16 * 1024^2,
              "command registry changed while saving; retry the registration or removal" =
                identical(signature, .custom_command_signature(key)))
    if (!suppressWarnings(file.rename(staged, key)))
      stop("Could not save custom commands; the previous registry was retained.", call. = FALSE)
    signature <- .custom_command_signature(key)
    stopifnot("saved command registry must exist" = isTRUE(signature$exists))
  }
  .custom_command_state[[key]] <- list(entries = entries,
    functions = .custom_command_functions(entries, previous),
    signature = signature, error = NULL, warned = FALSE)
  invisible(NULL)
}

#' Find a Custom Definition and Restore It Only for Invocation
#' @param name Canonical name or alias.
#' @param file Registry path or NULL.
#' @param load Restore the registered function object.
#' @return A command record, with fun when requested, or NULL.
#' @noRd
.custom_command_lookup <- function(name, file, load = FALSE) {
  name <- .custom_command_name(name)
  stopifnot("load must be logical" = .is_flag(load))
  entries <- .custom_command_entries(file)
  index <- match(name, names(entries))
  if (is.na(index)) index <- match(name, vapply(entries, `[[`, character(1L), "alias"))
  if (is.na(index)) return(NULL)
  out <- entries[[index]]
  if (load) {
    key <- .custom_command_key(file)
    cache <- .custom_command_state[[key]]
    fun <- cache$functions[[out$name]]
    if (is.null(fun)) {
      fun <- tryCatch(withCallingHandlers(unserialize(out$function_raw),
        warning = function(warning) stop(conditionMessage(warning), call. = FALSE)), error = function(error)
        stop(paste0("Could not restore custom command :", out$name, ": ", conditionMessage(error)), call. = FALSE))
      stopifnot("saved command must restore an ordinary R function" = is.function(fun) && !is.primitive(fun))
      parameters <- names(formals(fun))
      if (is.null(parameters)) parameters <- character()
      required <- unname(vapply(formals(fun), function(value)
        identical(value, quote(expr = )), logical(1L)))
      required[parameters == "..."] <- FALSE
      stopifnot("saved function signature does not match its command metadata" =
                  identical(parameters, out$parameters) && identical(required, out$required))
      cache$functions[[out$name]] <- fun
      .custom_command_state[[key]] <- cache
    }
    out$fun <- fun
  }
  stopifnot("custom command lookup must return its canonical record" = is.list(out) && .is_text(out$name))
  out
}

#' Format Custom Command Signatures
#' @param entry One validated command record.
#' @return One command synopsis.
#' @noRd
.custom_command_usage <- function(entry) {
  stopifnot("entry must contain signature metadata" = is.list(entry) && .is_text(entry$name) &&
              is.character(entry$parameters) && length(entry$required) == length(entry$parameters))
  arguments <- ifelse(entry$parameters == "...", "[ARG ...]",
    ifelse(entry$required, toupper(entry$parameters), paste0("[", toupper(entry$parameters), "]")))
  out <- paste(c(paste0(":", entry$name), arguments), collapse = " ")
  stopifnot("command usage must start with a colon" = .is_text(out) && startsWith(out, ":"))
  out
}

#' Expose Registry Metadata Without Function Environments
#' @param entries Named validated records.
#' @param persistent Whether these records have a backing file.
#' @return A metadata data frame.
#' @noRd
.custom_command_metadata <- function(entries, persistent) {
  stopifnot("entries must be a list" = is.list(entries), "persistent must be logical" = .is_flag(persistent))
  out <- data.frame(name = vapply(entries, `[[`, character(1L), "name"),
    alias = vapply(entries, `[[`, character(1L), "alias"),
    usage = vapply(entries, .custom_command_usage, character(1L)),
    description = vapply(entries, `[[`, character(1L), "description"),
    persistent = rep(persistent, length(entries)),
    external = vapply(entries, `[[`, logical(1L), "external"), row.names = NULL, stringsAsFactors = FALSE)
  stopifnot("metadata rows must match registered commands" = nrow(out) == length(entries) && !anyNA(out))
  out
}

#' Add Custom Metadata to the Shared Command Palette
#' @param file Registry path or NULL.
#' @return Command catalog rows without restoring functions.
#' @noRd
.custom_command_catalog <- function(file) {
  entries <- .custom_command_entries(file)
  metadata <- .custom_command_metadata(entries, persistent = !is.null(file))
  out <- data.frame(id = metadata$name, command = metadata$usage, alias = metadata$alias,
    group = rep("Custom", nrow(metadata)), description = metadata$description,
    insert = vapply(entries, function(entry) paste0(":", entry$name,
      if (length(entry$parameters)) " " else ""), character(1L)), row.names = NULL, stringsAsFactors = FALSE)
  stopifnot("custom catalog must have one row per definition" = nrow(out) == length(entries) && !anyNA(out))
  out
}
