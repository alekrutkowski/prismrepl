#' Resolve an Editable Function Without Forcing Special Bindings
#' @param name Exact function binding name.
#' @param envir Workspace where the edited function will be stored.
#' @return A target descriptor containing the function and its destination.
#' @noRd
.function_edit_target <- function(name, envir) {
  stopifnot("name must be one function name" = .is_text(name),
            "envir must be a workspace environment" = is.environment(envir) && !isNamespace(envir) &&
              !startsWith(environmentName(envir), "package:"))
  location <- envir
  repeat {
    if (exists(name, location, inherits = FALSE)) break
    if (identical(location, emptyenv())) stop("No function named '", name, "' was found.", call. = FALSE)
    location <- parent.env(location)
  }
  kind <- unname(binding_info(location, name))
  if (!identical(kind, "value")) stop("The function must be an ordinary binding. '", name,
    "' is a ", kind, " binding and was not evaluated. ",
    if (identical(kind, "promise")) paste0("Evaluate ", name, " explicitly at the R prompt, then retry :edit ", name, ".") else
      "Copy the intended function to an ordinary workspace binding first.", call. = FALSE)
  fun <- get(name, location, inherits = FALSE)
  stopifnot("only R functions with editable bodies are supported" = typeof(fun) == "closure",
            "the destination function binding must be unlocked" =
              !exists(name, envir, inherits = FALSE) || !bindingIsLocked(name, envir),
            "the workspace must permit a new function binding" =
              exists(name, envir, inherits = FALSE) || !environmentIsLocked(envir))
  out <- list(name = name, fun = fun, local = identical(location, envir),
    destination_before = if (identical(location, envir)) fun else NULL)
  stopifnot("edit target must retain a closure" = typeof(out$fun) == "closure")
  out
}

#' Render a Function as Editable Source
#' @param fun R closure.
#' @return One UTF-8 source string.
#' @noRd
.function_edit_source <- function(fun) {
  stopifnot("fun must be an R closure" = typeof(fun) == "closure")
  out <- paste(deparse(fun, width.cutoff = 100L, control = c("keepNA", "keepInteger", "niceNames")), collapse = "\n")
  stopifnot("function source must be nonempty" = .is_text(out))
  enc2utf8(out)
}

#' Validate Edited Source Without Executing Its Body or Defaults
#' @param text Edited source.
#' @param original Original closure whose lexical environment is retained.
#' @return A record with ok, fun and error fields.
#' @noRd
.function_edit_validate <- function(text, original) {
  stopifnot("text must be scalar source" = is.character(text) && length(text) == 1L && !is.na(text),
            "original must be an R closure" = typeof(original) == "closure")
  out <- tryCatch({
    expressions <- parse(text = text, keep.source = FALSE)
    stopifnot("enter exactly one function definition, without an assignment or other expressions" =
                length(expressions) == 1L && is.call(expressions[[1L]]) &&
                  identical(expressions[[1L]][[1L]], as.name("function")))
    # A binding named `function` can mask the primitive constructor. Construct in
    # baseenv(), then restore lexical scope; no user expression is evaluated.
    fun <- eval(expressions[[1L]], envir = baseenv())
    environment(fun) <- environment(original)
    attributes <- attributes(original)
    attributes[c("srcref", "wholeSrcref", "srcfile")] <- NULL
    attributes(fun) <- attributes
    list(ok = TRUE, fun = fun, error = "")
  }, error = function(e) list(ok = FALSE, fun = NULL, error = .safe_text(conditionMessage(e))))
  stopifnot("validation must return a closure or a reason" = .is_flag(out$ok) &&
              if (out$ok) typeof(out$fun) == "closure" else .is_text(out$error))
  out
}

#' Save One Validated Edited Function to Its Workspace
#' @param target Function target descriptor.
#' @param text Source to save.
#' @param envir Destination workspace.
#' @return A save result; failures leave the binding unchanged.
#' @noRd
.function_edit_save <- function(target, text, envir) {
  stopifnot("target must contain a function name and closure" = is.list(target) && .is_text(target$name) &&
              typeof(target$fun) == "closure",
            "destination must be a workspace" = is.environment(envir) && !isNamespace(envir) &&
              !startsWith(environmentName(envir), "package:"))
  value <- .function_edit_validate(text, target$fun)
  if (!value$ok) return(value)
  out <- tryCatch({
    present <- exists(target$name, envir, inherits = FALSE)
    stopifnot("the function binding changed while the editor was open; close and reopen it" =
                if (isTRUE(target$local)) present &&
                  identical(unname(binding_info(envir, target$name)), "value") &&
                  identical(get(target$name, envir, inherits = FALSE), target$destination_before) else !present,
              "the destination function binding is locked" = !present || !bindingIsLocked(target$name, envir),
              "the workspace is locked" = present || !environmentIsLocked(envir))
    assign(target$name, value$fun, envir = envir)
    value
  }, error = function(e) list(ok = FALSE, fun = NULL, error = .safe_text(conditionMessage(e))))
  stopifnot("save must report success or failure" = .is_flag(out$ok))
  out
}

#' Create a Small Function Editor State
#' @param source Initial source.
#' @return Editor and undo state.
#' @noRd
.function_editor_state <- function(source) {
  stopifnot("source must be scalar text" = .is_text(source))
  editor <- .editor_state(source)
  editor$cursor <- 0L
  out <- list(editor = editor, saved_source = source, undo = list(), redo = list(),
    query = "", searching = FALSE, status = "", save = FALSE, quit = FALSE)
  stopifnot("function editor must start at the beginning" = out$editor$cursor == 0L)
  out
}

#' Find the Next Literal Match in Editable Source
#' @param state Function editor state.
#' @param restart Start searching from the beginning.
#' @return Updated cursor and match status.
#' @noRd
.function_editor_find <- function(state, restart = FALSE) {
  stopifnot("state must have an editor and query" = is.list(state$editor) && is.character(state$query),
            "restart must be logical" = .is_flag(restart))
  text <- paste0(state$editor$chars, collapse = "")
  matches <- if (nzchar(state$query)) stringi::stri_locate_all_fixed(text, state$query,
    opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))[[1L]] else matrix(NA_integer_, ncol = 2L)
  if (is.na(matches[[1L]])) { state$status <- "No matching text."; return(state) }
  grapheme_ends <- c(0L, cumsum(nchar(state$editor$chars, type = "chars")))
  indices <- findInterval(matches[, 1L] - 1L, grapheme_ends) - 1L
  after <- if (restart) seq_along(indices) else which(indices > state$editor$cursor)
  chosen <- if (length(after)) after[[1L]] else 1L
  state$editor$cursor <- max(0L, indices[[chosen]])
  state$status <- paste0("Match ", chosen, "/", length(indices), ": ", state$query, "  (Ctrl+N next)")
  stopifnot("search cursor must remain valid" = state$editor$cursor >= 0L &&
              state$editor$cursor <= length(state$editor$chars))
  state
}

#' Move an Ad-Hoc Editor Cursor Without Falling into Console History
#' @param editor Ordinary editor state.
#' @param frame Wrapped source frame.
#' @param amount Number of visual rows to move.
#' @return Editor state with clamped cursor.
#' @noRd
.function_editor_move <- function(editor, frame, amount) {
  stopifnot("frame must include wrapped geometry" = is.list(frame$geometry),
            "amount must be one finite number" = is.numeric(amount) && length(amount) == 1L && is.finite(amount))
  index <- max(1L, min(length(frame$geometry$starts), frame$visual_row + amount))
  row <- .editor_geometry_row(frame$geometry, index)
  editor$cursor <- row$from + which.min(abs(row$columns - frame$cursor_column)) - 1L
  editor$completion <- NULL
  editor$overlay <- character()
  stopifnot("moved editor cursor must be valid" = editor$cursor >= 0L && editor$cursor <= length(editor$chars))
  editor
}

#' Apply an Ad-Hoc Function Editor Event
#' @param state Function editor state.
#' @param event Native event after mouse-selection dispatch.
#' @param config Console configuration.
#' @param envir Completion environment.
#' @param frame Optional current full editor frame.
#' @return Updated editor state; save requests are explicit flags.
#' @noRd
.function_editor_apply <- function(state, event, config, envir, frame = NULL) {
  stopifnot("state and event must be lists" = is.list(state) && is.list(event),
            "envir must be an environment" = is.environment(envir))
  state$save <- FALSE
  ctrl <- "ctrl" %in% event$modifiers
  action <- .key_action(event, config)
  key <- if (is.null(event$key)) "" else event$key
  if (identical(event$type, "eof")) { state$quit <- TRUE; return(state) }
  if (state$searching) {
    if (action %in% c("escape", "cancel")) state$searching <- FALSE
    else if (action == "submit") {
      state$searching <- FALSE
      state <- .function_editor_find(state, restart = TRUE)
    } else if (action == "backspace") state$query <- paste(utils::head(.editor_graphemes(state$query), -1L), collapse = "")
    else if (ctrl && identical(key, "u")) state$query <- ""
    else if (event$type %in% c("text", "paste") && !ctrl) state$query <- substr(paste0(state$query,
      gsub("[\r\n\t]", " ", .safe_text(event$text))), 1L, 200L)
    return(state)
  }
  if (identical(action, "function_help")) {
    state$editor <- .editor_request_help(state$editor)
    if (length(state$editor$overlay)) state$status <- paste(state$editor$overlay, collapse = " ")
    return(state)
  }
  if (action %in% c("escape", "cancel") || (ctrl && key == "x")) { state$quit <- TRUE; return(state) }
  if (ctrl && key == "s") { state$save <- TRUE; return(state) }
  if (ctrl && key == "w") { state$searching <- TRUE; return(state) }
  if (ctrl && key == "n" && nzchar(state$query)) return(.function_editor_find(state))
  if (ctrl && key %in% c("z", "y")) {
    source <- if (key == "z") "undo" else "redo"
    other <- if (key == "z") "redo" else "undo"
    if (length(state[[source]])) {
      state[[other]] <- utils::tail(c(state[[other]], list(state$editor)), 100L)
      state$editor <- utils::tail(state[[source]], 1L)[[1L]]
      state[[source]] <- utils::head(state[[source]], -1L)
      state$status <- if (key == "z") "Undone." else "Redone."
    } else state$status <- paste0("Nothing to ", source, ".")
    return(state)
  }
  before <- state$editor
  if (identical(event$type, "mouse") && !is.null(frame)) {
    if (event$key %in% c("wheelup", "wheeldown")) state$editor <- .function_editor_move(state$editor,
      frame$editor, if (event$key == "wheelup") -3L else 3L)
    else state$editor <- .editor_mouse(state$editor, event, frame$editor, frame$editor_layout)
  } else if (action %in% c("up", "down", "scroll_up", "scroll_down", "history_previous", "history_next") &&
             !is.null(frame)) {
    direction <- if (action %in% c("up", "scroll_up", "history_previous")) -1L else 1L
    step <- if (action %in% c("scroll_up", "scroll_down")) max(1L, frame$editor_height - 1L) else 1L
    state$editor <- .function_editor_move(state$editor, frame$editor, direction * step)
  } else if (action == "help") state$status <- paste0("Ctrl+S save; Ctrl+X close; Ctrl+Z undo; Ctrl+Y redo; Ctrl+W find; Ctrl+N next; Tab completes; ",
    .editor_key_label(config, "function_help", "F1"), " function help.")
  else {
    local <- config
    if (action %in% c("submit", "newline")) {
      chord <- .chord_from_parts(event$key, event$modifiers)
      local$key_lookup[[chord]] <- "newline"
    }
    if (action == "history_search") state$status <- "Ctrl+W searches this function; Ctrl+N finds the next occurrence."
    else {
      state$editor <- .editor_step(state$editor, event, history = character(), envir = envir, config = local)
      state$editor$action <- "edit"
      state$editor$search <- NULL
    }
  }
  if (!identical(before$chars, state$editor$chars)) {
    state$undo <- utils::tail(c(state$undo, list(before)), 100L)
    state$redo <- list()
    state$status <- ""
  }
  stopifnot("function editor must retain editable state" = identical(state$editor$action, "edit") &&
              state$editor$cursor >= 0L && state$editor$cursor <= length(state$editor$chars))
  state
}

#' Compose a Full-Window Function Editor
#' @param state Function editor state.
#' @param name Destination binding name.
#' @param config Console configuration.
#' @param size Terminal dimensions.
#' @return Full frame plus source hit-test metadata.
#' @noRd
.function_editor_frame <- function(state, name, config, size) {
  stopifnot("state must contain an editor" = is.list(state$editor), "name must be text" = .is_text(name),
            "size must contain terminal dimensions" = all(c("width", "height") %in% names(size)))
  name <- gsub("[\r\n]", " ", .safe_text(name))
  width <- max(1L, as.integer(size[["width"]]) - 1L)
  height <- as.integer(size[["height"]])
  header <- as.integer(height >= 3L)
  footer <- min(2L, max(0L, height - header - 1L))
  available <- max(1L, height - header - footer)
  content_width <- max(1L, width - 2L)
  geometry <- .editor_wrap_geometry(state$editor, content_width, "", "", config)
  editor <- .editor_wrapped_frame(state$editor, geometry, content_width, available, config)
  rows <- rep(.screen_paint(strrep(" ", width), "editor", config), height)
  text <- paste0(state$editor$chars, collapse = "")
  dirty <- !identical(text, state$saved_source)
  if (header) rows[[1L]] <- .screen_paint(.screen_fit(paste0(" EDIT FUNCTION  ", name,
    if (dirty) "  [modified]" else "  [saved]"), width), "panel_header", config)
  indices <- header + seq_along(editor$lines)
  rows[indices] <- vapply(editor$lines, function(line)
    .screen_paint(.screen_fit(paste0(" ", line), width), "editor", config), character(1L))
  if (footer >= 2L) {
    current <- .editor_line(state$editor)
    status <- if (state$searching) paste0("Find: ", state$query) else if (nzchar(state$status)) state$status else
      paste0("Line ", current$row, "/", state$editor$line_count, "  Column ", state$editor$cursor - current$start + 1L,
        "  Save replaces only ", name, " in this workspace; close discards unsaved edits.")
    rows[[height - 1L]] <- .screen_paint(.screen_fit(paste0(" ", status), width), "status", config)
  }
  if (footer >= 1L) rows[[height]] <- .screen_paint(.screen_fit(paste0(" ", .footer_hints(
    c("Ctrl+S" = "save", "Ctrl+X" = "close", "Ctrl+Z" = "undo", "Ctrl+W" = "find", "Ctrl+N" = "next",
      stats::setNames("docs", .editor_key_label(config, "function_help", "F1"))),
    config, max(1L, width - 1L))), width), "status", config)
  out <- list(lines = rows, cursor_row = header + editor$cursor_row,
    cursor_column = min(width - 1L, 1L + editor$cursor_column),
    layout = list(width = size[["width"]], height = height), editor = editor,
    editor_layout = list(editor_start = header, content_width = content_width), editor_height = available)
  stopifnot("function editor frame must preserve dimensions" = length(out$lines) == height &&
              out$cursor_row >= 0L && out$cursor_row < height)
  out
}

#' Run the Ad-Hoc Function Editor in a Scoped Terminal Session
#' @param target Function target descriptor.
#' @param envir Destination workspace.
#' @param config Console configuration.
#' @return A record containing saved and dirty flags.
#' @noRd
.run_function_editor <- function(target, envir, config) {
  stopifnot("target must identify a closure" = is.list(target) && typeof(target$fun) == "closure",
            "envir must be an environment" = is.environment(envir),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  source <- .function_edit_source(target$fun)
  stopifnot("function exceeds editor limits; increase max_paste or max_lines" =
              nchar(source, type = "bytes") <= config$max_paste &&
                stringi::stri_count_fixed(source, "\n") + 1L <= config$max_lines)
  state <- .function_editor_state(source)
  config$highlight_cache <- new.env(parent = emptyenv())
  session <- .viewer_start(config)
  on.exit(.viewer_finish(session, config), add = TRUE)
  if (is.null(session$handle)) stop("The function editor needs a supported terminal. Use mode = 'terminal' in Rterm or a terminal R session.", call. = FALSE)
  size <- term_size()
  previous <- NULL
  mouse <- .mouse_state()
  frame <- NULL
  redraw <- TRUE
  saved <- FALSE
  repeat {
    if (redraw) {
      frame <- .function_editor_frame(state, target$name, config, size)
      painted <- .mouse_paint(frame, mouse, config)
      painted <- .cursor_frame(painted, config, role = "editor")
      update <- .screen_build_update(painted, previous, config, show_cursor = TRUE)
      if (nzchar(update)) term_write(update, synchronize = isTRUE(config$synchronize),
                                    cursor = painted$cursor_color)
      previous <- painted
      redraw <- FALSE
    }
    event <- term_read(session$handle, 50L)
    if (identical(event$type, "timeout")) next
    dispatched <- .mouse_dispatch(event, frame, mouse, config)
    mouse <- dispatched$selection
    redraw <- isTRUE(dispatched$redraw)
    if (nzchar(dispatched$status)) state$status <- dispatched$status
    event <- dispatched$event
    if (is.null(event)) next
    if (identical(event$type, "resize")) { size <- term_size(); previous <- NULL; redraw <- TRUE; next }
    state <- .function_editor_apply(state, event, config, envir, frame)
    if (!is.null(state$editor$help_request)) {
      state$editor <- .editor_open_help(state$editor, config, session$handle)
      if (length(state$editor$overlay)) state$status <- paste(state$editor$overlay, collapse = " ")
      size <- term_size()
      previous <- NULL
      mouse <- .mouse_state()
    }
    if (state$quit) break
    if (state$save) {
      text <- paste0(state$editor$chars, collapse = "")
      result <- .function_edit_save(target, text, envir)
      if (result$ok) {
        saved <- TRUE
        target$fun <- target$destination_before <- result$fun
        target$local <- TRUE
        state$saved_source <- text
        state$status <- paste0("Saved ", target$name, " in the workspace. Ctrl+X closes the editor.")
      } else state$status <- paste0("Not saved: ", gsub("[\r\n]", " ", result$error))
    }
    redraw <- TRUE
  }
  out <- list(saved = saved, dirty = !identical(paste0(state$editor$chars, collapse = ""), state$saved_source), name = target$name)
  stopifnot("editor result must report save and unsaved changes" = .is_flag(out$saved) && .is_flag(out$dirty))
  out
}

#' Edit a Workspace Function with Explicit Save Approval
#' @param name Exact binding name.
#' @param envir Evaluation workspace.
#' @param config Console configuration.
#' @return A save summary invisibly.
#' @noRd
.edit_function_command <- function(name, envir, config) {
  stopifnot("name must be one function name" = .is_text(name), "envir must be an environment" = is.environment(envir),
            "config must be a console configuration" = inherits(config, "prismrepl_config"))
  target <- .function_edit_target(name, envir)
  out <- .run_function_editor(target, envir, config)
  .emit(paste0(if (out$saved) paste0("Saved ", name, " in the workspace.") else "Function left unchanged.",
               if (out$dirty) " Unsaved edits discarded." else ""), "message", config)
  stopifnot("function edit must return a summary" = is.list(out) && .is_flag(out$saved))
  invisible(out)
}
