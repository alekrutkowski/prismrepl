#' Choose the Pipe Inserted by the Keyboard Shortcut
#' @param config Console configuration.
#' @return The chosen pipe operator, or NULL after cancellation.
#' @noRd
.pipe_choice <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  out <- .command_choice(data.frame(id = c("|>", "%>%"),
    operator = c("|>", "%>%"),
    description = c("Native R pipe; built into R.",
      "magrittr pipe; make the %>% operator available before using it."),
    stringsAsFactors = FALSE), "Pipe operator", config, current = config$pipe)
  stopifnot("pipe choice must be valid or cancelled" = is.null(out) ||
              (.is_text(out) && out %in% c("|>", "%>%")))
  out
}

#' Choose the Console Layout
#' @param config Console configuration.
#' @return Fullscreen, inline, or NULL after cancellation.
#' @noRd
.layout_choice <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  out <- .command_choice(data.frame(id = c("fullscreen", "inline"),
    layout = c("fullscreen", "inline"), description = c(
      "Responsive transcript, editor, sidebar and footer in the alternate screen.",
      "Edit at the normal terminal prompt and retain terminal scrollback."),
    stringsAsFactors = FALSE), "Console layout", config, current = config$ui)
  stopifnot("layout choice must be valid or cancelled" = is.null(out) ||
              (.is_text(out) && out %in% c("fullscreen", "inline")))
  out
}

#' Choose Status Fields as an Explicit Batch
#' @param config Console configuration.
#' @return Enabled field names, an empty vector for none, or NULL on cancellation.
#' @noRd
.status_choice <- function(config) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"))
  fields <- c("directory", "git", "objects", "packages", "memory")
  labels <- paste(format(fields, width = 10L), c("Current directory path",
    "Repository branch and working-tree status", "Number of workspace objects",
    "Number of attached packages", "Estimated size of workspace objects"))
  chosen <- .tui_panel(labels, "Status fields", config, selected = fields %in% config$status)
  if (is.null(chosen)) return(NULL)
  stopifnot("status selection must contain valid field indices" = is.numeric(chosen) &&
              !anyNA(chosen) && all(chosen %in% seq_along(fields)) && !anyDuplicated(chosen))
  out <- fields[chosen]
  stopifnot("chosen status fields must be valid" = is.character(out) && all(out %in% fields))
  out
}

#' Choose a Keyboard Action Without Changing Its Binding
#' @param config Console configuration.
#' @param title Menu heading.
#' @param action Optional action to focus and show alone.
#' @param reset Include the explicit reset command as a selectable item.
#' @return A keyboard action or reset, or NULL on cancellation.
#' @noRd
.binding_choice <- function(config, title = "Remappable keyboard actions", action = NULL, reset = TRUE) {
  stopifnot("config must be a console configuration" = inherits(config, "prismrepl_config"),
            "title must be nonempty text" = .is_text(title),
            "action must be NULL or a known action" = is.null(action) ||
              (.is_text(action) && action %in% names(config$keybindings)),
            "reset must be logical" = .is_flag(reset))
  table <- .keybinding_table(config)
  if (!is.null(action)) table <- table[table$id == action, , drop = FALSE]
  data <- data.frame(id = table$id, name = table$id, key = table$key,
    action = table$action, stringsAsFactors = FALSE)
  if (reset && is.null(action)) data <- rbind(data, data.frame(id = "reset", name = "reset",
    key = "", action = "Stage :bind reset to restore every default shortcut.",
    stringsAsFactors = FALSE))
  out <- .command_choice(data, title, config, current = action)
  stopifnot("binding choice must be an action, reset, or cancellation" = is.null(out) ||
              (.is_text(out) && out %in% data$id))
  out
}

#' Stage a Binding Command for the User to Complete
#' @param state Console session environment.
#' @param action Optional action to show alone.
#' @param title Menu heading.
#' @return NULL invisibly; acceptance updates the input buffer without remapping.
#' @noRd
.binding_menu <- function(state, action = NULL, title = "Remappable keyboard actions") {
  stopifnot("state must be an environment" = is.environment(state))
  chosen <- .binding_choice(state$config, title, action)
  if (!is.null(chosen)) {
    state$initial <- paste0(":bind ", chosen, if (chosen != "reset") " ")
    stopifnot("binding menu must stage a complete command prefix" =
                .is_text(state$initial) && startsWith(state$initial, ":bind "))
  }
  invisible(NULL)
}

#' Open Editable Preferences from Their Overview
#' @param state Console session environment.
#' @param envir Workspace environment.
#' @return NULL invisibly. Choices open a setting menu or stage a command.
#' @noRd
.preferences_menu <- function(state, envir) {
  stopifnot("state and workspace must be environments" = is.environment(state) && is.environment(envir),
            "state must contain a console configuration" = inherits(state$config, "prismrepl_config"))
  cfg <- state$config
  flag <- function(x) if (isTRUE(x)) "on" else "off"
  data <- data.frame(
    id = c("theme", "pipe", "mouse", "rainbow", "autopair", "ai", "custom", "bind",
      "ui", "sidebar", "status", "persist", "path", "reset"),
    setting = c("Theme", "Pipe", "Mouse", "Rainbow delimiters", "Automatic pairing",
      "AI provider", "Custom AI URL", "Keyboard shortcuts", "Console layout", "Session pane",
      "Status fields", "Save changes", "Preferences file", "Reset preferences"),
    value = c(cfg$theme$name, cfg$pipe, flag(cfg$mouse), flag(cfg$rainbow), flag(cfg$autopair),
      cfg$ai_provider, if (is.null(cfg$ai_url)) "unset" else cfg$ai_url,
      paste(length(cfg$keybindings), "actions"), cfg$ui, flag(cfg$sidebar),
      if (length(cfg$status)) paste(cfg$status, collapse = " ") else "none", flag(cfg$persist),
      if (is.null(cfg$settings_file)) "disabled" else cfg$settings_file, "defaults"),
    description = c("Choose and save a theme.", "Choose the pipe shortcut's operator.",
      "Choose whether mouse interaction is enabled.", "Choose matching-delimiter colors.",
      "Choose automatic delimiter insertion.", "Choose a provider for error-explanation links.",
      "Place :ai custom in the input to edit its HTTPS template.",
      "Choose an action, then enter its new shortcut.", "Choose fullscreen or inline for this session.",
      "Show or hide the sidebar for this session.", "Select fields shown in this session's status line.",
      "View how preference saving is configured.", "View the preferences file location.",
      "Place :settings reset in the input for explicit submission."),
    stringsAsFactors = FALSE)
  choice <- .command_choice(data, "Preferences", cfg)
  if (is.null(choice)) return(invisible(NULL))
  if (choice == "custom") state$initial <- paste0(":ai custom ", if (!is.null(cfg$ai_url)) cfg$ai_url)
  else if (choice == "reset") state$initial <- ":settings reset"
  else if (choice == "persist") .tui_panel(c(paste("Save changes:", flag(cfg$persist)),
    "Set persist = TRUE or FALSE in repl_config() when starting the console.",
    "A NULL settings_file disables preference-file access."), "Preference saving", cfg)
  else if (choice == "path") .tui_panel(c("Preferences file:",
    if (is.null(cfg$settings_file)) "disabled" else cfg$settings_file), "Preferences file", cfg)
  else .repl_command(paste0(":", choice), state, envir)
  stopifnot("preferences menu must preserve the console configuration" =
              inherits(state$config, "prismrepl_config"))
  invisible(NULL)
}
