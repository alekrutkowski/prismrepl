#' Cache the Initial Desktop Appearance Without Querying It at Load Time
#' @keywords internal
#' @noRd
.appearance_cache <- new.env(parent = emptyenv())

#' Pick a Built-in Theme from the Initial Desktop Appearance
#'
#' Detection is lazy and cached for the R process. This helper only reads
#' desktop preferences; saving the selected theme belongs to REPL startup.
#' Unknown appearances use the dark midnight theme. No terminal control
#' queries are sent, so detection cannot consume pending keyboard input.
#'
#' @param refresh Logical; repeat desktop detection instead of using the cache.
#' @param detector Appearance reader, injectable for tests.
#' @param cache An environment holding the cached result.
#' @return One built-in theme name, paper or midnight.
#' @keywords internal
#' @noRd
.initial_theme <- function(refresh = FALSE, detector = .detect_appearance,
                           cache = .appearance_cache) {
  stopifnot("refresh must be logical" = .is_flag(refresh),
            "detector must be a function" = is.function(detector),
            "cache must be an environment" = is.environment(cache))
  if (refresh || !exists("theme", envir = cache, inherits = FALSE)) {
    mode <- tryCatch(detector(), error = function(e) "unknown")
    cache$theme <- if (identical(mode, "light")) "paper" else "midnight"
  }
  out <- cache$theme
  stopifnot("initial theme must be paper or midnight" = .is_text(out) &&
              out %in% c("paper", "midnight"))
  out
}

#' Read the Desktop Light or Dark Preference
#'
#' Windows reads AppsUseLightTheme from the current user's Personalize key.
#' GNOME reads the interface schema in one bounded gsettings invocation and
#' uses gtk-theme as a fallback for desktops predating color-scheme. macOS
#' reads the global AppleInterfaceStyle preference. An explicit GTK theme or
#' a conventional COLORFGBG terminal background can supply fallback evidence.
#' No desktop process is started for an unrecognized Linux desktop.
#'
#' @param platform Operating-system name, normally `Sys.info()[["sysname"]]`.
#' @param variables Named environment-variable values.
#' @param registry Function reading the Windows personalization values.
#' @param command Function accepting a program and its fixed arguments, and
#'   returning a list with output and status fields.
#' @return One of light, dark, or unknown.
#' @keywords internal
#' @noRd
.detect_appearance <- function(platform = Sys.info()[["sysname"]],
    variables = Sys.getenv(c("XDG_CURRENT_DESKTOP", "DESKTOP_SESSION",
                             "GDMSESSION", "GTK_THEME", "COLORFGBG")),
    registry = .appearance_registry, command = .appearance_command) {
  stopifnot("platform must be one nonempty name" = .is_text(platform),
            "variables must be named character values" = is.character(variables) &&
              !anyNA(variables) && (length(variables) == 0L ||
                (!is.null(names(variables)) && !anyDuplicated(names(variables)))),
            "registry must be a function" = is.function(registry),
            "command must be a function" = is.function(command))
  value <- function(name) if (name %in% names(variables)) variables[[name]] else ""
  read_command <- function(program, args) {
    result <- tryCatch(suppressWarnings(command(program, args)), error = function(e) NULL)
    if (!is.list(result) || !is.character(result$output) || anyNA(result$output) ||
        !.is_count(result$status)) return(list(output = character(), status = 127L))
    list(output = head(result$output, 512L), status = as.integer(result$status))
  }
  out <- "unknown"
  if (identical(platform, "Windows")) {
    values <- tryCatch(suppressWarnings(registry()), error = function(e) list())
    light <- if (is.list(values)) values[["AppsUseLightTheme"]] else NULL
    if (.is_count(light) && light %in% c(0L, 1L))
      out <- if (light == 1L) "light" else "dark"
  } else if (identical(platform, "Darwin")) {
    result <- read_command("defaults", c("read", "-g", "AppleInterfaceStyle"))
    appearance <- tolower(trimws(paste(result$output, collapse = " ")))
    if (result$status == 0L && appearance %in% c("dark", "light")) out <- appearance
    # defaults exits 1 for an absent key, the normal light-appearance state.
    # Missing executables (127) and timed-out processes (124) remain unknown.
    if (result$status == 1L && !length(result$output)) out <- "light"
  } else if (identical(platform, "Linux")) {
    desktop <- tolower(paste(value("XDG_CURRENT_DESKTOP"), value("DESKTOP_SESSION"),
                            value("GDMSESSION"), collapse = " "))
    if (grepl("(^|[: _-])(gnome|ubuntu|unity)([: _-]|$)", desktop, perl = TRUE)) {
      result <- read_command("gsettings", c("list-recursively", "org.gnome.desktop.interface"))
      if (result$status == 0L) out <- .appearance_gnome(result$output)
    }
    if (out == "unknown" && nzchar(value("GTK_THEME")))
      out <- .appearance_theme_name(value("GTK_THEME"))
  }
  if (out == "unknown") {
    background <- tail(strsplit(value("COLORFGBG"), ";", fixed = TRUE)[[1L]], 1L)
    if (length(background) && background %in% c("0", "8")) out <- "dark"
    if (length(background) && background %in% c("7", "15")) out <- "light"
  }
  stopifnot("appearance must be light, dark, or unknown" = .is_text(out) &&
              out %in% c("light", "dark", "unknown"))
  out
}

#' Read One Windows Application-Appearance Registry Key
#' @return A list of registry values, or an empty list when unavailable.
#' @keywords internal
#' @noRd
.appearance_registry <- function() {
  out <- if (.Platform$OS.type == "windows") tryCatch(suppressWarnings({
    # readRegistry is exported by utils only on Windows.
    reader <- getExportedValue("utils", "readRegistry")
    reader("Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
           hive = "HCU", maxdepth = 1L)
  }), error = function(e) list()) else list()
  if (!is.list(out)) out <- list()
  stopifnot("registry result must be a list" = is.list(out))
  out
}

#' Run One Fixed Desktop-Preference Query with a Timeout
#' @param program Executable name.
#' @param args Fixed, separate command arguments.
#' @param locate Executable locator, injectable for tests.
#' @param run Bounded process runner, injectable for tests.
#' @return A list containing captured output and an integer exit status.
#' @keywords internal
#' @noRd
.appearance_command <- function(program, args, locate = Sys.which, run = system2) {
  stopifnot("program must be one nonempty name" = .is_text(program),
            "args must be character values" = is.character(args) && !anyNA(args),
            "locate must be a function" = is.function(locate),
            "run must be a function" = is.function(run))
  executable <- unname(locate(program))
  output <- if (nzchar(executable)) tryCatch(suppressWarnings(
    run(executable, args = vapply(args, shQuote, character(1)),
        stdout = TRUE, stderr = FALSE, timeout = 1L)),
    error = function(e) structure(character(), status = 127L))
    else structure(character(), status = 127L)
  status <- attr(output, "status", exact = TRUE)
  out <- list(output = head(as.character(output), 512L),
              status = if (is.null(status)) 0L else as.integer(status))
  stopifnot("command output must be character" = is.character(out$output) && !anyNA(out$output),
            "command status must be a nonnegative integer" = .is_count(out$status))
  out
}

#' Resolve GNOME Interface Schema Values
#' @param lines Output of gsettings list-recursively for the interface schema.
#' @return One of light, dark, or unknown.
#' @keywords internal
#' @noRd
.appearance_gnome <- function(lines) {
  stopifnot("GNOME settings must be character lines" = is.character(lines) && !anyNA(lines))
  setting <- function(key) {
    prefix <- paste0("^org\\.gnome\\.desktop\\.interface[[:space:]]+", key, "[[:space:]]+")
    found <- lines[grepl(prefix, lines)]
    if (!length(found)) return("")
    value <- trimws(sub(prefix, "", found[[1L]]))
    if (nchar(value) >= 2L && startsWith(value, "'") && endsWith(value, "'"))
      value <- substr(value, 2L, nchar(value) - 1L)
    tolower(value)
  }
  scheme <- setting("color-scheme")
  theme <- setting("gtk-theme")
  out <- if (scheme == "prefer-dark") "dark" else if (scheme == "prefer-light") "light"
    else if (nzchar(theme)) .appearance_theme_name(theme)
    else if (scheme == "default") "light" else "unknown"
  stopifnot("GNOME appearance must be light, dark, or unknown" = .is_text(out) &&
              out %in% c("light", "dark", "unknown"))
  out
}

#' Infer Light or Dark from a GTK Theme Name
#' @param name One nonempty theme name.
#' @return Light, dark, or unknown for an unfamiliar custom theme name.
#' @keywords internal
#' @noRd
.appearance_theme_name <- function(name) {
  stopifnot("theme name must be nonempty" = .is_text(name))
  name <- tolower(name)
  out <- if (grepl("(^|[-_: ])dark($|[-_: ])", name)) "dark"
    else if (grepl("(^|[-_: ])light($|[-_: ])", name) ||
      name %in% c("adwaita", "yaru", "radiance", "highcontrast")) "light" else "unknown"
  stopifnot("theme-name appearance must be supported" = out %in% c("light", "dark", "unknown"))
  out
}
