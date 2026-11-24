test_that("Windows uses the application appearance without starting a process", {
  no_command <- function(...) stop("command must not run")
  detect <- function(value) .detect_appearance("Windows", character(),
    registry = function() list(AppsUseLightTheme = value), command = no_command)
  expect_identical(detect(0L), "dark")
  expect_identical(detect(1L), "light")
  expect_identical(detect(2L), "unknown")
  expect_identical(detect(NULL), "unknown")
  expect_identical(detect("1"), "unknown")
  expect_identical(.detect_appearance("Windows", character(),
    registry = function() stop("not readable"), command = no_command), "unknown")
  expect_identical(.detect_appearance("Windows", character(),
    registry = function() NA, command = no_command), "unknown")
})

test_that("GNOME preference detection uses one fixed read and handles old schemas", {
  calls <- list()
  output <- c("org.gnome.desktop.interface color-scheme 'prefer-dark'",
              "org.gnome.desktop.interface gtk-theme 'Yaru'")
  reader <- function(program, args) {
    calls[[length(calls) + 1L]] <<- list(program = program, args = args)
    list(output = output, status = 0L)
  }
  detect <- function() .detect_appearance("Linux",
    c(XDG_CURRENT_DESKTOP = "ubuntu:GNOME"), command = reader)
  expect_identical(detect(), "dark")
  expect_length(calls, 1L)
  expect_identical(calls[[1L]], list(program = "gsettings",
    args = c("list-recursively", "org.gnome.desktop.interface")))
  output <- c("org.gnome.desktop.interface color-scheme 'prefer-light'",
              "org.gnome.desktop.interface gtk-theme 'Yaru-dark'")
  expect_identical(detect(), "light")
  output <- "org.gnome.desktop.interface color-scheme 'default'"
  expect_identical(detect(), "light")
  output <- "org.gnome.desktop.interface gtk-theme 'Adwaita-dark'"
  expect_identical(detect(), "dark")
  output <- "org.gnome.desktop.interface gtk-theme 'Yaru'"
  expect_identical(detect(), "light")
  output <- "org.gnome.desktop.interface gtk-theme 'UnfamiliarCustomTheme'"
  expect_identical(detect(), "unknown")
  output <- c("org.gnome.desktop.interface color-scheme 'prefer-dark'",
              "org.gnome.desktop.interface gtk-theme 'Bad\033[0m'")
  expect_identical(detect(), "dark")
})

test_that("appearance fallbacks are read-only and do not query unrelated desktops", {
  calls <- 0L
  no_command <- function(...) { calls <<- calls + 1L; stop("command must not run") }
  expect_identical(.detect_appearance("Linux", character(), command = no_command), "unknown")
  expect_identical(.detect_appearance("Linux", c(XDG_CURRENT_DESKTOP = "KDE"),
    command = no_command), "unknown")
  expect_identical(.detect_appearance("Linux", c(GTK_THEME = "Adwaita:dark"),
    command = no_command), "dark")
  expect_identical(.detect_appearance("Linux", c(GTK_THEME = "Yaru-light"),
    command = no_command), "light")
  expect_identical(.detect_appearance("Linux", c(GTK_THEME = "Nordic"),
    command = no_command), "unknown")
  expect_identical(.detect_appearance("Linux", c(COLORFGBG = "15;0"),
    command = no_command), "dark")
  expect_identical(.detect_appearance("Linux", c(COLORFGBG = "0;15"),
    command = no_command), "light")
  expect_identical(.detect_appearance("Linux", c(COLORFGBG = "0;2"),
    command = no_command), "unknown")
  expect_equal(calls, 0L)
  expect_identical(.detect_appearance("Linux", c(XDG_CURRENT_DESKTOP = "GNOME"),
    command = function(...) stop("timed out")), "unknown")
  expect_identical(.detect_appearance("Linux", c(XDG_CURRENT_DESKTOP = "GNOME"),
    command = function(...) list(output = "error", status = 124L)), "unknown")
  expect_identical(.detect_appearance("Linux", c(XDG_CURRENT_DESKTOP = "GNOME"),
    command = function(...) list(output = NA_character_, status = 0L)), "unknown")
})

test_that("macOS defaults are bounded and missing light-mode keys are understood", {
  detect <- function(output, status) .detect_appearance("Darwin", character(),
    command = function(program, args) {
      expect_identical(program, "defaults")
      expect_identical(args, c("read", "-g", "AppleInterfaceStyle"))
      list(output = output, status = status)
    })
  expect_identical(detect("Dark", 0L), "dark")
  expect_identical(detect("Light", 0L), "light")
  expect_identical(detect(character(), 1L), "light")
  expect_identical(detect(character(), 124L), "unknown")
  expect_identical(detect(character(), 127L), "unknown")
  expect_identical(detect("unknown-value", 0L), "unknown")
})

test_that("initial appearance is cached lazily and explicitly refreshable", {
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  mode <- "light"
  reader <- function() { calls <<- calls + 1L; mode }
  expect_identical(.initial_theme(detector = reader, cache = cache), "paper")
  expect_equal(calls, 1L)
  mode <- "dark"
  expect_identical(.initial_theme(detector = reader, cache = cache), "paper")
  expect_equal(calls, 1L)
  expect_identical(.initial_theme(TRUE, detector = reader, cache = cache), "midnight")
  expect_equal(calls, 2L)
  expect_identical(.initial_theme(TRUE, detector = function() stop("unavailable"),
    cache = cache), "midnight")
  expect_identical(.initial_theme(TRUE, detector = function() "unknown",
    cache = cache), "midnight")
})

test_that("appearance queries have a one-second timeout and safe arguments", {
  calls <- list()
  result <- .appearance_command("gsettings",
    c("list-recursively", "org.gnome.desktop.interface"),
    locate = function(...) "/some path/gsettings",
    run = function(command, args, stdout, stderr, timeout) {
      calls <<- list(command = command, args = args, stdout = stdout,
                     stderr = stderr, timeout = timeout)
      c("line one", "line two")
    })
  expect_identical(result, list(output = c("line one", "line two"), status = 0L))
  expect_identical(calls$command, "/some path/gsettings")
  expect_identical(calls$args, vapply(c("list-recursively", "org.gnome.desktop.interface"),
                                    shQuote, character(1)))
  expect_identical(calls$stdout, TRUE)
  expect_identical(calls$stderr, FALSE)
  expect_identical(calls$timeout, 1L)
})

test_that("unavailable preference tools and invalid detector inputs fail predictably", {
  expect_identical(.appearance_command("missing", character(), locate = function(...) ""),
                   list(output = character(), status = 127L))
  expect_error(.detect_appearance(NA_character_), "platform")
  expect_error(.detect_appearance("Linux", "unnamed"), "variables")
  expect_error(.initial_theme(refresh = NA), "refresh")
  expect_error(.appearance_command("", character()), "program")
  expect_error(.appearance_theme_name(""), "theme name")
})
