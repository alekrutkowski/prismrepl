# Driven by windows-console-smoke.ps1 inside an actual Windows pseudoconsole.
.prism_windows_test <- function() {
  ns <- asNamespace("prismrepl")
  snapshot <- function() {
    current <- ns$term_output_state()
    as.integer(unlist(current[c("stdout_mode", "stderr_mode", "codepage")],
                      use.names = FALSE))
  }
  stopifnot("a Windows console is required" =
              identical(ns$term_capabilities()$platform, "windows") &&
              ns$term_capabilities()$supported)
  probe <- dyn.load(Sys.getenv("PRISM_CONSOLE_PROBE"))
  set_modes <- getNativeSymbolInfo("console_test_set", probe)
  input_mode <- getNativeSymbolInfo("console_test_input_mode", probe)
  send_mouse <- getNativeSymbolInfo("console_test_mouse", probe)
  send_key <- getNativeSymbolInfo("console_test_key", probe)
  original <- snapshot()
  on.exit({
    ns$term_reset(output = TRUE)
    .Call(set_modes, original)
    dyn.unload(probe[["path"]])
  }, add = TRUE)
  # Set a controlled baseline that would expose the original lifetime bug.
  seeded <- c(bitwAnd(original[[1L]], bitwNot(4L)),
              bitwAnd(original[[2L]], bitwNot(4L)), 437L)
  stopifnot("controlled console baseline must be applied" = .Call(set_modes, seeded))
  stopifnot("baseline must have VT disabled and code page 437" =
              identical(snapshot(), seeded) && !ns$term_output_state()$vt_stdout)
  output <- ns$term_output_open()
  stopifnot("session output must open" = !is.null(output))
  enabled <- ns$term_output_state()
  stopifnot("VT and UTF-8 output must be enabled" = enabled$active &&
              enabled$vt_stdout && enabled$vt_stderr && enabled$codepage == 65001L)
  # WriteConsoleW must preserve Unicode even if a third-party package changes
  # the output code page. The native frame must bypass R's text sink.
  stopifnot("test output page must change" = .Call(set_modes, c(NA_integer_, NA_integer_, 437L)))
  frame <- "NATIVE_FRAME_\u6f22\u5b57 \U0001f9ea\r\n"
  captured <- character()
  connection <- textConnection("captured", "w", local = TRUE)
  sink(connection)
  bytes <- tryCatch(ns$term_write(frame, cursor = "#A1B2C3"), finally = { sink(); close(connection) })
  stopifnot("native frames must bypass output sinks" = length(captured) == 0L,
            "writer must report payload bytes" = bytes == nchar(frame, type = "bytes"))
  stopifnot("UTF-8 output page must resume" = .Call(set_modes, c(NA_integer_, NA_integer_, 65001L)))
  original_input <- .Call(input_mode)
  input <- ns$term_open(mouse = TRUE)
  stopifnot("native input must open" = !is.null(input))
  stopifnot("mouse input must be enabled" = bitwAnd(.Call(input_mode), 16L) != 0L)
  next_event <- function() {
    deadline <- proc.time()[["elapsed"]] + 3
    repeat {
      event <- ns$term_read(input, 50L)
      if (!event$type %in% c("timeout", "resize")) return(event)
      stopifnot("injected console input must arrive" = proc.time()[["elapsed"]] < deadline)
    }
  }
  stopifnot("mouse press must be injected" = .Call(send_mouse, c(5L, 7L, 1L, 0L, 0L)))
  event <- next_event()
  stopifnot("clicks must use viewport coordinates" = identical(event$type, "mouse") &&
              identical(event$key, "left") && identical(event$action, "press") &&
              identical(c(event$x, event$y), c(5L, 7L)))
  stopifnot("Ctrl-drag must be injected" = .Call(send_mouse, c(8L, 9L, 1L, 1L, 8L)))
  event <- next_event()
  stopifnot("Ctrl-drag keeps its button, position and modifier" =
              identical(event$key, "left") && identical(event$action, "move") &&
              identical(event$modifiers, "ctrl") && identical(c(event$x, event$y), c(8L, 9L)))
  stopifnot("mouse release must be injected" = .Call(send_mouse, c(5L, 7L, 0L, 0L, 0L)))
  event <- next_event()
  stopifnot("release must retain its button" = identical(event$key, "left") &&
              identical(event$action, "release"))
  stopifnot("wheel event must be injected" =
              .Call(send_mouse, c(5L, 7L, bitwShiftL(120L, 16L), 4L, 0L)))
  event <- next_event()
  stopifnot("positive wheel delta must scroll up" = identical(event$key, "wheelup") &&
              identical(event$action, "wheel"))
  stopifnot("Ctrl+J must be injected" = .Call(send_key, c(74L, 10L, 8L)))
  event <- next_event()
  stopifnot("Ctrl+J must remain distinct from Enter" = identical(event$key, "j") &&
              identical(event$modifiers, "ctrl"))
  stopifnot("Enter must be injected" = .Call(send_key, c(13L, 13L, 0L)))
  stopifnot("VK_RETURN must submit" = identical(next_event()$key, "enter"))
  shortcuts <- list(
    list(fields = c(187L, 61L, 8L), key = "=", modifiers = "ctrl"),
    list(fields = c(49L, 49L, 8L), key = "1", modifiers = "ctrl"),
    list(fields = c(49L, 0L, 8L), key = "1", modifiers = "ctrl"),
    list(fields = c(32L, 0L, 8L), key = " ", modifiers = "ctrl"),
    list(fields = c(188L, 44L, 2L), key = ",", modifiers = "alt"),
    list(fields = c(187L, 61L, 10L), key = "=", modifiers = c("ctrl", "alt")),
    list(fields = c(220L, 28L, 8L), key = "\\", modifiers = "ctrl"),
    list(fields = c(77L, 13L, 24L), key = "m", modifiers = c("ctrl", "shift")),
    list(fields = c(124L, 0L, 0L), key = "f13", modifiers = character()),
    list(fields = c(135L, 0L, 8L), key = "f24", modifiers = "ctrl")
  )
  invisible(lapply(shortcuts, function(shortcut) {
    stopifnot("shortcut must be injected" = .Call(send_key, shortcut$fields))
    event <- next_event()
    stopifnot("custom shortcuts must preserve keys and modifiers" =
                identical(event$type, "key") && identical(event$key, shortcut$key) &&
                identical(event$modifiers, shortcut$modifiers))
  }))
  stopifnot("AltGr character must be injected" = .Call(send_key, c(81L, 64L, 9L)))
  event <- next_event()
  stopifnot("AltGr must remain text input" = identical(event$type, "text") &&
              identical(event$text, "@") && identical(event$modifiers, character()))
  if (identical(Sys.getenv("PRISM_TEST_CLIPBOARD"), "1")) {
    # This is enabled on disposable CI runners. A developer can opt in, but a
    # non-text clipboard cannot be faithfully saved by a Unicode-only probe.
    clipboard_before <- .Call(ns$C_clipboard_read, 1048576L)
    on.exit(if (clipboard_before$ok)
      try(.Call(ns$C_clipboard_write, clipboard_before$text), silent = TRUE), add = TRUE)
    cfg <- ns$repl_config(settings_file = NULL, mouse = TRUE, color = TRUE)
    rows <- rep(strrep(" ", 30L), 15L)
    rows[c(10L, 11L)] <- c("alpha \u03b2eta 123", "gamma \u03b4elta 45")
    ns$term_write(paste0("\033[10;1H", rows[[10L]], "\033[11;1H", rows[[11L]]))
    selected <- ns$.mouse_state()
    stopifnot("selection press must be injected" = .Call(send_mouse, c(2L, 10L, 1L, 0L, 8L)))
    routed <- ns$.mouse_dispatch(next_event(), rows, selected, cfg)
    stopifnot("selection must defer click activation" = is.null(routed$event))
    stopifnot("selection drag must be injected" = .Call(send_mouse, c(7L, 11L, 1L, 1L, 8L)))
    routed <- ns$.mouse_dispatch(next_event(), rows, routed$selection, cfg)
    expected <- ns$.mouse_text(routed$selection)
    stopifnot("Ctrl-drag must produce a rectangular Unicode selection" =
      routed$selection$rectangle && identical(expected, "lpha \u03b2\namma \u03b4"))
    stopifnot("selection release must be injected" = .Call(send_mouse, c(7L, 11L, 0L, 0L, 8L)))
    routed <- ns$.mouse_dispatch(next_event(), rows, routed$selection, cfg)
    clipboard_after <- .Call(ns$C_clipboard_read, 1048576L)
    stopifnot("releasing a selection must copy CF_UNICODETEXT" =
      clipboard_after$ok && identical(gsub("\r\n", "\n", clipboard_after$text), expected))
    stopifnot("right click must be injected" = .Call(send_mouse, c(3L, 11L, 2L, 0L, 0L)))
    routed <- ns$.mouse_dispatch(next_event(), rows, routed$selection, cfg)
    stopifnot("right click must read the system clipboard as a paste event" = identical(routed$event$type, "paste"))
    editor <- ns$.editor_step(ns$.editor_state(), routed$event, character(), new.env(), cfg)
    stopifnot("clipboard paste must await explicit Enter" = identical(editor$action, "edit") &&
      identical(paste0(editor$chars, collapse = ""), expected))
    stopifnot("right release must be injected" = .Call(send_mouse, c(3L, 11L, 0L, 0L, 0L)))
    stopifnot("right release must retain its button" = identical(next_event()$button, "right"))
    ns$term_write("\033[12;1HNATIVE_CLIPBOARD_OK\r\n")
  }
  ns$term_close(input)
  ns$term_write("NATIVE_CURSOR_INPUT_CLOSED\r\n", cursor = "#A1B2C3")
  ns$term_write("NATIVE_CURSOR_EXPLICIT_RESET\r\n", cursor = "")
  ns$term_write("NATIVE_CURSOR_SUSPEND\r\n", cursor = "#D4E5F6")
  ns$term_output_screen(FALSE)
  ns$term_write("NATIVE_CURSOR_RESUMED\r\n", cursor = "#D4E5F6")
  stopifnot("mouse and keyboard input modes must be restored" =
              identical(.Call(input_mode), original_input))
  stopifnot("closing an input line must preserve session output" =
              identical(ns$term_output_state(), enabled))
  ns$term_reset()
  stopifnot("editor recovery must preserve session output" =
              identical(ns$term_output_state(), enabled))
  ns$term_output_close(output)
  ns$term_output_close(output)
  stopifnot("closing output must restore both modes and code page" =
              identical(snapshot(), seeded) && !ns$term_output_state()$active)
  stopifnot("original console settings must be restored" = .Call(set_modes, original))
  cat("NATIVE_OUTPUT_GUARD_OK\n")
  before_repl <- snapshot()
  prismrepl::repl(config = prismrepl::repl_config(
    ui = "fullscreen", theme = "black", color = TRUE, sidebar = TRUE, history_file = NULL,
    settings_file = NULL, mouse = TRUE, confirm = FALSE, link_context = FALSE))
  stopifnot("the REPL must restore its output modes and code page" =
              identical(snapshot(), before_repl) && !ns$term_output_state()$active)
  cat("WINDOWS_SESSION_RESTORE_OK\n")
  invisible(NULL)
}
.prism_windows_test()
quit(save = "no", status = 0L)
