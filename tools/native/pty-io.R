# Native I/O protocol test driven by pty-io.py, outside the R unit test runner.
dll_path <- Sys.getenv("PRISM_NATIVE_DLL", "")
if (nzchar(dll_path)) {
  dll <- dyn.load(dll_path)
  native <- list2env(getDLLRegisteredRoutines(dll)[[".Call"]],
                    parent = baseenv())
  source(file.path(Sys.getenv("PRISM_SOURCE_ROOT"), "R", "terminal-native.R"),
         local = native)
} else {
  loadNamespace("prismrepl")
  native <- asNamespace("prismrepl")
}

run_native_io <- function() {
  output <- native$term_output_open()
  on.exit(native$term_reset(output = TRUE), add = TRUE)
  stopifnot("native output must open" = !is.null(output))
  input <- native$term_open(mouse = TRUE)
  stopifnot("native input must open" = !is.null(input))

  frame <- paste0("\033[1;1H", "NATIVE_FRAME_\u03b1\U0001f642")
  stopifnot("buffered write returns the UTF-8 payload byte count" =
              native$term_write(frame, cursor = "#A1B2C3") == nchar(frame, type = "bytes"))
  captured <- character()
  connection <- textConnection("captured", "w", local = TRUE)
  sink(connection)
  tryCatch(native$term_write("\033[2;1HNATIVE_SINK_BYPASS", cursor = "#A1B2C3"),
           finally = { sink(); close(connection) })
  stopifnot("native output bypasses R sinks" = length(captured) == 0L)
  native$term_write("\033[3;1HNATIVE_SYNCHRONIZED", synchronize = TRUE, cursor = "#D4E5F6")

  next_event <- function() {
    deadline <- proc.time()[["elapsed"]] + 5
    repeat {
      event <- native$term_read(input, 50L)
      if (!event$type %in% c("timeout", "resize")) return(event)
      stopifnot("expected native input must arrive" = proc.time()[["elapsed"]] < deadline)
    }
  }
  native$term_write("\033[4;1HREADY_NATIVE_EVENTS")
  press <- next_event()
  stopifnot("left press uses one-based viewport coordinates" =
              identical(press$type, "mouse") && identical(press$key, "left") &&
              identical(press$button, "left") && identical(press$action, "press") &&
              identical(c(press$x, press$y), c(12L, 8L)))
  drag <- next_event()
  stopifnot("Ctrl-drag motion must preserve coordinates and rectangle modifier" =
              identical(drag$key, "left") && identical(drag$action, "move") &&
              identical(drag$modifiers, "ctrl") && identical(c(drag$x, drag$y), c(14L, 9L)))
  release <- next_event()
  stopifnot("release keeps the button" = identical(release$key, "left") &&
              identical(release$action, "release"))
  modified <- next_event()
  stopifnot("mouse modifiers are decoded" = identical(modified$key, "right") &&
              identical(modified$modifiers, c("ctrl", "shift")))
  up <- next_event()
  down <- next_event()
  stopifnot("wheel events keep their direction" = identical(up$key, "wheelup") &&
              identical(up$action, "wheel") && identical(down$key, "wheeldown"))
  newline <- next_event()
  enter <- next_event()
  stopifnot("Ctrl+J is distinct from Enter" = identical(newline$key, "j") &&
              identical(newline$modifiers, "ctrl") && identical(enter$key, "enter"))
  paste <- next_event()
  stopifnot("bracketed paste preserves literal Unicode and line feeds" =
              identical(paste$type, "paste") &&
              identical(paste$text, "\u03b1 <- 1\n\u03b1 + 1\n"))
  native$term_close(input)
  native$term_write("\033[5;1HNATIVE_INPUT_CLOSE_CURSOR", cursor = "#D4E5F6")
  native$term_write("\033[6;1HNATIVE_CURSOR_RESET", cursor = "")
  native$term_write("\033[6;1HNATIVE_CURSOR_RESET_AGAIN", cursor = "")
  native$term_write("\033[7;1HNATIVE_CURSOR_SUSPEND_BEGIN", cursor = "#010203")
  native$term_output_screen(FALSE)
  native$term_write("\033[7;1HNATIVE_CURSOR_SUSPEND_END")
  native$term_write("\033[7;1HNATIVE_CURSOR_RESUME", cursor = "#010203")

  input <- native$term_open(mouse = FALSE)
  native$term_write("\033[5;1HREADY_MOUSE_DISABLED")
  after <- next_event()
  stopifnot("unrequested mouse reports do not enter the edit buffer" =
              identical(after$type, "text") && identical(after$text, "x") &&
              is.na(after$x) && is.na(after$y) && identical(after$action, ""))

  frame <- paste0(vapply(seq_len(24L), function(row) {
    paste0("\033[", row, ";1H", strrep("x", 78L), "\033[K")
  }, character(1L)), collapse = "")
  started <- proc.time()[["elapsed"]]
  invisible(lapply(seq_len(250L), function(i) native$term_write(frame)))
  elapsed <- proc.time()[["elapsed"]] - started
  native$term_close(input)
  native$term_output_close(output)
  native$term_output_close(output)
  rejected <- tryCatch(native$term_write("UNOWNED_CURSOR_BAD", cursor = "#123456"), error = identity)
  stopifnot("cursor color must require an output owner" = inherits(rejected, "error"))
  native$term_write("NATIVE_CURSOR_UNOWNED_RESET", cursor = "")
  untouched <- native$term_output_open()
  native$term_write("NATIVE_CURSOR_UNTOUCHED", cursor = "")
  native$term_output_close(untouched)
  failed <- tryCatch((function() {
    guard <- native$term_output_open()
    on.exit(native$term_output_close(guard), add = TRUE)
    native$term_write("NATIVE_CURSOR_FAILURE_BEGIN", cursor = "#334455")
    stop("intentional cursor cleanup test", call. = FALSE)
  })(), error = identity)
  stopifnot("errors must unwind output ownership" = inherits(failed, "error") &&
              !native$term_output_state()$active)
  cat(sprintf("\r\nNATIVE_IO_OK frames=250 elapsed=%.3fs\r\n", elapsed))
  invisible(NULL)
}
run_native_io()
