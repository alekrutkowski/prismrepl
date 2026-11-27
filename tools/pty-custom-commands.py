"""Exercise persisted custom commands in a fresh, real terminal session.

Run after installing prismrepl. PRISM_R selects R and R_LIBS can select an
isolated library. Only the Python standard library is required. Registration
runs in a separate R process, and all files live in a private temporary folder.
The shared VT model checks visible results, staged palette actions, keyboard
completion, condition badges, and exact terminal restoration.
"""
import ast
import codecs
import fcntl
import os
from pathlib import Path
import pty
import select
import shutil
import struct
import subprocess
import sys
import tempfile
import termios
import time
import unicodedata

from pty_support import terminal_snapshot


source = Path(__file__).with_name("pty-fullscreen.py")
model_ast = next(node for node in ast.parse(source.read_text(encoding="utf-8")).body
                 if isinstance(node, ast.ClassDef) and node.name == "Screen")
namespace = {"codecs": codecs, "unicodedata": unicodedata}
exec(compile(ast.Module(body=[model_ast], type_ignores=[]), str(source), "exec"), namespace)
Screen = namespace["Screen"]

if os.name != "posix":
    raise SystemExit("This integration test requires a POSIX pseudo-terminal.")


REGISTRATION = r'''
file <- Sys.getenv("PRISMREPL_COMMANDS_FILE")
prismrepl::repl_add_command("custom-zero", function() {
  cat("CUSTOM_ZERO_RESULT\n")
  invisible("zero")
}, alias = "zx", description = "Run the zero-argument terminal fixture", file = file)
prismrepl::repl_add_command("custom-one", local({
  prefix <- "CUSTOM_ONE_RESULT"
  function(text) {
    stopifnot(is.character(text), length(text) == 1L, !is.na(text))
    cat(prefix, "=<", text, ">\n", sep = "")
    invisible(text)
  }
}), description = "Restore the captured phrase prefix", file = file)
prismrepl::repl_add_command("custom-many", function(first, second) {
  stopifnot(is.character(first), length(first) == 1L, !is.na(first),
            is.character(second), length(second) == 1L, !is.na(second))
  cat("CUSTOM_MANY_RESULT=<", first, ">|<", second, ">\n", sep = "")
  invisible(c(first, second))
}, description = "Preserve quoted multi-argument strings", file = file)
prismrepl::repl_add_command("custom-dots", function(...) {
  values <- list(...)
  stopifnot(all(vapply(values, function(x)
    is.character(x) && length(x) == 1L && !is.na(x), logical(1))))
  cat("CUSTOM_DOTS_RESULT=", paste0("<", unlist(values), ">", collapse = "|"),
      "\n", sep = "")
  invisible(values)
}, description = "Preserve variadic string arguments", file = file)
prismrepl::repl_add_command("custom-error", function() {
  message("CUSTOM_MESSAGE_FIRST\nCUSTOM_MESSAGE_SECOND")
  warning("CUSTOM_WARNING_TEXT", call. = FALSE)
  stop("CUSTOM_ERROR_FIRST\nCUSTOM_ERROR_SECOND", call. = FALSE)
}, description = "Render normal custom command conditions", file = file)
stopifnot(nrow(prismrepl::repl_custom_commands(file)) == 5L, file.exists(file))
cat("CUSTOM_REGISTRATION_OK\n")
'''


def exercise(directory):
    environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor",
                       PRISMREPL_COMMANDS_FILE=str(directory / "commands.rds"))
    environment.pop("NO_COLOR", None)
    executable = os.environ.get("PRISM_R", "R")
    executable = shutil.which(executable) or str(Path(executable).resolve())
    registration = directory / "register.R"
    registration.write_text(REGISTRATION, encoding="utf-8")
    registered = subprocess.run([executable, "--vanilla", "--slave", "-f", str(registration)],
                                env=environment, cwd=directory, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=30)
    assert registered.returncode == 0, registered.stdout
    assert "CUSTOM_REGISTRATION_OK" in registered.stdout, registered.stdout

    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 120, 0, 0))
    original_modes = terminal_snapshot(slave)
    model = Screen(30, 120)
    raw = bytearray()
    process = subprocess.Popen([executable, "--vanilla", "--quiet", "--interactive"],
                               stdin=slave, stdout=slave, stderr=slave,
                               env=environment, cwd=directory, close_fds=True)

    def collect(seconds=0.05):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            ready, _, _ = select.select([master], [], [], max(0, deadline - time.monotonic()))
            if not ready:
                break
            try:
                chunk = os.read(master, 65536)
            except OSError:
                break
            if not chunk:
                break
            raw.extend(chunk)
            model.feed(chunk)

    def wait_until(check, description, timeout=25):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            collect()
            if check():
                return
            if process.poll() is not None:
                break
        raise AssertionError(f"Missing {description}\n{model.text()}")

    def wait_for(marker):
        wait_until(lambda: marker in model.text(), repr(marker))

    def send(text):
        os.write(master, text.encode("utf-8"))

    def ready_editor(staged="", after=0):
        def ready():
            opened = raw.rfind(b"\x1b[?2004h")
            closed = raw.rfind(b"\x1b[?2004l")
            flags = termios.tcgetattr(slave)[3]
            current = "".join(model.cells[model.row]).strip()
            return (opened >= after and opened > closed and model.cursor_visible and
                    not flags & (termios.ICANON | termios.ECHO) and
                    current == ("r> " + staged).rstrip())
        wait_until(ready, f"a fresh editable prompt {staged!r}")

    def paste(text):
        send("\x1b[200~" + text + "\x1b[201~")
        collect()

    def expression(text):
        ready_editor()
        after = len(raw)
        paste(text)
        wait_for(text)
        send("\r")
        return after

    def submit(text):
        after = expression(text)
        ready_editor(after=after)

    def verify(assertion, marker):
        submit(assertion + ';cat(paste0("CHECK_","' + marker + '\\n"))')
        wait_for("CHECK_" + marker)

    try:
        wait_for(">")
        bootstrap = (
            'fixture <- new.env(parent=baseenv());'
            'result <- prismrepl::repl(envir=fixture,mode="terminal",'
            'config=prismrepl::repl_config(ui="fullscreen",theme="black",'
            'color=TRUE,color_depth=16777216L,settings_file=NULL,'
            'commands_file=Sys.getenv("PRISMREPL_COMMANDS_FILE"),'
            'history_file=NULL,persist=FALSE,sidebar=FALSE,status=character(),'
            'mouse=TRUE,hyperlinks=TRUE));'
            'stopifnot(all(c(":zx",":commands",":custom-error",'
            '":custom-many \\\"alpha  beta\\\" NA",'
            '":custom-dots first \\\"second  argument\\\" \\\"\\\"") %in% result$history),'
            'identical(result$last_error$code,":custom-error"),'
            'length(result$last_error$messages)==1L,length(result$last_error$warnings)==1L,'
            'identical(conditionMessage(result$last_error$error),'
            '"CUSTOM_ERROR_FIRST\\nCUSTOM_ERROR_SECOND"));'
            'cat(paste0("CUSTOM_","PTY_DONE\\n"));q("no")'
        )
        send(bootstrap + "\n")
        ready_editor()

        submit(":zx")
        wait_for("CUSTOM_ZERO_RESULT")
        verify('stopifnot(identical(.Last.value,"zero"))', "ZERO")

        paste(":custom-ma")
        send("\t")
        ready_editor(":custom-many")
        send(' "alpha  beta" NA\r')
        wait_for("CUSTOM_MANY_RESULT=<alpha  beta>|<NA>")
        ready_editor()
        verify('stopifnot(identical(.Last.value,c("alpha  beta","NA")))', "MANY")

        submit(':custom-dots first "second  argument" ""')
        wait_for("CUSTOM_DOTS_RESULT=<first>|<second  argument>|<>")
        verify('stopifnot(identical(.Last.value,list("first","second  argument","")))', "DOTS")

        # Search a description and stage the selected command. Its function must
        # not run until the staged input is completed and approved with Enter.
        before_palette = len(raw)
        expression(":commands")
        wait_for("Command palette")
        paste("captured phrase prefix")
        wait_for("1/5 commands")
        assert ":custom-one" in model.text(), model.text()
        send("\r")
        ready_editor(":custom-one")
        assert b"CUSTOM_ONE_RESULT" not in raw[before_palette:], "Palette selection executed a command."
        send("hello  café  world\r")
        wait_for("CUSTOM_ONE_RESULT=<hello  café  world>")
        ready_editor()
        verify('stopifnot(identical(.Last.value,"hello  café  world"))', "ONE")

        submit(":clear")
        submit(":custom-error")
        for marker in ("CUSTOM_MESSAGE_FIRST", "CUSTOM_MESSAGE_SECOND", "CUSTOM_WARNING_TEXT",
                       "CUSTOM_ERROR_FIRST", "CUSTOM_ERROR_SECOND"):
            wait_for(marker)
        assert all(badge in model.text() for badge in ("[MESSAGE]", "[WARNING]", "[ERROR]")), model.text()
        assert "ERROR" in "".join(model.cells[0]), model.text()
        assert len(model.sgr) >= 6, "Condition rendering did not use the theme's distinct styles."
        submit('cat(paste0("CUSTOM_","RECOVERY_OK\\n"))')
        wait_for("CUSTOM_RECOVERY_OK")

        expression(":quit")
        wait_until(lambda: "CUSTOM_PTY_DONE" in model.text().splitlines(), "successful final R assertions")
        process.wait(timeout=10)
        assert process.returncode == 0, process.returncode
        assert terminal_snapshot(slave) == original_modes
        assert model.enters == model.leaves == 1
        assert model.alternate_scrolls == 0
        assert not model.bad_cursor, model.bad_cursor
        print("PASS: persisted closures across R processes, custom alias and Tab completion,")
        print("description-search palette staging, single/multiple/variadic string arguments,")
        print("condition blocks and error recovery, colon history, and exact terminal restoration.")
    except BaseException:
        sys.stderr.write(model.text() + "\n")
        raise
    finally:
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
        if os.environ.get("PRISM_PTY_ANSI"):
            Path(os.environ["PRISM_PTY_ANSI"]).write_bytes(raw)
        os.close(master)
        os.close(slave)


if __name__ == "__main__":
    with tempfile.TemporaryDirectory(prefix="prism-custom-commands-") as temporary:
        exercise(Path(temporary))
