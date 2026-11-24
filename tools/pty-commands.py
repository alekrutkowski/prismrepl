"""Exercise staged palette commands, command history and function editing.

Run against the installed package with PRISM_R selecting the R executable.
This test uses a real POSIX PTY and the same small VT model as the fullscreen
test. No packages are installed and no user's function or history is touched.
"""
import ast
import codecs
import fcntl
import os
from pathlib import Path
import pty
import select
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time
import unicodedata


source = Path(__file__).with_name("pty-fullscreen.py")
model_ast = next(node for node in ast.parse(source.read_text(encoding="utf-8")).body
                 if isinstance(node, ast.ClassDef) and node.name == "Screen")
namespace = {"codecs": codecs, "unicodedata": unicodedata}
exec(compile(ast.Module(body=[model_ast], type_ignores=[]), str(source), "exec"), namespace)
Screen = namespace["Screen"]

if os.name != "posix":
    raise SystemExit("This integration test requires a POSIX pseudo-terminal.")

master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 120, 0, 0))
original_modes = termios.tcgetattr(slave)
model = Screen(30, 120)
raw = bytearray()
environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor")
environment.pop("NO_COLOR", None)
process = subprocess.Popen([os.environ.get("PRISM_R", "R"), "--vanilla", "--quiet", "--interactive"],
                           stdin=slave, stdout=slave, stderr=slave,
                           env=environment, close_fds=True)
temporary = tempfile.TemporaryDirectory(prefix="prism-command-path-")
script = Path(temporary.name) / "folder with spaces" / "my file.R"
script.parent.mkdir()
script.write_text('sourced <- 42L\ncat(paste0("LITERAL_", "SOURCE_OK\\n"))\n', encoding="utf-8")


def collect(seconds=0.1):
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


def wait_for(marker, timeout=20):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        collect(0.05)
        if marker in model.text():
            return
        if process.poll() is not None:
            break
    raise AssertionError(f"Missing live-screen marker {marker!r}\n{model.text()}")


def send(text):
    os.write(master, text.encode("utf-8"))


def paste(text):
    send("\x1b[200~" + text + "\x1b[201~")
    collect(0.08)


def expression(text):
    paste(text)
    send("\r")


def replace_function(text):
    send("\x1b[1;5H" + "\x0b" * 12)
    collect(0.1)
    paste(text)


def resize(rows, columns):
    model.resize(rows, columns)
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
    os.kill(process.pid, signal.SIGWINCH)
    collect(0.1)


try:
    wait_for(">")
    bootstrap = (
        'fixture <- new.env(parent=baseenv()); fixture$f <- function(x) x+1L; '
        'result <- prismrepl::repl(envir=fixture,config=prismrepl::repl_config('
        'ui="fullscreen",color=TRUE,color_depth=16777216L,settings_file=NULL,'
        'history_file=NULL,persist=FALSE,mouse=TRUE,status=character(),hyperlinks=FALSE));'
        'stopifnot(identical(fixture$f(3L),5L), ":help" %in% result$history,'
        '":edit f" %in% result$history, ":h" %in% result$history, ":pwd" %in% result$history,'
        'identical(fixture$sourced,42L),'
        '!":clear" %in% result$history);'
        'cat("COMMANDS_PTY_DONE\\n");q("no")'
    )
    send(bootstrap + "\n")
    wait_for("PRISM REPL")

    expression(":help")
    wait_for("Command palette")
    paste("function in a small")
    wait_for("1/37 commands")
    assert ":edit NAME" in model.text(), model.text()
    send("\t")
    wait_for("alphabetical")
    resize(20, 60)
    wait_for("Command palette")
    assert ":edit NAME" in model.text(), model.text()
    send("\r")
    wait_for("PRISM REPL")
    wait_for("r> :edit")
    assert "EDIT FUNCTION" not in model.text(), model.text()
    send("f\r")
    wait_for("EDIT FUNCTION  f")
    resize(30, 120)
    replace_function("function(x) x + 2L")
    send("\x13")
    wait_for("Saved f in the workspace")
    send("\x18")
    wait_for("PRISM REPL")
    expression('stopifnot(identical(f(3L),5L));cat(paste0("SAVED_","FUNCTION\\n"))')
    wait_for("SAVED_FUNCTION")

    expression(":edit f")
    wait_for("EDIT FUNCTION  f")
    replace_function("function(x) {")
    send("\x13")
    wait_for("Not saved:")
    send("\x18")
    wait_for("PRISM REPL")
    expression('stopifnot(identical(f(3L),5L));cat(paste0("INVALID_","UNCHANGED\\n"))')
    wait_for("INVALID_UNCHANGED")

    expression(":h")
    wait_for("Command palette")
    paste("current working directory")
    wait_for("2/37 commands")
    send("\x1b[F\r")
    wait_for("PRISM REPL")
    wait_for("r> :pwd")
    # Only approval at the ordinary input submits the staged command.
    send("\r")
    collect(0.1)
    expression(":so " + str(script))
    wait_for("LITERAL_SOURCE_OK")

    expression(":h")
    wait_for("Command palette")
    paste("Clear the retained")
    wait_for("1/37 commands")
    row = next(i + 1 for i, line in enumerate(model.text().splitlines()) if ":clear" in line)
    send(f"\x1b[<0;5;{row}M")
    collect(0.1)
    assert "Command palette" in model.text(), "A press must wait for release to permit drag selection."
    send(f"\x1b[<0;5;{row}m")
    wait_for("PRISM REPL")
    wait_for("r> :clear")
    assert "LITERAL_SOURCE_OK" in model.text(), "Staging :clear must not clear the transcript."
    send("\x03")
    collect(0.1)
    expression(":quit")
    wait_for("COMMANDS_PTY_DONE")
    process.wait(timeout=10)
    assert process.returncode == 0, process.returncode
    assert termios.tcgetattr(slave) == original_modes
    assert model.enters == model.leaves == 1
    assert model.alternate_scrolls == 0
    assert not model.bad_cursor, model.bad_cursor
    print("PASS: description search, sort toggle, narrow palette, staged keyboard/mouse commands,")
    print("explicit function save, rejected invalid function, cancelled staged command, colon history,")
    print("shared alternate screen, no rolling redraw, and exact terminal restoration.")
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
    temporary.cleanup()
