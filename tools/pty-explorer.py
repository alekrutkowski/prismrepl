"""Exercise paired shortcut help and structural exploration in a real POSIX PTY.

Run after installing the current package; PRISM_R selects R. Only the Python
standard library is required. The pure Screen model is reused from the adjacent
fullscreen test without executing that test's process-launching code.
"""
import ast
import codecs
import fcntl
import os
from pathlib import Path
import pty
import re
import select
import signal
import struct
import subprocess
import sys
import termios
import time
import unicodedata

source = Path(__file__).with_name("pty-fullscreen.py")
model_ast = next(node for node in ast.parse(source.read_text(encoding="utf-8")).body
                 if isinstance(node, ast.ClassDef) and node.name == "Screen")
model_namespace = {"codecs": codecs, "unicodedata": unicodedata}
exec(compile(ast.Module(body=[model_ast], type_ignores=[]), str(source), "exec"), model_namespace)
Screen = model_namespace["Screen"]

if os.name != "posix":
    raise SystemExit("This integration test requires a POSIX pseudo-terminal.")

master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 32, 120, 0, 0))
original_modes = termios.tcgetattr(slave)
model = Screen(32, 120)
raw = bytearray()
environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor")
environment.pop("NO_COLOR", None)
process = subprocess.Popen([os.environ.get("PRISM_R", "R"), "--vanilla", "--quiet", "--interactive"],
                           stdin=slave, stdout=slave, stderr=slave, env=environment, close_fds=True)


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


def expression(text):
    send("\x1b[200~" + text + "\x1b[201~")
    collect(0.06)
    send("\r")


def resize(rows, columns):
    model.resize(rows, columns)
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
    os.kill(process.pid, signal.SIGWINCH)
    collect(0.15)


def close_viewer():
    send("q")
    wait_for("PRISM REPL")
    assert model.enters == 1 and model.leaves == 0, (model.enters, model.leaves)


try:
    wait_for(">")
    bootstrap = (
        'fixture <- new.env(parent=baseenv()); '
        'fixture$collection <- list(data=datasets::iris,nested=list(beta=pi,vector=1:12)); '
        'fixture$list_frame <- data.frame(id=1:2); '
        'fixture$list_frame$details <- list(list(scores=c(11L,12L)),list(flag=c(TRUE,FALSE))); '
        'makeActiveBinding("guarded",function(v)stop("ACTIVE_BINDING_EXECUTED"),fixture); '
        'delayedAssign("deferred",stop("PROMISE_EXECUTED"),assign.env=fixture); '
        'prismrepl::repl(envir=fixture,config=prismrepl::repl_config(ui="fullscreen",'
        'color=TRUE,color_depth=16777216L,settings_file=NULL,persist=FALSE,mouse=TRUE,'
        'status=c("objects","memory"),hyperlinks=FALSE));'
        'cat("EXPLORER_PTY_DONE\\n");q("no")'
    )
    send(bootstrap + "\n")
    wait_for("PRISM REPL")

    expression(":keys")
    wait_for("Keyboard shortcuts")
    assert any("submit" in line and "Run complete code" in line
               for line in model.text().splitlines()), model.text()
    send("\x1bOP")
    wait_for("Viewer shortcuts")
    wait_for("PageUp/PageDown")
    send("q")
    wait_for("Keyboard shortcuts")
    resize(22, 48)
    wait_for("Keyboard shortcuts")
    assert "Record 1/" in model.text(), model.text()
    assert any(line.strip().startswith("submit") or line.strip().startswith("> submit")
               for line in model.text().splitlines()), model.text()
    assert "Run complete code" in model.text(), model.text()
    close_viewer()

    resize(30, 110)
    expression(":objects collection")
    wait_for("Object explorer: Workspace")
    wait_for("collection")
    send("\r")
    wait_for("Workspace / collection")
    send("\x1b[C")
    wait_for("150 x 5")
    wait_for("Sepal.Length")
    wait_for("Size")
    send("\t")
    wait_for("[Preview]")
    wait_for("Sepal.Length")
    assert "5.1" in model.text(), model.text()
    send("\t")
    wait_for("[Attributes]")
    send("\r")
    wait_for("/ names")
    send("\x1b[D")
    wait_for("[Attributes]")
    send("\t")
    wait_for("[Children]")
    send("s")
    wait_for("s sizes off")
    send("s")
    wait_for("s sizes on")

    # A released short click selects the second column; a second opens it.
    send("\x1b[<0;5;6M\x1b[<0;5;6m")
    collect(0.08)
    assert re.search(r">\s+Sepal.Width", model.text()), model.text()
    send("\x1b[<0;5;6M\x1b[<0;5;6m")
    wait_for("/ Sepal.Width")
    send("\x1b[D")
    wait_for("150 x 5")
    send("\x1b[<65;8;10M")
    collect(0.1)
    assert re.search(r">\s+Species", model.text()), model.text()
    send("/Petal.Width\r")
    wait_for("matching 'Petal.Width'")
    send("\r")
    wait_for("/ Petal.Width")
    send("\x1bOP")
    wait_for("Explorer keyboard and mouse shortcuts")
    send("q")
    wait_for("/ Petal.Width")
    wait_for("[Preview]")
    assert re.search(r"\[1\]\s+0\.2", model.text()), model.text()
    leaf_breadcrumb = model.text().splitlines()[0]
    assert "[[1]]" not in leaf_breadcrumb, leaf_breadcrumb
    send("\r\x1b[C\r\x1b[C")
    wait_for("Attributes remain available.")
    collect(0.1)
    assert model.text().splitlines()[0] == leaf_breadcrumb, model.text()
    assert re.search(r"\[1\]\s+0\.2", model.text()), model.text()
    send("\x1b[D")
    wait_for("150 x 5")
    close_viewer()

    expression(":objects list_frame")
    wait_for("Object explorer: Workspace")
    send("\r")
    wait_for("2 x 2")
    send("\x1b[B\r")
    wait_for("/ details")
    wait_for("[Children]")
    send("\r")
    wait_for("/ [[1]]")
    wait_for("scores")
    send("\r")
    wait_for("/ scores")
    wait_for("[Preview]")
    assert re.search(r"\[1\]\s+11", model.text()), model.text()
    assert re.search(r"\[2\]\s+12", model.text()), model.text()
    list_breadcrumb = model.text().splitlines()[0]
    send("\r\x1b[C")
    wait_for("Attributes remain available.")
    collect(0.1)
    assert model.text().splitlines()[0] == list_breadcrumb, model.text()
    close_viewer()

    expression(":objects deferred")
    wait_for("Object explorer: Workspace")
    send("\r")
    wait_for("Cannot open: promise binding; not evaluated.")
    close_viewer()
    expression(":objects guarded")
    wait_for("Object explorer: Workspace")
    send("\r")
    wait_for("Cannot open: active binding; not evaluated.")
    close_viewer()

    expression('stopifnot(identical(collection$data,datasets::iris));cat(paste0("TREE_","SAFE\\n"))')
    wait_for("TREE_SAFE")
    expression(":quit")
    wait_for("EXPLORER_PTY_DONE")
    process.wait(timeout=10)
    assert process.returncode == 0, process.returncode
    assert termios.tcgetattr(slave) == original_modes
    assert model.enters == model.leaves == 1
    assert not model.bad_cursor, model.bad_cursor
    print("PASS: live paired shortcut help, narrow reflow, pager shortcuts, nested object/dataframe/")
    print("attribute and list-column drill-down with atomic-vector preview leaves, size toggle, name filters, mouse")
    print("selection/double-click/wheel, blocked special bindings, unchanged data, and terminal restoration.")
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
    os.close(master)
    os.close(slave)
