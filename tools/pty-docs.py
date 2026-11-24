"""Exercise rich help, occurrence search and leaf exploration in a real PTY.

Run after installing the current package; PRISM_R selects R. The headless
clipboard check uses an explicit mouse selection and Prism's session fallback.
It does not claim to test a desktop clipboard or graphical terminal emulator.
Only the Python standard library is required.
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
namespace = {"codecs": codecs, "unicodedata": unicodedata}
exec(compile(ast.Module(body=[model_ast], type_ignores=[]), str(source), "exec"), namespace)
Screen = namespace["Screen"]

if os.name != "posix":
    raise SystemExit("This integration test requires a POSIX pseudo-terminal.")

master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 32, 110, 0, 0))
original_modes = termios.tcgetattr(slave)
model = Screen(32, 110)
raw = bytearray()
environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor")
for key in ("NO_COLOR", "DISPLAY", "WAYLAND_DISPLAY", "WSL_DISTRO_NAME"):
    environment.pop(key, None)
process = subprocess.Popen([os.environ.get("PRISM_R", "R"), "--vanilla", "--quiet", "--interactive"],
                           stdin=slave, stdout=slave, stderr=slave, env=environment, close_fds=True)


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


def wait_for(marker, timeout=25):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        collect()
        if marker in model.text():
            return
        if process.poll() is not None:
            break
    raise AssertionError(f"Missing live-screen marker {marker!r}\n{model.text()}")


def send(text):
    os.write(master, text.encode("utf-8"))


def expression(text):
    send("\x1b[200~" + text + "\x1b[201~")
    collect(0.08)
    send("\r")


def search(text):
    send("/" + text + "\r")
    wait_for("Match 1/")
    collect(0.1)
    count = re.search(r"Match 1/(\d+)", model.text())
    assert count, model.text()
    return int(count.group(1))


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
        'fixture <- new.env(parent=baseenv());fixture$alpha <- 12345L;'
        'fixture$tom <- "caf\\u00e9 \\u03bc";'
        'prismrepl::repl(envir=fixture,config=prismrepl::repl_config(ui="fullscreen",'
        'color=TRUE,color_depth=16777216L,settings_file=NULL,persist=FALSE,history_file=NULL,'
        'mouse=TRUE,sidebar=FALSE,status=character(),hyperlinks=FALSE));'
        'cat("DOCS_PTY_DONE\\n");q("no")'
    )
    send(bootstrap + "\n")
    wait_for("PRISM REPL")

    doc_start = len(raw)
    expression(":doc base::mean")
    wait_for("Help: base::mean")
    wait_for("Arithmetic Mean")
    wait_for("mean(x, trim = 0, na.rm = FALSE, ...)")
    assert any(code.startswith("1;") for code in model.sgr), model.sgr
    assert any("38;2;196;181;253" in code for code in model.sgr), model.sgr

    total = search("mean")
    assert total >= 5, total
    send("n")
    wait_for(f"Match 2/{total}")
    send("p")
    wait_for(f"Match 1/{total}")
    assert any("48;2;251;191;36" in code for code in model.sgr), model.sgr
    assert any("48;2;110;231;183" in code for code in model.sgr), model.sgr

    # Both A in Arithmetic and a in Mean occur on the first content line.
    repeated = search("a")
    first_line = re.search(r"Line (\d+)/", model.text()).group(1)
    send("n")
    wait_for(f"Match 2/{repeated}")
    second_line = re.search(r"Line (\d+)/", model.text()).group(1)
    assert first_line == second_line == "1", model.text()
    assert "column 1" in model.text(), model.text()
    send("p")
    wait_for(f"Match 1/{repeated}")

    # Clear highlighting and select the word Mean directly from the heading.
    send("/\r")
    collect(0.1)
    row = next(i for i, line in enumerate(model.text().splitlines(), 1) if "Arithmetic Mean" in line)
    line = model.text().splitlines()[row - 1]
    x = line.index("Mean") + 1
    send(f"\x1b[<0;{x};{row}M\x1b[<32;{x + 3};{row}M\x1b[<0;{x + 3};{row}m")
    wait_for("Copied")
    send(f"\x1b[<2;{x};{row}M\x1b[<2;{x};{row}m")
    wait_for("Search: Mean")
    send("\r")
    wait_for(f"Match 1/{total}")

    search("References")
    wait_for("The New S Language")
    assert "3" in model.sgr, "Missing italic font style in installed documentation"
    send("\x1bOP")
    wait_for("Viewer shortcuts")
    wait_for("Right-click")
    send("q")
    wait_for("Help: base::mean")
    resize(22, 52)
    wait_for("Help: base::mean")
    search("arithmetic mean")
    assert "Arithmetic Mean" in model.text(), model.text()
    assert b"\x08" not in raw[doc_start:], "Documentation emitted terminal overstrikes"
    close_viewer()

    resize(24, 110)
    expression(":objects")
    wait_for("Object explorer: Workspace")
    rows = model.text().splitlines()
    heading = next(row for row in rows if "Type / class" in row)
    alpha = next(row for row in rows if "alpha" in row and "integer" in row)
    assert heading.index("Name") == alpha.index("alpha"), (heading, alpha)
    assert heading.index("Type / class") == alpha.index("integer"), (heading, alpha)
    assert heading.index("Value") == alpha.index("12345"), (heading, alpha)
    close_viewer()

    expression(":view tom")
    wait_for("Object explorer: tom")
    wait_for("[Preview]")
    wait_for("café μ")
    send("\r\r\x1b[C")
    wait_for("no further children")
    assert "[[1]]" not in model.text(), model.text()
    assert model.text().splitlines()[0].strip() == "Object explorer: tom", model.text()
    close_viewer()

    expression(":quit")
    wait_for("DOCS_PTY_DONE")
    process.wait(timeout=10)
    assert process.returncode == 0, process.returncode
    assert termios.tcgetattr(slave) == original_modes
    assert model.enters == model.leaves == 1
    assert not model.bad_cursor, model.bad_cursor
    assert model.alternate_scrolls == 0, model.alternate_scrolls
    print("PASS: installed rich docs with bold/italic/syntax styles; all literal occurrences;")
    print("same-line next/previous jumps without unnecessary panning; wrapped phrase search;")
    print("mouse copy and right-click paste into search via session clipboard; F1 and resize;")
    print("Unicode leaf previews with finite drill-down; exact heading/data column alignment;")
    print("shared alternate screen, zero scrolling redraws, and exact terminal restoration.")
except BaseException:
    sys.stderr.write(model.text() + "\n")
    raise
finally:
    log = os.environ.get("PRISM_PTY_DOCS_LOG")
    if log:
        Path(log).write_bytes(raw)
    if process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
    os.close(master)
    os.close(slave)
