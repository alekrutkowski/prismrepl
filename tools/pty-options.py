"""Exercise option-list acceptance and cancellation in a real POSIX terminal.

Run after installing prismrepl; PRISM_R selects R. The test reuses the pure VT
model from pty-fullscreen.py and writes preferences only in a temporary folder.
It never opens an AI provider, browser, package repository, or network service.
"""
import ast
import codecs
import fcntl
import json
import os
from pathlib import Path
import pty
import re
import select
import signal
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

temporary = tempfile.TemporaryDirectory(prefix="prism-options-")
preferences = Path(temporary.name) / "preferences.dcf"
master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 120, 0, 0))
original_modes = terminal_snapshot(slave)
model = Screen(30, 120)
raw = bytearray()
phase = "startup"
environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor",
                   PRISMREPL_CONFIG_FILE=str(preferences))
environment.pop("NO_COLOR", None)
process = subprocess.Popen([os.environ.get("PRISM_R", "R"), "--vanilla", "--quiet", "--interactive"],
                           stdin=slave, stdout=slave, stderr=slave,
                           env=environment, close_fds=True)


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
        scrolls = model.alternate_scrolls
        model.feed(chunk)
        if model.alternate_scrolls != scrolls:
            sys.stderr.write(f"Unexpected alternate-screen scroll during {phase}: {chunk!r}\n")


def wait_until(check, description, timeout=20):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        collect(0.05)
        if check():
            return
        if process.poll() is not None:
            break
    raise AssertionError(f"Missing {description}\n{model.text()}")


def wait_for(marker):
    wait_until(lambda: marker in model.text(), repr(marker))


def wait_title(title):
    wait_until(lambda: model.text().splitlines()[0].lstrip().startswith(title),
               f"panel title {title!r}")


def wait_prompt(staged=""):
    def ready():
        line = "".join(model.cells[model.row]).strip()
        # A closing viewer repaints its parent before the selected command
        # applies or saves anything. The editor is ready only when native
        # input reopens, enabling bracketed paste after the viewer disabled it.
        input_open = raw.rfind(b"\x1b[?2004h") > raw.rfind(b"\x1b[?2004l")
        return (model.text().splitlines()[0].lstrip().startswith("PRISM REPL") and
                input_open and model.cursor_visible and line == ("r> " + staged).rstrip())
    wait_until(ready, f"editable prompt {staged!r}")


def focus(label):
    pattern = re.compile(r"^>\s+" + re.escape(label) + r"(?:\s|$)", re.MULTILINE)
    wait_until(lambda: pattern.search(model.text()) is not None, f"focused record {label!r}")


def send(text):
    os.write(master, text.encode("utf-8"))


def expression(text):
    global phase
    phase = text
    send("\x1b[200~" + text + "\x1b[201~")
    collect(0.06)
    send("\r")


def search(query):
    # The first Enter finishes search; accepting the highlighted record takes
    # a separate Enter. This is intentional and checked before application.
    send("/" + query + "\r")
    # Wait for the finished-search frame before a resize. Per-key frames can
    # remain queued on a busy host; interpreting an old frame at new dimensions
    # would report wrapping that did not belong to the resized renderer.
    wait_until(lambda: "Match " in model.text().splitlines()[-2] and
               "Search:" not in model.text().splitlines()[-1],
               f"finished search {query!r}")


def resize(rows, columns):
    global phase
    phase = f"resize to {rows} rows and {columns} columns"
    model.resize(rows, columns)
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
    os.kill(process.pid, signal.SIGWINCH)
    collect(0.15)


def saved(field):
    for line in preferences.read_text(encoding="utf-8").splitlines():
        name, separator, value = line.partition(":")
        if separator and name == field:
            return value.strip()
    return None


try:
    wait_for(">")
    path = json.dumps(str(preferences))
    bootstrap = (
        f'fixture <- new.env(parent=baseenv()); settings <- {path}; '
        'invisible(prismrepl::repl_settings(file=settings,theme="black",ai_provider="claude")); '
        'result <- prismrepl::repl(envir=fixture,config=prismrepl::repl_config('
        'ui="fullscreen",color=TRUE,color_depth=16777216L,settings_file=settings,'
        'history_file=NULL,persist=TRUE,mouse=TRUE,sidebar=FALSE,status=character(),'
        'hyperlinks=FALSE)); '
        'saved <- prismrepl::repl_settings(file=settings); '
        'stopifnot(identical(saved$ai_provider,"chatgpt"),identical(saved$pipe,"|>"),'
        'identical(saved$mouse,FALSE),identical(saved$rainbow,FALSE),'
        'identical(saved$keybindings$newline,character()),'
        '!":ai custom" %in% trimws(result$history)); '
        'cat("OPTIONS_PTY_DONE\\n");q("no")'
    )
    send(bootstrap + "\n")
    wait_prompt()

    # A current option is focused initially, and moving focus cannot save it.
    original_preferences = preferences.read_bytes()
    expression(":ai")
    wait_title("AI provider")
    focus("claude")
    send("\x1b[B")
    focus("perplexity")
    send("\x1b")
    wait_prompt()
    assert preferences.read_bytes() == original_preferences, "Cancel saved a focused AI provider"

    expression(":ai")
    wait_title("AI provider")
    search("ChatGPT")
    focus("chatgpt")
    assert preferences.read_bytes() == original_preferences, "Finishing search applied an option"
    send("\r")
    wait_prompt()
    assert saved("AI-Provider") == "chatgpt", preferences.read_text()
    wait_for("AI provider: ChatGPT (saved)")

    # An incomplete custom option returns editable text without saving a URL.
    before_custom = preferences.read_bytes()
    expression(":ai")
    wait_title("AI provider")
    search("Custom HTTPS")
    send("\r")
    wait_prompt(":ai custom")
    assert preferences.read_bytes() == before_custom, "Incomplete custom AI choice was saved"
    send("\x03")
    wait_prompt()

    expression(":pipe")
    wait_title("Pipe operator")
    send("\x1b[F\r")
    wait_prompt()
    assert saved("Pipe") == "%>%", preferences.read_text()

    expression(":mouse")
    wait_title("Mouse")
    send("\x1b[B\r")
    wait_prompt()
    assert saved("Mouse") == "false", preferences.read_text()

    expression(":rainbow")
    wait_title("Rainbow delimiters")
    send("\x1b[F\r")
    wait_prompt()
    assert saved("Rainbow") == "false", preferences.read_text()

    # Search can focus a description continuation. Reflow must still accept
    # that record's identifier, rather than its physical display-line number.
    expression(":bind")
    wait_title("Remappable keyboard actions")
    search("without running code")
    resize(8, 34)
    assert "newline" in model.text(), model.text()
    assert "Record 2/" in model.text(), model.text()
    send("\r")
    resize(30, 120)
    wait_prompt(":bind newline")
    assert saved("Key-newline") is None, "Staging a binding changed preferences"
    send("F4\r")
    wait_prompt()
    assert saved("Key-newline") == "F4", preferences.read_text()

    expression(":keys")
    wait_title("Keyboard shortcuts")
    search("Cycle completions backward")
    send("\r")
    wait_prompt(":bind complete_previous")
    before_cancel = preferences.read_bytes()
    send("\x03")
    wait_prompt()
    assert preferences.read_bytes() == before_cancel

    expression(":unbind")
    wait_title("Disable a shortcut action")
    search("without running code")
    send("\r")
    wait_prompt()
    assert saved("Key-newline") == "none", preferences.read_text()

    # Preferences rows open the actual option picker and keep one shared
    # alternate screen throughout the nested transition.
    expression(":settings")
    wait_title("Preferences")
    search("Pipe")
    send("\r")
    wait_title("Pipe operator")
    send("\x1b[H\r")
    wait_prompt()
    assert saved("Pipe") == "|>", preferences.read_text()
    assert model.enters == 1 and model.leaves == 0, (model.enters, model.leaves)

    expression(":status")
    wait_title("Status fields")
    send("\x1b[H \x1b[B\x1b[B \r")
    wait_prompt()
    wait_for("cwd:")
    wait_for("objects: 0")
    expression(":status")
    wait_title("Status fields")
    assert re.search(r"\[x\]\s+directory", model.text()), model.text()
    assert re.search(r"\[x\]\s+objects", model.text()), model.text()
    send("\x1b[H \x1b")
    wait_prompt()
    wait_for("cwd:")
    expression(":status")
    wait_title("Status fields")
    send("aa\r")
    wait_prompt()
    wait_for("Ready  |  local R process")

    expression(":quit")
    wait_for("OPTIONS_PTY_DONE")
    process.wait(timeout=10)
    assert process.returncode == 0, process.returncode
    assert terminal_snapshot(slave) == original_modes, "Input modes were not restored"
    assert model.enters == model.leaves == 1, (model.enters, model.leaves)
    assert model.alternate_scrolls == 0, model.alternate_scrolls
    assert not model.bad_cursor, model.bad_cursor
    assert model.cursor_color == "profile", model.cursor_color
    print("PASS: current-option focus, navigation/cancel, search plus explicit acceptance, saved AI/pipe/")
    print("mouse/editing choices, staged custom AI and bindings, tiny resize with description-to-record")
    print("mapping, shortcut disabling, nested settings selection, status multiselect/clear/cancel,")
    print("one alternate screen, no rolling redraw, cursor restoration, and exact terminal restoration.")
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
