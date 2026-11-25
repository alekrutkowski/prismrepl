"""Exercise cursor help with a live native input handle in both layouts.

Run after installing prismrepl; PRISM_R selects R and R_LIBS can select an
isolated installed library. Only the Python standard library is required.
The test covers F1, the complete searchable F2 shortcuts view, Unicode multiline
drafts, borrowed viewer ownership, cursor restoration, nano-style function
editing and exact terminal cleanup.
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


def exercise(mode):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 120, 0, 0))
    original_modes = terminal_snapshot(slave)
    model = Screen(30, 120)
    raw = bytearray()
    environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor")
    environment.pop("NO_COLOR", None)
    process = subprocess.Popen([os.environ.get("PRISM_R", "R"), "--vanilla", "--quiet", "--interactive"],
                               stdin=slave, stdout=slave, stderr=slave,
                               env=environment, close_fds=True)

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
        raise AssertionError(f"{mode}: missing {description}\n{model.text()}")

    def wait_for(marker):
        wait_until(lambda: marker in model.text(), repr(marker))

    def send(text):
        os.write(master, text.encode("utf-8"))

    def ready_editor(after=0):
        def ready():
            opened = raw.rfind(b"\x1b[?2004h")
            closed = raw.rfind(b"\x1b[?2004l")
            flags = termios.tcgetattr(slave)[3]
            current = "".join(model.cells[model.row]).strip()
            return (opened >= after and opened > closed and model.cursor_visible and
                    not flags & (termios.ICANON | termios.ECHO) and current == "r>")
        wait_until(ready, "a fresh native input prompt")

    def expression(text):
        ready_editor()
        after = len(raw)
        send("\x1b[200~" + text + "\x1b[201~")
        wait_for(text)
        send("\r")
        return after

    def help_roundtrip(expected_parent, expected_text, resize=False):
        cursor = (model.row, model.column)
        opened, closed = raw.count(b"\x1b[?2004h"), raw.count(b"\x1b[?2004l")
        entered, left = model.enters, model.leaves
        send("\x1bOP")
        wait_for("Help: mean")
        wait_for("Arithmetic Mean")
        assert raw.count(b"\x1b[?2004h") == opened, "Help reopened native input"
        assert raw.count(b"\x1b[?2004l") == closed, "Help closed native input"
        if resize:
            model.resize(30, 100)
            fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))
            os.kill(process.pid, signal.SIGWINCH)
            wait_for("Arithmetic Mean")
        send("q")
        wait_until(lambda: expected_parent in model.text() and expected_text in model.text()
                   and model.cursor_visible and (model.row, model.column) == cursor,
                   "the same draft and cursor after closing help")
        nested = expected_parent == "EDIT FUNCTION" or mode == "fullscreen"
        assert model.enters == entered + (0 if nested else 1)
        assert model.leaves == left + (0 if nested else 1)

    try:
        wait_for(">")
        bootstrap = (
            'fixture <- new.env(parent=baseenv());fixture$f <- function(x) base::mean(x);'
            'prismrepl::repl(envir=fixture,config=prismrepl::repl_config('
            f'ui="{mode}",theme="black",color=TRUE,color_depth=16777216L,'
            'settings_file=NULL,persist=FALSE,history_file=NULL,mouse=TRUE,'
            'sidebar=FALSE,status=character(),hyperlinks=FALSE));'
            'stopifnot(identical(get("cafe\\u0301",fixture),"\\u6f22\\u5b57"),'
            'identical(fixture$f(c(2,4)),3));'
            'cat("CONTEXT_HELP_DONE\\n");q("no")'
        )
        send(bootstrap + "\n")
        ready_editor()
        draft = '`cafe\u0301` <- "漢字"\nbase::mean(c(2, 4))'
        send("\x1b[200~" + draft + "\x1b[201~")
        wait_for("base::mean(c(2, 4))")
        prefix = draft[:draft.index("mean") + 2]
        graphemes = sum(not unicodedata.combining(char) for char in prefix)
        send("\x01" + "\x1b[C" * graphemes)
        expected_column = next(line.index("mean") + 2 for line in model.text().splitlines()
                               if "base::mean(c(2, 4))" in line)
        wait_until(lambda: model.column == expected_column, "the cursor on the function name")
        help_roundtrip("PRISM REPL" if mode == "fullscreen" else "r>", "base::mean(c(2, 4))", resize=True)
        if mode == "fullscreen":
            send("\x0c")
            wait_until(lambda: model.cursor_visible and "base::mean(c(2, 4))" in model.text(),
                       "the draft after a forced redraw")
        shortcut_cursor = (model.row, model.column)
        opened, closed = raw.count(b"\x1b[?2004h"), raw.count(b"\x1b[?2004l")
        entered, left = model.enters, model.leaves
        send("\x1bOQ")
        wait_for("Keyboard shortcuts")
        wait_for("Insert a newline without running code.")
        assert "More: :keys" not in model.text(), model.text()
        send("/pipe")
        wait_for("Search: pipe")
        wait_for("Insert the selected |> or %>% pipe.")
        send("\rG")
        wait_for("Scroll it down by one line.")
        model.resize(30, 90)
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 90, 0, 0))
        os.kill(process.pid, signal.SIGWINCH)
        wait_for("Scroll it down by one line.")
        model.resize(30, 100)
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))
        os.kill(process.pid, signal.SIGWINCH)
        wait_for("Keyboard shortcuts")
        send("\x1b")
        wait_until(lambda: "Keyboard shortcuts" not in model.text() and
                   "base::mean(c(2, 4))" in model.text() and model.cursor_visible and
                   (model.row, model.column) == shortcut_cursor,
                   "the exact draft and cursor after closing the shortcuts viewer")
        assert raw.count(b"\x1b[?2004h") == opened, "Shortcuts reopened native input"
        assert raw.count(b"\x1b[?2004l") == closed, "Shortcuts closed native input"
        assert model.enters == entered + (1 if mode == "inline" else 0)
        assert model.leaves == left + (1 if mode == "inline" else 0)
        after = len(raw)
        send("\r")
        ready_editor(after)

        expression(":edit f")
        wait_for("EDIT FUNCTION")
        wait_for("base::mean(x)")
        lines = model.text().splitlines()
        row = next(i for i, line in enumerate(lines) if "base::mean(x)" in line)
        column = lines[row].index("mean") + 2
        # Native mouse positioning also verifies that help follows the actual
        # cursor in the nano-style editor, rather than searching the whole text.
        send(f"\x1b[<0;{column + 1};{row + 1}M\x1b[<0;{column + 1};{row + 1}m")
        wait_until(lambda: (model.row, model.column) == (row, column), "the function editor caret")
        help_roundtrip("EDIT FUNCTION", "base::mean(x)")
        after = len(raw)
        send("\x18")
        ready_editor(after)
        expression(":quit")
        wait_until(lambda: any(line.strip() == "CONTEXT_HELP_DONE" for line in model.text().splitlines()),
                   "the completion marker after draft and function checks")
        process.wait(timeout=10)
        assert process.returncode == 0, process.returncode
        assert terminal_snapshot(slave) == original_modes
        assert model.enters == model.leaves == (1 if mode == "fullscreen" else 3)
        assert not model.bad_cursor, model.bad_cursor
        assert model.alternate_scrolls == 0, model.alternate_scrolls
        print(f"PASS ({mode}): F1 context docs, full F2 shortcuts with search/scroll/resize;")
        print("Unicode multiline draft and cursor restored exactly;")
        print("borrowed input handle, nested function-editor help, exact terminal restoration.")
    except BaseException:
        sys.stderr.write(model.text() + "\n")
        raise
    finally:
        log = os.environ.get("PRISM_PTY_CONTEXT_HELP_LOG")
        if log:
            Path(log + "." + mode + ".ansi").write_bytes(raw)
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
        os.close(master)
        os.close(slave)


for layout in ("fullscreen", "inline"):
    exercise(layout)
