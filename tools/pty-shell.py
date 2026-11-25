"""Exercise shell commands through real full-screen, inline and basic POSIX PTYs.

Run against an installed package, with PRISM_R selecting R and R_LIBS selecting
its library. Only Python's standard library is needed. Each child owns its
session and controlling terminal so Ctrl+C exercises actual terminal signals.
All files and shell redirection targets belong to temporary test directories.
"""
import ast
import codecs
import fcntl
import os
from pathlib import Path
import pty
import select
import shlex
import shutil
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


def controlling_terminal():
    """Give only the child process group ownership of its private terminal."""
    os.setsid()
    fcntl.ioctl(0, termios.TIOCSCTTY, 0)
    os.tcsetpgrp(0, os.getpgrp())


def exercise(layout):
    temporary = tempfile.TemporaryDirectory(prefix="prism shell pty ")
    output = Path(temporary.name) / "result with spaces.txt"
    literal = 'caf\u00e9  \u6f22\u5b57 \\ path "double" and single\'quote'
    pipeline = (":shell printf '%s\\n' " + shlex.quote(literal) + " | cat > " +
                shlex.quote(str(output)) +
                "; printf '%s%s\\n' 'SHELL_' 'LIVE_OK'   ")
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 120, 0, 0))
    original_modes = terminal_snapshot(slave)
    model = Screen(30, 120)
    raw = bytearray()
    environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor",
                       PRISM_SHELL_HISTORY_COMMAND=pipeline)
    environment.pop("NO_COLOR", None)
    executable = os.environ.get("PRISM_R", "R")
    executable = shutil.which(executable) or str(Path(executable).resolve())
    process = subprocess.Popen(
        [executable, "--vanilla", "--quiet", "--interactive"],
        stdin=slave, stdout=slave, stderr=slave, env=environment,
        cwd=temporary.name, close_fds=True, preexec_fn=controlling_terminal,
    )
    shell_calls = 0

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

    def wait_until(predicate, description, timeout=20):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            collect()
            if predicate():
                return
            if process.poll() is not None:
                break
        raise AssertionError(f"{layout}: missing {description}\n{model.text()}")

    def wait_for(marker):
        wait_until(lambda: marker in model.text(), repr(marker))

    def send(text):
        os.write(master, text.encode("utf-8"))

    def editor_ready(after=0):
        def ready():
            current = "".join(model.cells[model.row]).strip()
            if layout == "basic":
                return current == "r>" and len(raw) > after
            opened = raw.rfind(b"\x1b[?2004h")
            closed = raw.rfind(b"\x1b[?2004l")
            flags = termios.tcgetattr(slave)[3]
            return (opened >= after and opened > closed and model.cursor_visible and
                    not flags & (termios.ICANON | termios.ECHO) and current == "r>")
        wait_until(ready, "a fresh console input prompt")

    def submit(text):
        editor_ready()
        after = len(raw)
        if layout == "basic":
            send(text + "\n")
        else:
            send("\x1b[200~" + text + "\x1b[201~")
            collect(0.08)
            send("\r")
        return after

    def shell(text):
        nonlocal shell_calls
        shell_calls += 1
        return submit(text)

    def finish_shell(after, status=0, interrupt_pause=False):
        status_text = f"Shell exited with status {status}."
        wait_until(lambda: status_text in model.text() and
                   status_text.encode() in raw[after:], "the new shell exit status")
        if layout == "fullscreen":
            prompt = b"Press Enter to return to Prism (Ctrl+C also returns):"
            wait_until(lambda: prompt in raw[after:], "the new ordinary-terminal return prompt")
            assert not model.alternate, "Shell output/pause was painted in the alternate screen"
            assert model.enters == shell_calls, "The full-screen console resumed before approval"
            assert model.leaves == shell_calls, "The full-screen console was not suspended"
            # The main-screen output remains visible until an explicit key.
            collect(0.1)
            assert not model.alternate, "Shell output was hidden before approval"
            resumed = len(raw)
            send("\x03" if interrupt_pause else "\r")
            editor_ready(resumed)
            assert model.alternate and model.enters == shell_calls + 1
        else:
            editor_ready(after)
            assert "Press Enter to return to Prism" not in raw.decode("utf-8", "replace")
            assert model.enters == model.leaves == 0

    try:
        wait_for(">")
        ui = "inline" if layout == "basic" else layout
        mode = "basic" if layout == "basic" else "terminal"
        bootstrap = (
            'fixture <- new.env(parent=baseenv());'
            'result <- prismrepl::repl(envir=fixture,'
            f'mode="{mode}",config=prismrepl::repl_config(ui="{ui}",'
            'theme="black",color=TRUE,color_depth=16777216L,settings_file=NULL,'
            'history_file=NULL,persist=FALSE,sidebar=FALSE,status="process_memory",'
            'mouse=TRUE,hyperlinks=FALSE));'
            'stopifnot(identical(fixture$observed_exit,7L),'
            'Sys.getenv("PRISM_SHELL_HISTORY_COMMAND") %in% result$history,'
            '":sh exit 7" %in% result$history);'
            'cat("SHELL_PTY_DONE\\n");q("no")'
        )
        send(bootstrap + "\n")
        editor_ready()
        # This label is absent from the echoed bootstrap, so it must come from
        # the live status display in each layout, including the basic reader.
        wait_for("R RAM:")
        assert "R RAM: unavailable" not in model.text(), "Process memory was not measured"

        after = shell(pipeline)
        wait_for("SHELL_LIVE_OK")
        if layout == "fullscreen":
            assert not model.alternate, "Native shell stdout must use the ordinary terminal"
        assert output.read_bytes() == (literal + "\n").encode("utf-8"), output.read_bytes()
        finish_shell(after)

        after = shell(":sh exit 7")
        finish_shell(after, status=7)
        after = submit('observed_exit <- .Last.value;stopifnot(identical(observed_exit,7L));'
                       'cat(paste0("EXIT_","STATUS_OK\\n"))')
        wait_for("EXIT_STATUS_OK")
        editor_ready(after)

        after = shell(":sh printf '%s%s' 'CHILD_' 'INPUT> '; IFS= read -r prism_line; "
                      "printf '\\nINPUT_RESULT=<%s>\\n' \"$prism_line\"")
        wait_for("CHILD_INPUT>")
        assert not model.alternate, "Interactive shell input must own the ordinary terminal"
        flags = termios.tcgetattr(slave)[3]
        assert flags & termios.ICANON and flags & termios.ECHO, "Child stdin was left in raw editor mode"
        child_input = 'caf\u00e9  input\\path "quote"'
        send(child_input + "\n")
        wait_for("INPUT_RESULT=<" + child_input + ">")
        finish_shell(after)

        if layout == "fullscreen":
            after = shell(":sh printf '%s%s\\n' 'PAUSE_' 'INTERRUPT_OK'")
            wait_for("PAUSE_INTERRUPT_OK")
            finish_shell(after, interrupt_pause=True)

            after = shell(":sh printf '%s%s\\n' 'CHILD_' 'RUNNING'; exec sleep 30")
            wait_for("CHILD_RUNNING")
            assert not model.alternate
            interrupted = len(raw)
            send("\x03")
            wait_until(lambda: model.alternate or
                       b"Press Enter to return to Prism (Ctrl+C also returns):" in raw[interrupted:],
                       "the interrupted foreground child returning control", timeout=8)
            if not model.alternate:
                # system() can return the platform's signal exit status instead
                # of raising an R interrupt. Both paths must preserve the REPL.
                fresh = bytes(raw[interrupted:]).decode("utf-8", "replace")
                assert "Shell exited with status 0." not in fresh, "Interrupted child reported success"
                resumed = len(raw)
                send("\r")
                editor_ready(resumed)
            else:
                editor_ready(interrupted)
            assert model.enters == shell_calls + 1 and model.leaves == shell_calls

        after = submit('cat(paste0("UNICODE_","\\u03bb \\u6f22\\u5b57","\\n"))')
        wait_for("UNICODE_\u03bb \u6f22\u5b57")
        editor_ready(after)
        submit(":quit")
        wait_until(lambda: any(line.strip() == "SHELL_PTY_DONE" for line in model.text().splitlines()),
                   "the checked shell/history completion marker")
        assert process.wait(timeout=10) == 0
        assert terminal_snapshot(slave) == original_modes, "Shell commands leaked terminal input settings"
        expected = shell_calls + 1 if layout == "fullscreen" else 0
        assert model.enters == model.leaves == expected
        assert model.alternate_scrolls == 0, model.alternate_scrolls
        assert not model.bad_cursor, model.bad_cursor
        print(f"PASS ({layout}): literal Unicode, quotes, backslashes and spaces; native pipeline/redirection;")
        print("live stdout, exit status/.Last.value, child stdin, colon history, post-shell Unicode and exact terminal restoration.")
        if layout == "fullscreen":
            print("PASS: ordinary-screen pause, Enter and Ctrl+C return, foreground child Ctrl+C, no rolling redraw.")
    except BaseException:
        sys.stderr.write(model.text() + "\n")
        raise
    finally:
        log = os.environ.get("PRISM_PTY_SHELL_LOG")
        if log:
            Path(log + "." + layout + ".ansi").write_bytes(raw)
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(timeout=5)
        os.close(master)
        os.close(slave)
        temporary.cleanup()


for mode in ("fullscreen", "inline", "basic"):
    exercise(mode)
