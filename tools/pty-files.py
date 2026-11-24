"""Exercise the FILES sidebar, file browser and mouse clipboard through a PTY.

Run against the installed package; PRISM_R selects R. Temporary scripts and a
local fake Wayland clipboard keep all test effects inside a disposable fixture.
The terminal model is reused without executing the fullscreen test itself.
"""
import ast
import codecs
import fcntl
import json
import os
from pathlib import Path
import pty
import select
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

temporary = tempfile.TemporaryDirectory(prefix="prism-files-")
root = Path(temporary.name).resolve()
folder = root / "folder with spaces"
folder.mkdir()
script = folder / "my file.R"
sentinel = root / "source-sentinel.txt"
script.write_text(
    "source_value <- 42L\n"
    f'writeLines("approved", {json.dumps(str(sentinel))})\n'
    'cat(paste0("FILE_", "SOURCED\\n"))\n', encoding="utf-8")
(root / "a root.R").write_text('stop("ROOT_SCRIPT_MUST_NOT_EXECUTE")\n', encoding="utf-8")
(root / "notes.txt").write_text("Read-only notes\n", encoding="utf-8")

clipboard = root / ".clipboard-text"
helpers = root / ".clipboard-bin"
helpers.mkdir()
helper = (
    "#!/usr/bin/env python3\n"
    "import os, sys\n"
    "from pathlib import Path\n"
    'path = Path(os.environ["PRISM_TEST_CLIPBOARD_FILE"])\n'
    'if Path(sys.argv[0]).name == "wl-copy":\n'
    "    path.write_bytes(sys.stdin.buffer.read())\n"
    "else:\n"
    '    sys.stdout.buffer.write(path.read_bytes() if path.exists() else b"")\n'
)
for name in ("wl-copy", "wl-paste"):
    target = helpers / name
    target.write_text(helper, encoding="utf-8")
    target.chmod(0o755)

master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 34, 132, 0, 0))
original_modes = termios.tcgetattr(slave)
model = Screen(34, 132)
raw = bytearray()
environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor",
                   WAYLAND_DISPLAY="prism-test-clipboard", DISPLAY="",
                   PRISM_TEST_CLIPBOARD_FILE=str(clipboard),
                   PATH=str(helpers) + os.pathsep + os.environ.get("PATH", ""))
environment.pop("NO_COLOR", None)
process = subprocess.Popen([os.environ.get("PRISM_R", "R"), "--vanilla", "--quiet", "--interactive"],
                           cwd=root, stdin=slave, stdout=slave, stderr=slave,
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
        model.feed(chunk)


def wait_until(predicate, description, timeout=20):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        collect(0.04)
        if predicate():
            return
        if process.poll() is not None:
            break
    raise AssertionError(f"Missing {description}\n{model.text()}")


def wait_for(marker):
    wait_until(lambda: marker in model.text(), repr(marker))


def send(text):
    os.write(master, text.encode("utf-8"))


def expression(text):
    send("\x1b[200~" + text + "\x1b[201~")
    collect(0.06)
    send("\r")


def click(x, y):
    send(f"\x1b[<0;{x};{y}M\x1b[<0;{x};{y}m")


def cancel():
    send("\x03")
    collect(0.08)


def sidebar_click(label):
    matches = [(row, line.find(label)) for row, line in enumerate(model.text().splitlines())
               if line.find(label) > model.columns // 2]
    assert matches, (label, model.text())
    row, column = matches[0]
    click(column + 2, row + 1)


def wait_files():
    wait_until(lambda: model.text().splitlines()[0].strip() == "Files", "the file browser")
    assert model.enters == 1 and model.leaves == 0


try:
    wait_for(">")
    bootstrap = (
        'fixture <- new.env(parent=baseenv()); '
        'invisible(lapply(seq_len(80L),function(i)assign(sprintf("object%03d",i),i,envir=fixture))); '
        'prismrepl::repl(envir=fixture,config=prismrepl::repl_config(ui="fullscreen",'
        'color=TRUE,color_depth=16777216L,settings_file=NULL,persist=FALSE,mouse=TRUE,'
        'sidebar=TRUE,status=c("directory","objects","packages"),hyperlinks=FALSE));'
        'cat("FILES_PTY_DONE\\n");q("no")'
    )
    send(bootstrap + "\n")
    wait_for("PRISM REPL")
    wait_for("FILES")
    assert all(section in model.text() for section in ("WORKSPACE", "PACKAGES", "FILES")), model.text()
    assert "folder with spaces/" in model.text(), model.text()

    # Sidebar short clicks only stage an editable command, even with 80 objects.
    sidebar_click("folder with spaces/")
    wait_for("r> :cd ")
    assert "folder with spaces" in model.text()
    cancel()
    expression(f'stopifnot(identical(getwd(),{json.dumps(str(root))}));cat(paste0("SIDEBAR_","CWD_SAFE\\n"))')
    wait_for("SIDEBAR_CWD_SAFE")
    sidebar_click("a root.R")
    wait_for("r> :files ")
    assert "a root.R" in model.text()
    assert "ROOT_SCRIPT_MUST_NOT_EXECUTE" not in model.text()
    cancel()

    expression(":files")
    wait_files()
    send("/folder with spaces\r\x1b[B\r")
    wait_until(lambda: str(folder) in model.text().splitlines()[1], "child directory breadcrumb")
    assert not sentinel.exists()

    # Clicking the blank table area must not open its final file.
    click(7, 9)
    collect(0.06)
    click(7, 9)
    collect(0.1)
    assert "Read-only preview" not in model.text(), model.text()
    assert str(folder) in model.text().splitlines()[1], model.text()

    # Right-click clipboard input filters names; it is not a one-letter action.
    clipboard.write_text("my file", encoding="utf-8")
    send("\x1b[<2;9;6M\x1b[<2;9;6m")
    wait_for("Filter: my file")
    send("\r\x1b[B\r")
    wait_for("Read-only preview")
    wait_for("source_value <- 42L")
    assert not sentinel.exists(), "Preview executed the R script"
    send("\x1b")
    wait_until(lambda: "Read-only preview" not in model.text(), "directory after preview")
    send("c")
    wait_for("r> :cd ")
    cancel()
    expression(f'stopifnot(identical(getwd(),{json.dumps(str(root))}));cat(paste0("FILES_","CWD_STAGED_ONLY\\n"))')
    wait_for("FILES_CWD_STAGED_ONLY")

    # Approve the staged directory explicitly, then inspect the script again.
    expression(":files " + str(folder))
    wait_files()
    send("c")
    wait_for("r> :cd ")
    send("\r")
    collect(0.1)
    expression(f'stopifnot(identical(getwd(),{json.dumps(str(folder))}));cat(paste0("CWD_","APPROVED\\n"))')
    wait_for("CWD_APPROVED")
    expression(":files " + str(script))
    wait_files()
    wait_for("Read-only preview")
    send("s")
    wait_for("r> :source ")
    assert not sentinel.exists(), "Staging :source executed the script"
    cancel()
    expression(f'stopifnot(!file.exists({json.dumps(str(sentinel))}));cat(paste0("SOURCE_","CANCELLED\\n"))')
    wait_for("SOURCE_CANCELLED")
    expression(":files " + str(script))
    wait_files()
    send("s")
    wait_for("r> :source ")
    assert not sentinel.exists()
    send("\r")
    wait_until(sentinel.exists, "the explicitly approved source side effect")
    wait_for("FILE_SOURCED")
    assert sentinel.read_text(encoding="utf-8").strip() == "approved"

    # Select two columns from real transcript rows and paste into the editor.
    expression('cat(paste0("COPY_","A 12345\\nCOPY_","B 67890\\n"))')
    wait_for("COPY_A 12345")
    wait_for("COPY_B 67890")
    lines = model.text().splitlines()
    a = next((i, line.index("12345")) for i, line in enumerate(lines) if line.strip().startswith("COPY_A 12345"))
    b = next((i, line.index("67890")) for i, line in enumerate(lines) if line.strip().startswith("COPY_B 67890"))
    assert a[1] == b[1], (a, b)
    send(f"\x1b[<16;{a[1] + 1};{a[0] + 1}M"
         f"\x1b[<48;{b[1] + 5};{b[0] + 1}M"
         f"\x1b[<16;{b[1] + 5};{b[0] + 1}m")
    wait_until(lambda: clipboard.exists() and clipboard.read_bytes() == b"12345\n67890",
               "rectangular copy from transcript through the clipboard provider")
    send("\x1b[<2;5;30M\x1b[<2;5;30m")
    wait_for("r> 12345")
    wait_for("67890")
    assert "EDIT" in model.text().splitlines()[0], model.text()
    cancel()
    expression('stopifnot(source_value == 42L);cat(paste0("CLIPBOARD_","EDITABLE\\n"))')
    wait_for("CLIPBOARD_EDITABLE")

    expression(":quit")
    wait_for("FILES_PTY_DONE")
    process.wait(timeout=10)
    assert process.returncode == 0, process.returncode
    assert termios.tcgetattr(slave) == original_modes
    assert model.enters == model.leaves == 1
    assert model.alternate_scrolls == 0
    assert not model.bad_cursor, model.bad_cursor
    print("PASS: crowded-workspace FILES sidebar, staged sidebar clicks, directory navigation,")
    print("blank-row hit testing, right-paste filtering, read-only R preview, spaced paths,")
    print("cancelled/approved cd and source, Ctrl-drag rectangular copy, editable right-paste,")
    print("one alternate screen, no rolling redraw and exact terminal restoration.")
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
