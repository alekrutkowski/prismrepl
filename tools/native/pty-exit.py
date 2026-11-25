"""Check real R process exit from the installed fullscreen console on POSIX.

Run after installing prismrepl. PRISM_R selects an R executable. This test
specifically exercises exit finalizers, because q() bypasses ordinary on.exit
handlers in the REPL call stack. It does not test forced process termination.
"""
import fcntl
import os
from pathlib import Path
import pty
import re
import select
import struct
import subprocess
import sys
import termios
import time

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from pty_support import terminal_snapshot

if os.name != "posix":
    raise SystemExit("This test requires a POSIX pseudo-terminal.")

master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))
before = terminal_snapshot(slave)
process = subprocess.Popen(
    [os.environ.get("PRISM_R", "R"), "--vanilla", "--quiet", "--interactive"],
    stdin=slave, stdout=slave, stderr=slave,
    env=dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor", NO_COLOR=""), close_fds=True,
)
raw = bytearray()
ansi = re.compile(rb"\x1b\[[0-?]*[ -/]*[@-~]")


def collect(seconds=0.1):
    until = time.monotonic() + seconds
    while time.monotonic() < until:
        if not select.select([master], [], [], max(0, until - time.monotonic()))[0]:
            return
        try:
            value = os.read(master, 65536)
        except OSError:
            return
        if not value:
            return
        raw.extend(value)


def wait_for(marker, timeout=30):
    until = time.monotonic() + timeout
    while time.monotonic() < until:
        collect()
        if marker in ansi.sub(b"", raw):
            return
        if process.poll() is not None:
            break
    raise AssertionError("Missing marker %r in %r" % (marker, raw[-5000:]))


try:
    wait_for(b"> ")
    os.write(master, (
        "prismrepl::repl(config=prismrepl::repl_config(ui='fullscreen',"
        "theme='black',color=TRUE,history_file=NULL,settings_file=NULL,mouse=TRUE,"
        "status=character(),confirm=FALSE))\n"
    ).encode())
    wait_for(b"r> ")
    wait_for(b"\x1b]12;rgb:FF/FF/FF\x07")
    os.write(master, b"q('no')\r")
    until = time.monotonic() + 30
    while process.poll() is None and time.monotonic() < until:
        collect()
    assert process.wait(timeout=5) == 0, raw[-5000:]
    collect(0.2)
    assert raw.rfind(b"\x1b[?1049h") >= 0, "Fullscreen was not entered."
    assert raw.rfind(b"\x1b[?1049l") > raw.rfind(b"\x1b[?1049h"), (
        "R exited without restoring the main screen: %r" % raw[-5000:]
    )
    assert terminal_snapshot(slave) == before, "R exited with changed input modes."
    assert raw.rfind(b"\x1b]112\x07") > raw.rfind(b"\x1b]12;rgb:FF/FF/FF\x07"), (
        "R exited without restoring the default cursor color."
    )
    assert raw.rfind(b"\x1b[?1000l") > raw.rfind(b"\x1b[?1000h") >= 0, (
        "R exited without disabling mouse button reporting."
    )
    assert raw.rfind(b"\x1b[?1006l") > raw.rfind(b"\x1b[?1006h") >= 0, (
        "R exited without disabling SGR mouse coordinates."
    )
    assert raw.rfind(b"\x1b[?1002l") > raw.rfind(b"\x1b[?1002h") >= 0, (
        "R exited without disabling drag reporting."
    )
    print("PASS: q() exits R, restores the main screen and cursor color, disables mouse reporting, and restores all terminal input settings")
finally:
    if process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=5)
    termios.tcsetattr(slave, termios.TCSANOW, before)
    os.close(master)
    os.close(slave)
