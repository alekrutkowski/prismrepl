"""Exercise the installed package in a real POSIX pseudo-terminal.

No third-party Python modules are required. Run after R CMD INSTALL .
Set PRISM_R to a specific R executable when testing a local R installation.
"""
import fcntl
import os
import pathlib
import pty
import re
import select
import signal
import struct
import subprocess
import sys
import termios
import time

if os.name != 'posix':
    raise SystemExit('This smoke test requires POSIX PTYs; use the Windows checklist.')

r = os.environ.get('PRISM_R', 'R')
master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 30, 100, 0, 0))
before = termios.tcgetattr(slave)
code = ('prismrepl::repl(config=prismrepl::repl_config(ui="inline",color=FALSE,status=character(),'
        'confirm=FALSE,link_context=FALSE,hyperlinks=FALSE));cat("PRISM_DONE\\n");q("no")')
env = dict(os.environ, TERM='xterm-256color', NO_COLOR='1')
proc = subprocess.Popen([r, '--vanilla', '--quiet', '--interactive'],
                        stdin=slave, stdout=slave, stderr=slave, env=env, close_fds=True)
raw = bytearray()
ansi = re.compile(rb'\x1b\][^\x07]*(?:\x07|\x1b\\)|\x1b\[[0-?]*[ -/]*[@-~]')

def plain():
    return ansi.sub(b'', bytes(raw)).replace(b'\r', b'').decode('utf-8', 'replace')

def collect(seconds=0.3):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        ready, _, _ = select.select([master], [], [], max(0, deadline-time.monotonic()))
        if not ready:
            break
        try:
            chunk = os.read(master, 65536)
        except OSError:
            break
        if not chunk:
            break
        raw.extend(chunk)

def wait_for(marker, timeout=20):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        collect(0.1)
        if marker in plain():
            return
        if proc.poll() is not None:
            break
    raise AssertionError(f'Missing output {marker!r}\n{plain()[-12000:]}')

def send(text):
    os.write(master, text.encode('utf-8') if isinstance(text, str) else text)

def expression(text):
    # Bracketed paste must not submit, even when text contains newlines.
    send('\x1b[200~' + text + '\x1b[201~')
    collect(0.1)
    send('\r')

try:
    wait_for('> ')
    send(code + '\n')
    wait_for('r> ')
    expression('x <- 41; cat("EVAL_OK=", x + 1, "\\n", sep="")')
    wait_for('EVAL_OK=42')
    expression('caf\u00e9 <- "\u6f22\u5b57 \U0001f9ea"; cat("UNICODE_OK=", caf\u00e9, "\\n", sep="")')
    wait_for('UNICODE_OK=\u6f22\u5b57 \U0001f9ea')
    expression('f <- function(x) {\n  x + 1\n}; cat("MULTI_OK=", f(2), "\\n", sep="")')
    wait_for('MULTI_OK=3')
    expression('warning("SMOKE_WARNING"); stop("SMOKE_ERROR")')
    wait_for('[WARNING]')
    wait_for('SMOKE_WARNING')
    wait_for('[ERROR]')
    wait_for('SMOKE_ERROR')
    wait_for('https://chat.mistral.ai/chat?q=')
    # Ctrl+C cancels a pending expression and leaves the REPL usable.
    send('this_should_not_run(')
    send('\x03')
    collect(0.1)
    expression('cat("CANCEL_OK\\n")')
    wait_for('CANCEL_OK\n')
    # A combining mark is removed with its base grapheme by one Backspace.
    send('e\u0301')
    send('\x7f')
    send('cat("GRAPHEME_OK\\n")\r')
    wait_for('GRAPHEME_OK\n')
    # Pipe insertion via portable Alt+M.
    send('1:3')
    send('\x1bm')
    send('sum()\r')
    collect(0.3)
    assert '[1] 6' in plain(), plain()[-4000:]
    # Resize while the editor is active.
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 18, 55, 0, 0))
    os.kill(proc.pid, signal.SIGWINCH)
    expression('cat("RESIZE_OK\\n")')
    wait_for('RESIZE_OK\n')
    # Read-only panel, search, then return to the same session.
    send(':keys\r')
    wait_for('Keyboard shortcuts')
    send('/pipe\r')
    collect(0.1)
    send('q')
    collect(0.1)
    send(':quit\r')
    wait_for('\nPRISM_DONE\n')
    proc.wait(timeout=10)
    assert proc.returncode == 0, proc.returncode
    after = termios.tcgetattr(slave)
    assert before == after, (before, after)
    print('PTY smoke passed: evaluation, Unicode, multiline paste, conditions, cancel,')
    print('graphemes, pipe insertion, resize, pager, exit, and exact terminal restoration.')
except BaseException:
    sys.stderr.write(plain()[-16000:] + '\n')
    raise
finally:
    if proc.poll() is None:
        proc.terminate()
        try:
            proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait()
    os.close(master)
    os.close(slave)
