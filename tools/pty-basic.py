"""Verify plain-console multiline input, blank lines, and Ctrl+D with a PTY."""
import os
import pty
import select
import subprocess
import termios
import time

master, slave = pty.openpty()
before = termios.tcgetattr(slave)
proc = subprocess.Popen([os.environ.get('PRISM_R', 'R'), '--vanilla', '--quiet', '--interactive'],
                        stdin=slave, stdout=slave, stderr=slave,
                        env=dict(os.environ, TERM='dumb', NO_COLOR='1'), close_fds=True)
output = bytearray()

def wait_for(marker):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        ready, _, _ = select.select([master], [], [], 0.1)
        if ready:
            try:
                output.extend(os.read(master, 65536))
            except OSError:
                break
        if marker in output.decode('utf-8', 'replace').replace('\r', ''):
            return
    raise AssertionError(output.decode('utf-8', 'replace'))

def send(text):
    os.write(master, text.encode())

try:
    wait_for('> ')
    send('prismrepl::repl(mode="basic",config=prismrepl::repl_config(color=FALSE,status=character()));cat("BASIC_DONE\\n");q("no")\n')
    wait_for('r> ')
    send('\n')
    send('cat("BLANK_OK\\n")\n')
    wait_for('\nBLANK_OK\n')
    send('f <- function(x) {\n')
    wait_for('+  ')
    send('x + 2\n}\n')
    send('cat("BASIC_MULTI=",f(3),"\\n",sep="")\n')
    wait_for('\nBASIC_MULTI=5\nr> ')
    # The basic prompt is flushed before readLines enters the host editor.
    # Sending a synthetic EOF during that transition can lose the EOF event.
    # Allow readline to prepare; builds without readline stay canonical.
    ready_deadline = time.monotonic() + 1
    while (termios.tcgetattr(slave)[3] & (termios.ICANON | termios.ECHO) and
           time.monotonic() < ready_deadline):
        time.sleep(0.01)
    time.sleep(0.1)
    send('\x04')
    wait_for('BASIC_DONE\n')
    proc.wait(timeout=5)
    assert proc.returncode == 0
    assert termios.tcgetattr(slave) == before
    print('Basic PTY passed: blank line, multiline input, Ctrl+D, terminal restoration.')
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
