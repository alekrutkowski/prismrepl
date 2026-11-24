#!/usr/bin/env python3
"""Exercise the installed native writer and mouse decoder through a real PTY."""
import fcntl
import os
from pathlib import Path
import pty
import select
import struct
import subprocess
import termios
import time


def main():
    root = Path(__file__).resolve().parents[2]
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))
    original = termios.tcgetattr(slave)
    process = subprocess.Popen(
        [os.environ.get("PRISM_R", "R"), "--vanilla", "--slave",
         "-f", str(root / "tools/native/pty-io.R")],
        stdin=slave, stdout=slave, stderr=slave,
        env={**os.environ, "TERM": "xterm-256color"},
    )
    output = bytearray()

    def expect(marker, seconds=15):
        deadline = time.monotonic() + seconds
        while marker not in output and time.monotonic() < deadline:
            if select.select([master], [], [], 0.05)[0]:
                try:
                    output.extend(os.read(master, 65536))
                except OSError:
                    break
            if process.poll() is not None:
                break
        if marker not in output:
            raise AssertionError(f"Missing {marker!r}: {bytes(output[-5000:])!r}")

    try:
        expect(b"READY_NATIVE_EVENTS")
        os.write(master, b"\x1b[<0;12;8M\x1b[<48;14;9M\x1b[<0;14;9m\x1b[<22;20;10M"
                 b"\x1b[<64;12;8M\x1b[<65;12;8M\n\r\x1b[200~" +
                 "\u03b1 <- 1\n\u03b1 + 1\n".encode("utf-8") + b"\x1b[201~")
        expect(b"READY_MOUSE_DISABLED")
        os.write(master, b"\x1b[<0;12;8Mx")
        expect(b"NATIVE_IO_OK")
        assert process.wait(timeout=5) == 0
        assert termios.tcgetattr(slave) == original, "Terminal input modes were not restored"
        assert b"\x1b[?1000h" in output and b"\x1b[?1000l" in output
        assert b"\x1b[?1002h" in output and b"\x1b[?1002l" in output
        assert b"\x1b[?1006h" in output and b"\x1b[?1006l" in output
        assert "NATIVE_FRAME_\u03b1\U0001f642".encode("utf-8") in output
        assert b"NATIVE_SINK_BYPASS" in output
        assert b"\x1b[?2026h\x1b]12;rgb:D4/E5/F6\x07\x1b[3;1HNATIVE_SYNCHRONIZED\x1b[?2026l" in output
        assert output.count(b"\x1b]12;rgb:A1/B2/C3\x07") == 1, "Unchanged cursor colors were written again"
        assert output.count(b"\x1b]12;rgb:D4/E5/F6\x07") == 1, "Input close reset a session-owned cursor"
        assert output.count(b"\x1b]12;rgb:01/02/03\x07") == 2, "Suspended cursor color was not reapplied"
        assert output.count(b"\x1b]112\x07") == 4, "Owned cursor resets were missing or duplicated"
        suspend = bytes(output).split(b"NATIVE_CURSOR_SUSPEND_BEGIN", 1)[1].split(b"NATIVE_CURSOR_SUSPEND_END", 1)[0]
        assert b"\x1b]112\x07" in suspend, "Screen suspension did not restore the cursor default"
        assert b"UNOWNED_CURSOR_BAD" not in output
        unowned = bytes(output).split(b"NATIVE_CURSOR_UNOWNED_RESET", 1)[1].split(b"NATIVE_CURSOR_FAILURE_BEGIN", 1)[0]
        assert b"\x1b]112\x07" not in unowned, "An untouched output guard reset the host cursor"
        failure = bytes(output).split(b"NATIVE_CURSOR_FAILURE_BEGIN", 1)[1]
        assert b"\x1b]112\x07" in failure, "Error cleanup did not restore the cursor default"
        timing = bytes(output).split(b"NATIVE_IO_OK", 1)[1].splitlines()[0].decode("ascii")
        print("PASS: buffered UTF-8 writes, sink bypass, synchronized framing, cached/owned cursor tint and restoration, mouse press/drag/release/wheel, "
              "modifiers, Ctrl+J, bracketed paste, opt-out, exact terminal restoration;" + timing)
    finally:
        if process.poll() is None:
            process.kill()
            process.wait(timeout=5)
        os.close(master)
        os.close(slave)


if __name__ == "__main__":
    main()
