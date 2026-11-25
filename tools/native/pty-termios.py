"""Check complete restoration snapshots and preservation of queued input.

No R installation is required. Run on both Linux and macOS. The real PTY test
reproduces Prism's raw-to-canonical transition and checks all termios fields.
"""
import copy
import fcntl
import os
from pathlib import Path
import pty
import select
import struct
import sys
import termios
import time
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from pty_support import terminal_snapshot


def available(fd):
    return struct.unpack("i", fcntl.ioctl(fd, termios.FIONREAD, struct.pack("i", 0)))[0]


def read_exact(fd, expected, timeout=2):
    received = bytearray()
    deadline = time.monotonic() + timeout
    while len(received) < len(expected) and time.monotonic() < deadline:
        if select.select([fd], [], [], max(0, deadline - time.monotonic()))[0]:
            received.extend(os.read(fd, 1024))
    assert received == expected, (bytes(received), expected)


# Exercise the Darwin dispatch even on Linux. The ioctl must precede the
# snapshot, and the helper must return the entire result without masking it.
sentinel = [1, 2, 3, 0x20000000, 9600, 9600, [b"\x03"]]
calls = mock.Mock()
with mock.patch("pty_support.sys.platform", "darwin"), \
     mock.patch("pty_support.fcntl.ioctl", return_value=struct.pack("i", 0)) as query, \
     mock.patch("pty_support.termios.tcgetattr", return_value=sentinel) as attributes, \
     mock.patch("os.read") as read, mock.patch("termios.tcflush") as flush, \
     mock.patch("termios.tcsetattr") as change:
    calls.attach_mock(query, "query")
    calls.attach_mock(attributes, "attributes")
    assert terminal_snapshot(123) is sentinel
    assert calls.mock_calls == [mock.call.query(123, termios.FIONREAD, struct.pack("i", 0)),
                                mock.call.attributes(123)]
    read.assert_not_called()
    flush.assert_not_called()
    change.assert_not_called()

for queued in (b"", b"prism_typeahead"):
    master, slave = pty.openpty()
    original = terminal_snapshot(slave)
    try:
        assert original[3] & termios.ICANON
        raw = copy.deepcopy(original)
        raw[0] &= ~(termios.BRKINT | termios.ICRNL | termios.INPCK |
                    termios.ISTRIP | termios.IXON)
        raw[2] |= termios.CS8
        raw[3] &= ~(termios.ECHO | termios.ICANON | termios.IEXTEN | termios.ISIG)
        raw[6][termios.VMIN] = raw[6][termios.VTIME] = 0
        termios.tcsetattr(slave, termios.TCSANOW, raw)
        if queued:
            os.write(master, queued)
            deadline = time.monotonic() + 2
            while available(slave) != len(queued) and time.monotonic() < deadline:
                time.sleep(0.005)
            assert available(slave) == len(queued), "Typeahead did not reach the raw input queue"
        termios.tcsetattr(slave, termios.TCSANOW, original)
        immediate = termios.tcgetattr(slave)
        settled = terminal_snapshot(slave)
        assert settled == original, (original, immediate, settled)
        if queued:
            # Linux and Darwin differ in how the incomplete line is exposed
            # during the transition. Both must retain every byte after FIONREAD.
            os.write(master, b"\n")
            read_exact(slave, queued + b"\n")
        assert terminal_snapshot(slave) == original

        # A genuine changed mode must still make the exact equality gate fail.
        wrong = copy.deepcopy(original)
        wrong[3] ^= termios.ECHO
        termios.tcsetattr(slave, termios.TCSANOW, wrong)
        changed = terminal_snapshot(slave)
        assert changed != original, "The snapshot concealed a changed echo mode"
        assert bool(changed[3] & termios.ECHO) != bool(original[3] & termios.ECHO)
        termios.tcsetattr(slave, termios.TCSANOW, original)
        assert terminal_snapshot(slave) == original
        print(f"{sys.platform}: queued={len(queued)}, immediate lflag={immediate[3]:#x}, "
              f"original={original[3]:#x}, settled={settled[3]:#x}")
    finally:
        termios.tcsetattr(slave, termios.TCSANOW, original)
        os.close(master)
        os.close(slave)

print("PASS: non-consuming Darwin query order, complete attribute equality, queued input preserved,")
print("and a real echo-mode mismatch remains detectable; no flags masked or input flushed")
