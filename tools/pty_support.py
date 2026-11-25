"""Shared state snapshots for the POSIX integration harnesses."""
import fcntl
import struct
import sys
import termios


def terminal_snapshot(fd):
    """Return complete termios attributes after Darwin resolves pending input.

    XNU sets PENDIN when TCSANOW restores canonical input. FIONREAD resolves
    that transient state through ttnread()/ttypend(), without reading or
    flushing input. Query before both snapshots; never mask flags or replace
    the full equality check. Other operating systems retain direct snapshots.

    Primary source: apple-oss-distributions/xnu, bsd/kern/tty.c, FIONREAD,
    ttioctl_locked, ttnread, and ttypend; bsd/sys/termios.h defines PENDIN.
    https://github.com/apple-oss-distributions/xnu/blob/main/bsd/kern/tty.c
    """
    if sys.platform == "darwin":
        fcntl.ioctl(fd, termios.FIONREAD, struct.pack("i", 0))
    return termios.tcgetattr(fd)
