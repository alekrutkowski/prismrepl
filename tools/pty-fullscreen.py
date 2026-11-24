"""Exercise prismrepl's real fullscreen renderer in a POSIX pseudo-terminal.

Run after installing the package. PRISM_R selects the R executable. Only the
Python standard library is used. A small VT model checks the actual screen
cells after redraws, scrolling, resizing, and modal-viewer transitions.
"""
import codecs
import fcntl
import os
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
import statistics


class Screen:
    """The cursor, erase, alternate-buffer, and text operations this UI emits."""

    def __init__(self, rows, columns):
        self.rows, self.columns = rows, columns
        self.cells = [[" "] * columns for _ in range(rows)]
        self.row = self.column = 0
        self.saved = None
        self.alternate = False
        self.enters = self.leaves = 0
        self.pending = ""
        self.decoder = codecs.getincrementaldecoder("utf-8")("replace")
        self.sgr = set()
        self.links = []
        self.cursor_color = "profile"
        self.cursor_visible = True
        self.cursor_colors = []
        self.bad_cursor = []
        self.alternate_scrolls = 0

    def resize(self, rows, columns):
        self.rows, self.columns = rows, columns
        self.cells = [[" "] * columns for _ in range(rows)]
        self.row = min(self.row, rows - 1)
        self.column = min(self.column, columns - 1)

    def text(self):
        return "\n".join("".join(row).rstrip() for row in self.cells)

    def newline(self):
        self.row += 1
        if self.row >= self.rows:
            if self.alternate:
                self.alternate_scrolls += 1
            self.cells.pop(0)
            self.cells.append([" "] * self.columns)
            self.row = self.rows - 1

    def csi(self, parameters, final):
        private = parameters.startswith("?")
        body = parameters[1:] if private else parameters
        values = [int(value) if value.isdigit() else 0 for value in body.split(";")]
        first = values[0] if values else 0
        if private and body == "1049" and final == "h":
            self.enters += 1
            self.saved = ([row[:] for row in self.cells], self.row, self.column)
            self.cells = [[" "] * self.columns for _ in range(self.rows)]
            self.row = self.column = 0
            self.alternate = True
        elif private and body == "1049" and final == "l":
            self.leaves += 1
            if self.saved:
                saved, self.row, self.column = self.saved
                self.cells = [(row + [" "] * self.columns)[:self.columns]
                              for row in saved[:self.rows]]
                self.cells += [[" "] * self.columns for _ in range(self.rows - len(self.cells))]
            self.alternate = False
        elif private and body == "25" and final in ("h", "l"):
            self.cursor_visible = final == "h"
        elif final in ("H", "f"):
            row = (first or 1) - 1
            column = ((values[1] if len(values) > 1 else 1) or 1) - 1
            if self.alternate and not (0 <= row < self.rows and 0 <= column < self.columns):
                self.bad_cursor.append((row, column, self.rows, self.columns))
            self.row = max(0, min(self.rows - 1, row))
            self.column = max(0, min(self.columns - 1, column))
        elif final == "A":
            self.row = max(0, self.row - (first or 1))
        elif final == "B":
            self.row = min(self.rows - 1, self.row + (first or 1))
        elif final == "C":
            self.column = min(self.columns - 1, self.column + (first or 1))
        elif final == "D":
            self.column = max(0, self.column - (first or 1))
        elif final == "G":
            self.column = max(0, min(self.columns - 1, (first or 1) - 1))
        elif final == "J":
            if first == 2:
                self.cells = [[" "] * self.columns for _ in range(self.rows)]
            elif first == 0:
                self.cells[self.row][self.column:] = [" "] * (self.columns - self.column)
                for row in range(self.row + 1, self.rows):
                    self.cells[row] = [" "] * self.columns
        elif final == "K":
            start = 0 if first == 2 else self.column
            self.cells[self.row][start:] = [" "] * (self.columns - start)
        elif final == "m":
            self.sgr.add(parameters)

    def feed(self, data):
        self.pending += self.decoder.decode(data)
        index = 0
        while index < len(self.pending):
            char = self.pending[index]
            if char == "\x1b":
                if index + 1 >= len(self.pending):
                    break
                kind = self.pending[index + 1]
                if kind == "[":
                    end = index + 2
                    while end < len(self.pending) and not ("@" <= self.pending[end] <= "~"):
                        end += 1
                    if end == len(self.pending):
                        break
                    self.csi(self.pending[index + 2:end], self.pending[end])
                    index = end + 1
                    continue
                if kind == "]":
                    bel = self.pending.find("\x07", index + 2)
                    st = self.pending.find("\x1b\\", index + 2)
                    ends = [(bel, 1), (st, 2)]
                    ends = [item for item in ends if item[0] >= 0]
                    if not ends:
                        break
                    end, size = min(ends)
                    payload = self.pending[index + 2:end]
                    if payload.startswith("8;;https://"):
                        self.links.append(payload[3:])
                    elif payload.startswith("12;"):
                        color = payload[3:]
                        if color.startswith("rgb:"):
                            parts = color[4:].split("/")
                            color = "#" + "".join(f"{round(int(part, 16) * 255 / (16 ** len(part) - 1)):02X}"
                                                   for part in parts)
                        self.cursor_color = color.upper()
                        self.cursor_colors.append(self.cursor_color)
                    elif payload == "112":
                        self.cursor_color = "profile"
                        self.cursor_colors.append("profile")
                    index = end + size
                    continue
                index += 2
                continue
            if char == "\r":
                self.column = 0
            elif char == "\n":
                self.newline()
            elif char == "\b":
                self.column = max(0, self.column - 1)
            elif char == "\t":
                self.column = min(self.columns - 1, (self.column // 8 + 1) * 8)
            elif ord(char) >= 32 and char != "\x7f":
                if unicodedata.combining(char):
                    if self.column:
                        self.cells[self.row][self.column - 1] += char
                else:
                    width = 2 if unicodedata.east_asian_width(char) in ("W", "F") else 1
                    if self.column >= self.columns:
                        self.column = 0
                        self.newline()
                    self.cells[self.row][self.column] = char
                    if width == 2 and self.column + 1 < self.columns:
                        self.cells[self.row][self.column + 1] = ""
                    self.column += width
            index += 1
        self.pending = self.pending[index:]


if os.name != "posix":
    raise SystemExit("This integration test requires a POSIX pseudo-terminal.")

r = os.environ.get("PRISM_R", "R")
master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 32, 132, 0, 0))
original_modes = termios.tcgetattr(slave)
model = Screen(32, 132)
raw = bytearray()
environment = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor")
environment.pop("NO_COLOR", None)
environment.update(XDG_CURRENT_DESKTOP="", DESKTOP_SESSION="", GDMSESSION="",
                   GTK_THEME="", COLORFGBG="0;15")
preferences = tempfile.TemporaryDirectory(prefix="prismrepl-fullscreen-")
environment["PRISMREPL_CONFIG_FILE"] = os.path.join(preferences.name, "preferences.dcf")
process = subprocess.Popen([r, "--vanilla", "--quiet", "--interactive"],
                           stdin=slave, stdout=slave, stderr=slave,
                           env=environment, close_fds=True)


def collect(seconds=0.15):
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


def wait_until(predicate, description, timeout=25, interval=0.1):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        collect(interval)
        if predicate():
            return
        if process.poll() is not None:
            break
    raise AssertionError(f"Timed out waiting for {description}.\n{model.text()}")


def wait_screen(marker, timeout=25):
    wait_until(lambda: marker in model.text(), repr(marker) + " in live screen", timeout)


def send(text):
    os.write(master, text.encode("utf-8") if isinstance(text, str) else text)


def paste(text):
    send("\x1b[200~" + text + "\x1b[201~")
    collect(0.15)


def expression(text):
    paste(text)
    send("\r")


def resize(rows, columns):
    model.resize(rows, columns)
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
    os.kill(process.pid, signal.SIGWINCH)


try:
    wait_screen(">")
    code = ('initial_cfg <- prismrepl::repl_config(ui="fullscreen",color=TRUE,'
            'sidebar=TRUE,editor_height=6L,status=c("directory","objects","packages"),'
            'confirm=FALSE,link_context=FALSE);'
            'stopifnot(!file.exists(initial_cfg$settings_file));'
            'prismrepl::repl(config=initial_cfg);cat("PRISM_FULLSCREEN_DONE\\n");q("no")')
    send(code + "\n")
    wait_screen("PRISM REPL")
    wait_screen("SESSION INFO")
    assert model.alternate and model.enters == 1 and model.leaves == 0
    assert "F1 keys" in model.text().splitlines()[-1], model.text()
    with open(environment["PRISMREPL_CONFIG_FILE"], encoding="utf-8") as saved_file:
        initial_saved = saved_file.read()
    first_theme = next(line.split(":", 1)[1].strip() for line in initial_saved.splitlines()
                       if line.startswith("Theme:"))
    assert first_theme in ("paper", "midnight"), initial_saved
    if sys.platform.startswith("linux"):
        assert first_theme == "paper", initial_saved
    wait_until(lambda: model.cursor_color == ("#000000" if first_theme == "paper" else "#FFFFFF")
               and model.cursor_visible, "visible cursor contrasting with initial theme")

    # Actual typed keys insert pairs and consume their generated closers once.
    send("paired_value <- sum(c(1, 2))")
    wait_screen("r> paired_value <- sum(c(1, 2))")
    send("\r")
    wait_until(lambda: "paired_value  numeric" in model.text() and
               "READY" in model.text().splitlines()[0], "typed pair evaluation completed")
    expression('stopifnot(identical(paired_value,3));cat("PAIRS_", "TYPED_OK\\n",sep="")')
    wait_screen("PAIRS_TYPED_OK")

    # A complete first line must remain editable after Ctrl+J, including on
    # terminals where Alt+Enter is intercepted before it reaches R.
    paste("multi_value <- 20")
    send(b"\n")
    collect(0.1)
    paste('multi_value <- multi_value + 22; cat("CTRLJ_", multi_value, "\\n", sep="")')
    assert "CTRLJ_42" not in model.text(), "Ctrl+J submitted complete input"
    send("\r")
    wait_screen("CTRLJ_42")

    # Response time is measured from key delivery to the final cursor update.
    # This is recorded rather than asserted against machine-dependent limits.
    paste("latency_probe")
    latencies = []
    for _ in range(12):
        before = (model.row, model.column)
        start = time.perf_counter()
        send("a")
        wait_until(lambda: (model.row, model.column) != before,
                   "single-key repaint", interval=0.005)
        latencies.append((time.perf_counter() - start) * 1000)
    send("\x03")
    collect(0.1)

    expression('x <- 41; cat("FULL_", "EVAL=", x + 1, "\\n", sep="")')
    wait_screen("FULL_EVAL=42")
    expression('warning(paste0("FULL_", "WARNING")); '
               'message(paste0("FULL_", "MESSAGE")); stop(paste0("FULL_", "ERROR"))')
    wait_screen("FULL_ERROR")
    wait_until(lambda: b"[WARNING]" in raw and b"[MESSAGE]" in raw and b"[ERROR]" in raw,
               "distinct warning, message, and error badges")
    wait_until(lambda: "ERROR" in "".join(model.cells[0]), "error mode badge")
    assert len(model.sgr) >= 6, model.sgr
    assert any(link.startswith("https://chat.mistral.ai/chat?q=") for link in model.links)
    expression(":ai claude")
    wait_screen("AI provider: Claude (saved)")
    expression('stop(paste0("CLAUDE_", "ERROR"))')
    wait_screen("Explain with Claude")
    assert any(link.startswith("https://claude.ai/new?q=") for link in model.links), model.links
    expression(":ai gemini")
    wait_screen("AI provider: Gemini (saved)")
    expression('stop(paste0("GEMINI_", "ERROR"))')
    wait_screen("Open Gemini")
    assert "https://gemini.google.com/app" in model.links, model.links
    assert ":explain copy" in model.text(), model.text()
    expression(":ai chatgpt")
    wait_screen("AI provider: ChatGPT (saved)")

    expression('cat(sprintf("SCROLL_%03d\\n", seq_len(70)))')
    wait_screen("SCROLL_070")
    paste("kept_scroll <- 1")
    send("\x1b[5~")
    wait_screen("lines above live")
    assert "SCROLL_070" not in model.text(), model.text()
    assert "kept_scroll" in model.text(), model.text()
    send("\x1b[6~")
    wait_screen("SCROLL_070")
    assert "kept_scroll" in model.text(), model.text()
    send("\x1b[<64;12;8M")
    wait_screen("lines above live")
    send("\x1b[<65;12;8M")
    wait_screen("SCROLL_070")
    send("\x03")
    collect(0.2)

    paste('pending <- c("kept", "through resize"); cat("RESIZE_", "BUFFER_OK\\n", sep="")')
    resize(14, 55)
    wait_screen("F1 keys")
    assert "SESSION INFO" not in model.text(), model.text()
    assert "RESIZE_BUFFER_OK" not in model.text(), "Paste executed before explicit Enter"
    send("\r")
    wait_screen("RESIZE_BUFFER_OK")
    resize(32, 132)
    wait_screen("SESSION INFO")

    # Click a Unicode boundary on the second physical row of one long logical
    # expression. SGR mouse coordinates are one-based terminal cells.
    resize(24, 55)
    wait_screen("F1 keys")
    paste('invisible("0123456789012345678901234567890123456789"); '
          'mouse_value <- "ab界e\u0301cd"; cat("MOUSE_", mouse_value, "\\n", sep="")')
    wait_screen("mouse_value")
    locations = [(row, column) for row, cells in enumerate(model.cells)
                 for column, cell in enumerate(cells) if cell == "界"]
    assert len(locations) == 1, model.text()
    mouse_row, mouse_column = locations[0]
    prompt_rows = [row for row, cells in enumerate(model.cells)
                   if "r> invisible" in "".join(cells)]
    assert prompt_rows and mouse_row > prompt_rows[-1], model.text()
    send(f"\x1b[<0;{mouse_column + 3};{mouse_row + 1}M")
    send(f"\x1b[<0;{mouse_column + 3};{mouse_row + 1}m")
    collect(0.1)
    send("X")
    wait_screen("ab界Xe\u0301cd")
    send("\r")
    wait_screen("MOUSE_ab界Xe\u0301cd")
    resize(32, 132)
    wait_screen("SESSION INFO")

    enters, leaves = model.enters, model.leaves
    expression(":keys")
    wait_screen("Keyboard shortcuts")
    wait_until(lambda: not model.cursor_visible, "read-only viewer hides insertion cursor")
    assert model.enters == enters and model.leaves == leaves, "Modal panel nested alternate buffers"
    send("q")
    wait_screen("PRISM REPL")
    wait_until(lambda: model.cursor_visible, "editor cursor restored after viewer")
    assert model.leaves == leaves, "Closing the modal left the main alternate buffer"

    old_colors = set(model.sgr)
    expression(":theme " + ("midnight" if first_theme == "paper" else "paper"))
    wait_until(lambda: bool(model.sgr - old_colors), "changed theme ANSI palette")
    wait_screen("SESSION INFO")
    assert "F1 keys" in model.text().splitlines()[-1], model.text()
    assert not model.bad_cursor, model.bad_cursor

    # Theme navigation is provisional. Escape must preserve the data-only
    # preferences byte for byte; keyboard and mouse choices require Enter.
    with open(environment["PRISMREPL_CONFIG_FILE"], encoding="utf-8") as saved_file:
        before_picker = saved_file.read()
    expression(":theme")
    wait_screen("Theme picker; current:")
    wait_screen("Enter apply")
    assert model.enters == enters and model.leaves == leaves
    send("\x1b[B")
    collect(0.15)
    send("\x1b")
    wait_screen("PRISM REPL")
    with open(environment["PRISMREPL_CONFIG_FILE"], encoding="utf-8") as saved_file:
        assert saved_file.read() == before_picker, "Cancelled theme navigation saved preferences"

    # Search may finish without accepting; the next Enter applies the theme.
    expression(":t")
    wait_screen("Theme picker; current:")
    send("/black")
    wait_screen("Search: black")
    send("\r")
    wait_screen("Enter apply")
    with open(environment["PRISMREPL_CONFIG_FILE"], encoding="utf-8") as saved_file:
        assert saved_file.read() == before_picker, "Ending search prematurely accepted a theme"
    send("\r")
    wait_screen("Theme: black (saved)")
    assert any("48;2;0;0;0" in code for code in model.sgr), model.sgr
    wait_until(lambda: model.cursor_color == "#FFFFFF" and model.cursor_visible,
               "white insertion cursor on black editor")
    cursor_updates = len(model.cursor_colors)
    send("abc")
    wait_screen("r> abc")
    assert len(model.cursor_colors) == cursor_updates, "Typing redundantly changed cursor color"
    send("\x03")
    collect(0.15)

    expression(":theme")
    wait_screen("Theme picker; current: black")
    white_rows = [row for row, cells in enumerate(model.cells)
                  if re.search(r"\bwhite\b", "".join(cells)) and "#FFFFFF" in "".join(cells)]
    assert len(white_rows) == 1, model.text()
    row = white_rows[0] + 1
    send(f"\x1b[<0;8;{row}M")
    send(f"\x1b[<0;8;{row}m")
    collect(0.15)
    assert "Theme picker; current: black" in model.text(), "Click applied a theme before Enter"
    with open(environment["PRISMREPL_CONFIG_FILE"], encoding="utf-8") as saved_file:
        assert "Theme: black\n" in saved_file.read(), "Mouse focus saved a theme before Enter"
    # Reflowing the selectable table must retain its focused theme.
    resize(18, 38)
    wait_screen("Theme picker;")
    collect(0.15)
    send("\r")
    wait_screen("Theme: white (saved)")
    resize(32, 132)
    wait_screen("SESSION INFO")
    assert any("48;2;255;255;255" in code for code in model.sgr), model.sgr
    wait_until(lambda: model.cursor_color == "#000000" and model.cursor_visible,
               "black insertion cursor on white editor")
    assert "F1 keys" in model.text().splitlines()[-1], model.text()
    expression(":bind newline F4")
    wait_screen("F4 newline")
    expression(":rainbow off")
    wait_screen("Rainbow delimiters: off (saved)")
    expression(":autopair off")
    wait_screen("Automatic pairing: off (saved)")
    assert model.alternate_scrolls == 0, ("The alternate screen rolled", model.alternate_scrolls)

    expression(":quit")
    wait_until(lambda: b"PRISM_FULLSCREEN_DONE\r\n" in raw or b"PRISM_FULLSCREEN_DONE\n" in raw,
               "completion after restoring the original buffer")
    process.wait(timeout=10)
    assert process.returncode == 0, process.returncode
    assert model.enters == 1 and model.leaves == 1, (model.enters, model.leaves)
    assert not model.alternate
    assert model.cursor_color == "profile" and model.cursor_visible, "Configured cursor was not restored"
    assert termios.tcgetattr(slave) == original_modes, "Terminal modes were not restored exactly"

    # Restart a real R process using the same isolated data-only preferences.
    model = Screen(32, 132)
    restart_start = len(raw)
    process = subprocess.Popen([r, "--vanilla", "--quiet", "--interactive"],
                               stdin=slave, stdout=slave, stderr=slave,
                               env=environment, close_fds=True)
    wait_screen(">")
    restart = ('restart_cfg <- prismrepl::repl_config(ui="fullscreen",color=TRUE,'
               'sidebar=TRUE,editor_height=6L,confirm=FALSE);'
               'stopifnot(identical(restart_cfg$rainbow,FALSE),identical(restart_cfg$autopair,FALSE),'
               'identical(restart_cfg$ai_provider,"chatgpt"));'
               'prismrepl::repl(config=restart_cfg);'
               'cat("PRISM_RESTART_DONE\\n");q("no")')
    send(restart + "\n")
    wait_screen("PRISM REPL")
    wait_screen("F4 newline")
    wait_until(lambda: model.cursor_color == "#000000" and model.cursor_visible,
               "saved white theme restores contrasting insertion cursor")
    assert any("48;2;255;255;255" in code for code in model.sgr), model.sgr
    send("literal_pair <- (")
    wait_screen("r> literal_pair <- (")
    assert "literal_pair <- ()" not in model.text(), model.text()
    send("\x03")
    collect(0.1)
    expression('stop(paste0("CHATGPT_", "RESTART"))')
    wait_screen("Explain with ChatGPT")
    assert any(link.startswith("https://chatgpt.com/?q=") for link in model.links), model.links
    paste("restart_value <- 40")
    send("\x1bOS")
    collect(0.1)
    paste('restart_value <- restart_value + 2; cat("PERSIST_", restart_value, "\\n", sep="")')
    assert "PERSIST_42" not in model.text(), "The persisted newline shortcut submitted input"
    send("\r")
    wait_screen("PERSIST_42")
    assert model.alternate_scrolls == 0, ("The restarted screen rolled", model.alternate_scrolls)
    expression(":quit")
    wait_until(lambda: b"PRISM_RESTART_DONE\r\n" in raw[restart_start:] or
               b"PRISM_RESTART_DONE\n" in raw[restart_start:], "completion after preference restart")
    process.wait(timeout=10)
    assert process.returncode == 0, process.returncode
    assert model.enters == 1 and model.leaves == 1, (model.enters, model.leaves)
    assert termios.tcgetattr(slave) == original_modes, "Restarted terminal modes were not restored"
    assert model.cursor_color == "profile" and model.cursor_visible, "Restarted session leaked its cursor color"
    if os.environ.get("PRISM_CAPTURE"):
        with open(os.environ["PRISM_CAPTURE"], "wb") as capture:
            capture.write(raw)
    print("Fullscreen PTY passed: real ANSI colors and condition badges, clickable links,")
    print("sidebar, footer, keyboard/mouse scrolling, resize-safe and wrapped Unicode input,")
    print("mouse cursor placement, Ctrl+J, modal reuse, and no screen rolling.")
    print("Theme picker passed cancellation, search, keyboard/mouse focus, Enter acceptance and resize.")
    print("Black/white themes use contrasting cursors; unchanged color is cached; exit restores profile color.")
    print("Initial theme was detected and saved without configuration-time writes.")
    print("Typed pairing and AI destinations passed; theme, F4, AI and editing options survived restart.")
    print("Terminal modes were restored after both sessions.")
    print(f"Real PTY key-to-cursor latency: median={statistics.median(latencies):.2f}ms, "
          f"max={max(latencies):.2f}ms across {len(latencies)} edits.")
except BaseException:
    sys.stderr.write("Live terminal cells at failure:\n" + model.text() + "\n")
    stripped = re.sub(rb"\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)|\x1b\[[0-?]*[ -/]*[@-~]", b"", bytes(raw))
    sys.stderr.write("Recent terminal text:\n" + stripped[-24000:].decode("utf-8", "replace") + "\n")
    raise
finally:
    if os.environ.get("PRISM_CAPTURE"):
        with open(os.environ["PRISM_CAPTURE"], "wb") as capture:
            capture.write(raw)
    if process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
    os.close(master)
    os.close(slave)
    preferences.cleanup()
