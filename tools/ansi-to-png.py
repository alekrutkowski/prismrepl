#!/usr/bin/env python3
"""Rasterize exported real terminal cells without inventing colors or content.

Usage: python tools/ansi-to-png.py tools/figures/*.json
The JSON comes from render-demo.R and includes the exact grapheme widths used
by prismrepl. Pillow is the only Python dependency. This is a deterministic
rendering of the package's ANSI frame, not an operating-system screenshot.
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


ANSI16 = [
    (0, 0, 0), (128, 0, 0), (0, 128, 0), (128, 128, 0),
    (0, 0, 128), (128, 0, 128), (0, 128, 128), (192, 192, 192),
    (128, 128, 128), (255, 0, 0), (0, 255, 0), (255, 255, 0),
    (0, 0, 255), (255, 0, 255), (0, 255, 255), (255, 255, 255),
]


def rgb(text: str) -> tuple[int, int, int]:
    return tuple(int(text[i:i + 2], 16) for i in (1, 3, 5))


def indexed_color(index: int) -> tuple[int, int, int]:
    if index < 16:
        return ANSI16[index]
    if index < 232:
        offset = index - 16
        levels = [0, 95, 135, 175, 215, 255]
        return (levels[offset // 36], levels[(offset // 6) % 6], levels[offset % 6])
    gray = 8 + 10 * (index - 232)
    return (gray, gray, gray)


class Style:
    """Small SGR interpreter for the styling emitted by prismrepl."""

    def __init__(self, foreground: str, background: str):
        self.default_fg = rgb(foreground)
        self.default_bg = rgb(background)
        self.reset()

    def reset(self):
        self.fg = self.default_fg
        self.bg = self.default_bg
        self.bold = False
        self.italic = False
        self.underline = False
        self.inverse = False
        self.dim = False

    def consume(self, token: str):
        if not token.startswith("\x1b[") or not token.endswith("m"):
            return  # OSC8 hyperlinks consume no cells.
        codes = [int(value) if value else 0
                 for value in token[2:-1].replace(":", ";").split(";")]
        i = 0
        while i < len(codes):
            code = codes[i]
            if code == 0:
                self.reset()
            elif code == 1:
                self.bold = True
            elif code == 2:
                self.dim = True
            elif code == 3:
                self.italic = True
            elif code == 4:
                self.underline = True
            elif code == 7:
                self.inverse = True
            elif code == 22:
                self.bold = self.dim = False
            elif code == 23:
                self.italic = False
            elif code == 24:
                self.underline = False
            elif code == 27:
                self.inverse = False
            elif 30 <= code <= 37:
                self.fg = ANSI16[code - 30]
            elif 90 <= code <= 97:
                self.fg = ANSI16[code - 90 + 8]
            elif 40 <= code <= 47:
                self.bg = ANSI16[code - 40]
            elif 100 <= code <= 107:
                self.bg = ANSI16[code - 100 + 8]
            elif code in (38, 48) and i + 2 < len(codes):
                value = None
                if codes[i + 1] == 5:
                    value = indexed_color(codes[i + 2])
                    i += 2
                elif codes[i + 1] == 2 and i + 4 < len(codes):
                    value = tuple(codes[i + 2:i + 5])
                    i += 4
                if value is not None:
                    if code == 38:
                        self.fg = value
                    else:
                        self.bg = value
            elif code == 39:
                self.fg = self.default_fg
            elif code == 49:
                self.bg = self.default_bg
            i += 1


def render(path: Path, font_dir: Path, size: int) -> Path:
    data = json.loads(path.read_text(encoding="utf-8"))
    names = {
        (False, False): "DejaVuSansMono.ttf",
        (True, False): "DejaVuSansMono-Bold.ttf",
        (False, True): "DejaVuSansMono-Oblique.ttf",
        (True, True): "DejaVuSansMono-BoldOblique.ttf",
    }
    fonts = {style: ImageFont.truetype(str(font_dir / name), size)
             for style, name in names.items()}
    cell_width = math.ceil(fonts[(False, False)].getlength("M"))
    ascent, descent = fonts[(False, False)].getmetrics()
    cell_height = ascent + descent
    padding = 16
    image = Image.new("RGB", (data["width"] * cell_width + 2 * padding,
                              data["height"] * cell_height + 2 * padding),
                      rgb(data["default_bg"]))
    draw = ImageDraw.Draw(image)
    state = Style(data["default_fg"], data["default_bg"])
    for row, line in enumerate(data["cells"]):
        column = 0
        for token, width, control in zip(line["tokens"], line["widths"], line["control"]):
            if control:
                state.consume(token)
                continue
            x = padding + column * cell_width
            y = padding + row * cell_height
            fg, bg = (state.bg, state.fg) if state.inverse else (state.fg, state.bg)
            if state.dim:
                fg = tuple((a + b) // 2 for a, b in zip(fg, bg))
            if width:
                draw.rectangle((x, y, x + width * cell_width - 1, y + cell_height - 1), fill=bg)
            draw.text((x, y + ascent), token, font=fonts[(state.bold, state.italic)],
                      fill=fg, anchor="ls")
            if state.underline and width:
                draw.line((x, y + ascent + 2, x + width * cell_width - 1, y + ascent + 2), fill=fg)
            column += width
        if column > data["width"]:
            raise ValueError(f"{path.name}: row {row} overflows the terminal")
    # An outline represents the real cursor location without obscuring the text.
    if data.get("show_cursor", True):
        x = padding + data["cursor_column"] * cell_width
        y = padding + data["cursor_row"] * cell_height
        draw.rectangle((x, y, x + cell_width - 1, y + cell_height - 1),
                       outline=rgb(data["default_fg"]), width=1)
    output = path.with_suffix(".png")
    image.save(output, optimize=True)
    print(f"{output}: {image.width} x {image.height} pixels")
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("frames", nargs="+", type=Path)
    parser.add_argument("--font-dir", type=Path,
                        default=Path("/usr/share/fonts/truetype/dejavu"))
    parser.add_argument("--font-size", type=int, default=18)
    args = parser.parse_args()
    for frame in args.frames:
        render(frame, args.font_dir, args.font_size)


if __name__ == "__main__":
    main()
