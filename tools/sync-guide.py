#!/usr/bin/env python3
"""Rebuild the installed text guide from the user-facing README sections."""

from pathlib import Path
import re

root = Path(__file__).resolve().parent.parent
readme = (root / "README.md").read_text(encoding="utf-8")
version = re.search(r"^Version: (.+)$", (root / "DESCRIPTION").read_text(), re.M).group(1)
capabilities = readme[readme.index("## Capabilities at a glance"):readme.index("## Actual renderer examples")]
guide = readme[readme.index("## Install and start"):readme.index("## GitHub and CRAN development")]
guide = guide.replace(
    "[validation record](VALIDATION.md)",
    "validation record included in the source repository",
)
(root / "inst" / "doc" / "guide.md").write_text(
    f"# Prism REPL guide\n\nVersion {version}.\n\n" + capabilities + guide,
    encoding="utf-8",
)
