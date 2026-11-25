#!/usr/bin/env python3
"""Verify the exact prismrepl source tarball before checking or submitting it.

Run from the repository: python3 tools/check-source-archive.py PATH.tar.gz
Only the Python standard library is required. This never modifies the archive.
"""
import argparse
from datetime import datetime
import hashlib
from pathlib import Path
import re
import tarfile


def description_fields(text):
    fields = {}
    key = None
    for line in text.splitlines():
        if line[:1].isspace() and key:
            fields[key] += " " + line.strip()
        elif line.strip():
            key, separator, value = line.partition(":")
            if not separator or key in fields:
                raise ValueError("DESCRIPTION must contain unique DCF fields")
            fields[key] = value.strip()
    return fields


def check_archive(path, description):
    path, description = Path(path), Path(description)
    expected = description_fields(description.read_text(encoding="utf-8"))
    package, version = expected["Package"], expected["Version"]
    if not re.fullmatch(r"[A-Za-z][A-Za-z0-9.]*", package) or not re.fullmatch(r"[0-9][A-Za-z0-9.+-]*", version):
        raise ValueError("DESCRIPTION must identify a valid Package and Version")
    if path.name != f"{package}_{version}.tar.gz":
        raise ValueError("Archive filename must match the current Package and Version")
    root_files = {"DESCRIPTION", "LICENSE", "NAMESPACE"}
    root_directories = {"R", "src", "man", "tests", "inst"}
    allowed = root_files | root_directories
    required = {"DESCRIPTION", "LICENSE", "NAMESPACE", "R/repl.R", "src/init.c",
                "src/terminal.c", "tests/testthat.R", "inst/doc/guide.md"}
    with tarfile.open(path, "r:gz") as archive:
        seen = set()
        files = {}
        for member in archive.getmembers():
            name = member.name[:-1] if member.isdir() and member.name.endswith("/") else member.name
            parts = name.split("/")
            # Check the raw components before path normalization can hide aliases.
            # Backslashes and colons are also unsafe when extracted on Windows.
            if (not name or parts[0] != package or any(part in ("", ".", "..") for part in parts) or
                    re.search(r"[\\:\x00-\x1f\x7f]", name) or name.casefold() in seen):
                raise ValueError(f"Unsafe or duplicate archive member: {member.name}")
            seen.add(name.casefold())
            if not (member.isdir() or member.isfile()):
                raise ValueError(f"Unexpected archive member type: {member.name}")
            if len(parts) == 1 and not member.isdir():
                raise ValueError("Package root must be a directory")
            if len(parts) > 1 and parts[1] not in allowed:
                raise ValueError(f"Repository-only or unexpected source content: {member.name}")
            if len(parts) > 1 and ((parts[1] in root_files and (len(parts) != 2 or not member.isfile())) or
                    (parts[1] in root_directories and len(parts) == 2 and not member.isdir())):
                raise ValueError(f"Unexpected source file or directory: {member.name}")
            if name.lower().endswith((".o", ".obj", ".a", ".so", ".dll", ".dylib", ".pyc")):
                raise ValueError(f"Compiled artifact in source package: {member.name}")
            if member.isfile():
                files["/".join(parts[1:])] = member
        if not required <= files.keys():
            raise ValueError(f"Missing source files: {sorted(required - files.keys())}")
        metadata = description_fields(archive.extractfile(files["DESCRIPTION"]).read().decode("utf-8"))
        for key in ("Package", "Version", "Title"):
            if metadata.get(key) != expected[key]:
                raise ValueError(f"Archived {key} does not match the source DESCRIPTION")
        if "Built" in metadata:
            raise ValueError("This is an installed binary, not a CRAN source package")
        packaged = metadata.get("Packaged", "")
        if not re.fullmatch(r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} UTC; \S.*", packaged):
            raise ValueError("Missing R CMD build timestamp; rebuild with R CMD build")
        try:
            datetime.strptime(packaged.split(" UTC;", 1)[0], "%Y-%m-%d %H:%M:%S")
        except ValueError as error:
            raise ValueError("Invalid R CMD build timestamp") from error
    return metadata, len(files)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("archive", type=Path)
    parser.add_argument("--description", type=Path, default=Path("DESCRIPTION"))
    args = parser.parse_args()
    try:
        metadata, count = check_archive(args.archive, args.description)
    except (OSError, ValueError, KeyError, tarfile.TarError) as error:
        parser.exit(1, f"Source archive rejected: {error}\n")
    digest = hashlib.sha256()
    with args.archive.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    print(f"PASS: {metadata['Package']} {metadata['Version']}; {count} source files")
    print(f"Packaged: {metadata['Packaged']}")
    print(f"SHA256: {digest.hexdigest()}")


if __name__ == "__main__":
    main()
