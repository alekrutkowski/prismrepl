"""Validate a complete, consistent six-target release and write SHA-256 checksums."""
import argparse
import hashlib
import os
import pathlib
import re
import sys
import tarfile


TARGETS = {(platform, arch) for platform in ("linux", "macos", "windows")
           for arch in ("x86_64", "arm64")}
EXTENSIONS = {"linux": ".tar.gz", "macos": ".tgz", "windows": ".zip"}


def read_dcf(text, label):
    """Read one DCF record, preserving folded values and empty fields."""
    fields = {}
    previous = None
    for line in text.splitlines():
        if not line.strip():
            continue
        if line.startswith((" ", "\t")):
            if previous is None:
                raise ValueError(f"Unexpected DCF continuation in {label}")
            fields[previous] += " " + line.strip()
            continue
        key, separator, value = line.partition(":")
        if not separator or not re.fullmatch(r"[^\s:]+", key) or key in fields:
            raise ValueError(f"Invalid or duplicate DCF field in {label}: {key!r}")
        previous = key
        fields[key] = value.strip()
    return fields


def safe_name(name):
    """Accept a single portable filename, with no path traversal or controls."""
    return bool(name and name not in (".", "..") and
                pathlib.PurePosixPath(name).name == name and
                not any(ch in name for ch in "\\/:\x00") and
                not any(ord(ch) < 32 or ord(ch) == 127 for ch in name))


def source_description(path, package):
    """Read source DESCRIPTION without extracting archive members to disk."""
    with tarfile.open(path, "r:gz") as archive:
        for member in archive.getmembers():
            parts = pathlib.PurePosixPath(member.name).parts
            if (not parts or parts[0] != package or ".." in parts or
                    "\\" in member.name or member.issym() or member.islnk() or
                    not (member.isfile() or member.isdir())):
                raise ValueError(f"Unsafe member in source archive: {member.name!r}")
        try:
            member = archive.getmember(f"{package}/DESCRIPTION")
        except KeyError as error:
            raise ValueError("Source archive is missing its DESCRIPTION") from error
        if not member.isfile() or member.size > 65536:
            raise ValueError("Source DESCRIPTION must be a regular file below 64 KiB")
        stream = archive.extractfile(member)
        if stream is None:
            raise ValueError("Cannot read source DESCRIPTION")
        with stream:
            return read_dcf(stream.read().decode("utf-8"), path.name)


def validate_release(root, description=pathlib.Path("DESCRIPTION"), env=None):
    """Validate manifests against repository metadata before hashing artifacts."""
    env = os.environ if env is None else env
    root = pathlib.Path(root)
    description = pathlib.Path(description)
    if not root.is_dir():
        raise ValueError("Release assets directory does not exist")
    metadata = read_dcf(description.read_text(encoding="utf-8"), "DESCRIPTION")
    package, version = metadata.get("Package", ""), metadata.get("Version", "")
    if not re.fullmatch(r"[A-Za-z][A-Za-z0-9.]*", package) or not re.fullmatch(r"[0-9]+(?:[.-][0-9]+)*", version):
        raise ValueError("DESCRIPTION has an invalid package name or version")
    ref = env.get("GITHUB_REF", "")
    if ref.startswith("refs/tags/"):
        tag = ref.removeprefix("refs/tags/")
        if tag != f"v{version}" or env.get("GITHUB_REF_NAME", tag) != tag:
            raise ValueError("Release tag does not match DESCRIPTION version")
    expected_commit = env.get("GITHUB_SHA", "")
    if expected_commit and not re.fullmatch(r"[0-9a-fA-F]{40}(?:[0-9a-fA-F]{24})?", expected_commit):
        raise ValueError("GITHUB_SHA is not a complete commit hash")
    files = list(root.iterdir())
    if any(path.is_symlink() or not path.is_file() for path in files):
        raise ValueError("Release assets must contain regular files only")
    if any(not safe_name(path.name) for path in files):
        raise ValueError("Release assets contain an unsafe filename")
    found, commits, series, expected_files = set(), set(), set(), set()
    manifests = sorted(path for path in files if path.name.endswith(".manifest"))
    required = {"package", "version", "R", "platform", "architecture", "r_platform", "binary", "commit"}
    for path in manifests:
        fields = read_dcf(path.read_text(encoding="utf-8"), path.name)
        if not required <= fields.keys():
            raise ValueError(f"Missing manifest fields in {path.name}: {sorted(required - fields.keys())}")
        if fields["package"] != package or fields["version"] != version:
            raise ValueError(f"Manifest package/version differs from DESCRIPTION: {path.name}")
        key = (fields["platform"], fields["architecture"])
        if key not in TARGETS or key in found:
            raise ValueError(f"Unexpected or duplicate release target: {key}")
        found.add(key)
        if not re.fullmatch(r"[0-9]+(?:\.[0-9]+){1,3}", fields["R"]):
            raise ValueError(f"Invalid R version in {path.name}")
        r_series = ".".join(fields["R"].split(".")[:2])
        series.add(r_series)
        arch_pattern = r"aarch64|arm64" if key[1] == "arm64" else r"x86_64|x64|amd64"
        os_pattern = {"linux": "linux", "macos": "apple-darwin", "windows": "mingw|windows"}[key[0]]
        if not re.search(arch_pattern, fields["r_platform"], re.I) or not re.search(os_pattern, fields["r_platform"], re.I):
            raise ValueError(f"R platform does not match manifest target: {path.name}")
        binary = fields["binary"]
        if not safe_name(binary):
            raise ValueError(f"Unsafe binary filename in {path.name}")
        expected_binary = f"{package}_{version}-R{r_series}-{key[0]}-{key[1]}{EXTENSIONS[key[0]]}"
        if binary != expected_binary or path.name != f"{binary}.manifest":
            raise ValueError(f"Binary or manifest name does not match its metadata: {path.name}")
        if not (root / binary).is_file():
            raise ValueError(f"Missing binary for {key}")
        commit = fields["commit"]
        if commit and not re.fullmatch(r"[0-9a-fA-F]{40}(?:[0-9a-fA-F]{24})?", commit):
            raise ValueError(f"Invalid commit hash in {path.name}")
        if expected_commit and commit != expected_commit:
            raise ValueError(f"Manifest commit differs from GITHUB_SHA: {path.name}")
        commits.add(commit)
        expected_files.update((binary, path.name))
    if found != TARGETS:
        raise ValueError(f"Release target mismatch: missing={sorted(TARGETS-found)}, extra={sorted(found-TARGETS)}")
    if len(commits) != 1:
        raise ValueError("Release manifests contain mixed commit hashes")
    if len(series) != 1:
        raise ValueError("Release manifests contain mixed R major/minor versions")
    source_name = f"{package}_{version}.tar.gz"
    source_path = root / source_name
    if not source_path.is_file():
        raise ValueError("Release is missing its source package archive")
    source = source_description(source_path, package)
    if source.get("Package") != package or source.get("Version") != version:
        raise ValueError("Source package does not match repository DESCRIPTION")
    expected_files.add(source_name)
    actual_files = {path.name for path in files} - {"SHA256SUMS.txt"}
    if actual_files != expected_files:
        raise ValueError(f"Unexpected or stale release files: {sorted(actual_files-expected_files)}")
    checks = []
    for name in sorted(expected_files):
        digest = hashlib.sha256()
        with (root / name).open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(chunk)
        checks.append(f"{digest.hexdigest()}  {name}")
    (root / "SHA256SUMS.txt").write_text("\n".join(checks) + "\n", encoding="utf-8")
    return package, version


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=pathlib.Path)
    parser.add_argument("--description", type=pathlib.Path, default=pathlib.Path("DESCRIPTION"))
    args = parser.parse_args()
    try:
        package, version = validate_release(args.directory, args.description)
    except (ValueError, OSError, tarfile.TarError) as error:
        print(f"Release validation failed: {error}", file=sys.stderr)
        return 1
    print(f"Validated {package} {version}: six native targets, source archive and SHA256SUMS.txt.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
