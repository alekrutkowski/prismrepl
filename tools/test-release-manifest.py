"""Offline regression checks for the GitHub release gate."""
import hashlib
import importlib.util
import io
import pathlib
import sys
import tarfile
import tempfile
import unittest

sys.dont_write_bytecode = True

spec = importlib.util.spec_from_file_location(
    "release_manifest", pathlib.Path(__file__).with_name("release-manifest.py"))
release = importlib.util.module_from_spec(spec)
spec.loader.exec_module(release)


class ReleaseManifestTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="prism-release-test-")
        self.addCleanup(self.temporary.cleanup)
        self.directory = pathlib.Path(self.temporary.name)
        self.root = self.directory / "assets"
        self.root.mkdir()
        self.description = self.directory / "DESCRIPTION"
        self.description.write_text("Package: fixturepkg\nVersion: 1.2.3\n", encoding="utf-8")
        self.fields = {}
        for platform, arch in sorted(release.TARGETS):
            native_arch = "aarch64" if arch == "arm64" else arch
            suffix = {"linux": "pc-linux-gnu", "macos": "apple-darwin20", "windows": "w64-mingw32"}[platform]
            self.fields[(platform, arch)] = {
                "package": "fixturepkg", "version": "1.2.3", "R": "4.6.2",
                "platform": platform, "architecture": arch,
                "r_platform": f"{native_arch}-{suffix}", "operating_system": "Test runner",
                "binary": f"fixturepkg_1.2.3-R4.6-{platform}-{arch}{release.EXTENSIONS[platform]}",
                "commit": "",
            }
            self.write_manifest((platform, arch))
            (self.root / self.fields[(platform, arch)]["binary"]).write_bytes(b"fixture archive bytes")
        self.write_source()

    def write_manifest(self, key):
        fields = self.fields[key]
        path = self.root / (fields["binary"] + ".manifest")
        path.write_text("".join(f"{name}: {value}\n" for name, value in fields.items()), encoding="utf-8")
        return path

    def write_source(self, version="1.2.3", unsafe=False):
        path = self.root / "fixturepkg_1.2.3.tar.gz"
        with tarfile.open(path, "w:gz") as archive:
            data = f"Package: fixturepkg\nVersion: {version}\n".encode()
            item = tarfile.TarInfo("fixturepkg/DESCRIPTION")
            item.size = len(data)
            archive.addfile(item, io.BytesIO(data))
            if unsafe:
                item = tarfile.TarInfo("fixturepkg/../outside")
                item.size = 1
                archive.addfile(item, io.BytesIO(b"x"))
        return path

    def validate(self, env=None):
        return release.validate_release(self.root, self.description, {} if env is None else env)

    def test_complete_local_release_with_empty_commits(self):
        self.assertEqual(self.validate(), ("fixturepkg", "1.2.3"))
        checks = (self.root / "SHA256SUMS.txt").read_text().splitlines()
        self.assertEqual(len(checks), 13)
        for line in checks:
            digest, name = line.split("  ", 1)
            self.assertEqual(digest, hashlib.sha256((self.root / name).read_bytes()).hexdigest())
        self.assertEqual(self.validate(), ("fixturepkg", "1.2.3"))

    def test_ci_tag_and_manual_dispatch_commits(self):
        commit = "a" * 40
        for key in self.fields:
            self.fields[key]["commit"] = commit
            self.write_manifest(key)
        self.assertEqual(self.validate({"GITHUB_SHA": commit, "GITHUB_REF": "refs/tags/v1.2.3",
                                        "GITHUB_REF_NAME": "v1.2.3"}), ("fixturepkg", "1.2.3"))
        self.assertEqual(self.validate({"GITHUB_SHA": commit, "GITHUB_REF": "refs/heads/main",
                                        "GITHUB_REF_NAME": "main"}), ("fixturepkg", "1.2.3"))

    def test_missing_binary_or_target_fails(self):
        key = ("windows", "arm64")
        (self.root / self.fields[key]["binary"]).unlink()
        with self.assertRaisesRegex(ValueError, "Missing binary"):
            self.validate()
        (self.root / (self.fields[key]["binary"] + ".manifest")).unlink()
        with self.assertRaisesRegex(ValueError, "target mismatch"):
            self.validate()

    def test_mixed_package_version_or_commit_fails(self):
        key = ("linux", "arm64")
        for field, value in (("package", "anotherpkg"), ("version", "9.9.9"), ("commit", "b" * 40)):
            with self.subTest(field=field):
                original = self.fields[key][field]
                self.fields[key][field] = value
                self.write_manifest(key)
                with self.assertRaisesRegex(ValueError, "DESCRIPTION|mixed commit"):
                    self.validate()
                self.fields[key][field] = original
                self.write_manifest(key)

    def test_wrong_checkout_tag_or_sha_fails(self):
        with self.assertRaisesRegex(ValueError, "tag"):
            self.validate({"GITHUB_REF": "refs/tags/v9.9.9"})
        with self.assertRaisesRegex(ValueError, "tag"):
            self.validate({"GITHUB_REF": "refs/tags/v1.2.3", "GITHUB_REF_NAME": "v2.0.0"})
        with self.assertRaisesRegex(ValueError, "GITHUB_SHA"):
            self.validate({"GITHUB_SHA": "c" * 40})
        with self.assertRaisesRegex(ValueError, "complete commit"):
            self.validate({"GITHUB_SHA": "short"})

    def test_source_is_required_and_matches_description(self):
        (self.root / "fixturepkg_1.2.3.tar.gz").unlink()
        with self.assertRaisesRegex(ValueError, "missing its source"):
            self.validate()
        self.write_source(version="9.9.9")
        with self.assertRaisesRegex(ValueError, "Source package does not match"):
            self.validate()
        self.write_source(unsafe=True)
        with self.assertRaisesRegex(ValueError, "Unsafe member"):
            self.validate()

    def test_unexpected_files_and_subdirectories_fail(self):
        path = self.root / "stale.tar.gz"
        path.write_bytes(b"stale")
        with self.assertRaisesRegex(ValueError, "stale"):
            self.validate()
        path.unlink()
        path.mkdir()
        with self.assertRaisesRegex(ValueError, "regular files"):
            self.validate()

    def test_manifest_cannot_traverse_paths_or_mislabel_architecture(self):
        key = ("linux", "arm64")
        path = self.write_manifest(key)
        text = path.read_text()
        path.write_text(text.replace(self.fields[key]["binary"], "../outside.tar.gz"))
        with self.assertRaisesRegex(ValueError, "Unsafe binary"):
            self.validate()
        self.fields[key]["r_platform"] = "x86_64-pc-linux-gnu"
        self.write_manifest(key)
        with self.assertRaisesRegex(ValueError, "R platform"):
            self.validate()

    def test_duplicate_and_missing_manifest_fields_fail(self):
        path = self.write_manifest(("linux", "arm64"))
        original = path.read_text()
        path.write_text(original + "version: 1.2.3\n")
        with self.assertRaisesRegex(ValueError, "duplicate DCF"):
            self.validate()
        path.write_text(original.replace("commit: \n", ""))
        with self.assertRaisesRegex(ValueError, "Missing manifest fields"):
            self.validate()

    def test_mixed_r_series_fails(self):
        key = ("macos", "arm64")
        old = self.fields[key]["binary"]
        self.fields[key]["R"] = "4.7.0"
        self.fields[key]["binary"] = old.replace("-R4.6-", "-R4.7-")
        (self.root / old).rename(self.root / self.fields[key]["binary"])
        (self.root / (old + ".manifest")).unlink()
        self.write_manifest(key)
        with self.assertRaisesRegex(ValueError, "mixed R"):
            self.validate()

    def test_dcf_folding_and_empty_commit_are_preserved(self):
        self.assertEqual(release.read_dcf("operating_system: Name\n continuation\ncommit:\n", "fixture"),
                         {"operating_system": "Name continuation", "commit": ""})
        for name in ("../escape", "sub/file", "sub\\file", "C:drive", "line\nfeed", ".", ".."):
            self.assertFalse(release.safe_name(name))


if __name__ == "__main__":
    unittest.main(verbosity=2)
