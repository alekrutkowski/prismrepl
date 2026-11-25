"""Offline checks for mistakes in CRAN source archives, using only the standard library."""
import importlib.util
import io
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import unittest

sys.dont_write_bytecode = True

CHECKER = Path(__file__).with_name("check-source-archive.py")
spec = importlib.util.spec_from_file_location("source_archive", CHECKER)
source = importlib.util.module_from_spec(spec)
spec.loader.exec_module(source)


class SourceArchiveTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="prism-source-test-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.description = self.root / "DESCRIPTION"
        self.metadata = "Package: prismrepl\nVersion: 1.2.3\nTitle: An Interactive R Console\n"
        self.description.write_text(self.metadata, encoding="utf-8")
        self.packaged = "Packaged: 2026-11-24 10:18:40 UTC; builder\n"
        self.files = {
            "DESCRIPTION": (self.metadata + self.packaged).encode(),
            "LICENSE": b"YEAR: 2026\nCOPYRIGHT HOLDER: Example Author\n",
            "NAMESPACE": b"useDynLib(prismrepl, .registration=TRUE)\n",
            "R/repl.R": b"repl <- function() invisible(NULL)\n",
            "src/init.c": b"/* Registered native routines. */\n",
            "src/terminal.c": b"/* Native terminal implementation. */\n",
            "tests/testthat.R": b"library(testthat)\nlibrary(prismrepl)\n",
            "inst/doc/guide.md": b"# User guide\n",
            "man/repl.Rd": b"\\name{repl}\n\\title{Start a console}\n",
        }
        self.path = self.root / "prismrepl_1.2.3.tar.gz"

    def write(self, extra=()):
        with tarfile.open(self.path, "w:gz") as archive:
            root = tarfile.TarInfo("prismrepl/")
            root.type = tarfile.DIRTYPE
            archive.addfile(root)
            for name, data in self.files.items():
                member = tarfile.TarInfo("prismrepl/" + name)
                member.size = len(data)
                archive.addfile(member, io.BytesIO(data))
            for member, data in extra:
                member.size = len(data) if member.isfile() else 0
                archive.addfile(member, io.BytesIO(data) if member.isfile() else None)
        return self.path

    def check(self):
        return source.check_archive(self.path, self.description)

    def test_built_source_passes_without_extracting_or_modifying_it(self):
        self.write()
        before = self.path.read_bytes()
        metadata, count = self.check()
        self.assertEqual(metadata["Version"], "1.2.3")
        self.assertEqual(count, len(self.files))
        self.assertEqual(self.path.read_bytes(), before)
        self.assertEqual(sorted(path.name for path in self.root.iterdir()),
                         ["DESCRIPTION", "prismrepl_1.2.3.tar.gz"])

    def test_hand_zipped_repository_without_packaged_timestamp_is_rejected(self):
        self.files["DESCRIPTION"] = self.metadata.encode()
        self.write()
        with self.assertRaisesRegex(ValueError, "R CMD build timestamp"):
            self.check()

    def test_repository_readme_and_tools_are_rejected(self):
        for name in ("README.md", "tools/build-release.R", ".github/workflows/check.yaml"):
            with self.subTest(name=name):
                self.write([(tarfile.TarInfo("prismrepl/" + name), b"Repository content\n")])
                with self.assertRaisesRegex(ValueError, "Repository-only"):
                    self.check()

    def test_binaries_and_installed_package_metadata_are_rejected(self):
        self.files["DESCRIPTION"] += b"Built: R 4.6.2; x86_64-pc-linux-gnu; 2026-11-24; unix\n"
        self.write()
        with self.assertRaisesRegex(ValueError, "installed binary"):
            self.check()
        self.files["DESCRIPTION"] = (self.metadata + self.packaged).encode()
        for name in ("src/terminal.o", "src/terminal.obj", "src/prismrepl.dll", "src/prismrepl.so"):
            with self.subTest(name=name):
                self.write([(tarfile.TarInfo("prismrepl/" + name), b"compiled bytes")])
                with self.assertRaisesRegex(ValueError, "Compiled artifact"):
                    self.check()

    def test_stale_filename_and_description_metadata_are_rejected(self):
        for field, replacement in (("Package: prismrepl", "Package: otherpkg"),
                                   ("Version: 1.2.3", "Version: 1.2.2"),
                                   ("Title: An Interactive R Console", "Title: A Different Package")):
            with self.subTest(field=field):
                self.files["DESCRIPTION"] = (self.metadata.replace(field, replacement) + self.packaged).encode()
                self.write()
                with self.assertRaisesRegex(ValueError, "does not match"):
                    self.check()
        self.files["DESCRIPTION"] = (self.metadata + self.packaged).encode()
        self.write()
        stale = self.path.with_name("prismrepl_1.2.2.tar.gz")
        self.path.rename(stale)
        with self.assertRaisesRegex(ValueError, "filename"):
            source.check_archive(stale, self.description)

    def test_traversal_windows_paths_and_normalized_aliases_are_rejected(self):
        for name in ("prismrepl/R/../../outside", "/prismrepl/R/outside",
                     "prismrepl/R/..\\..\\outside", "prismrepl/R/C:outside",
                     "prismrepl/./DESCRIPTION", "prismrepl//DESCRIPTION",
                     "prismrepl/R/../DESCRIPTION", "prismrepl/R/line\nfeed.R"):
            with self.subTest(name=name):
                self.write([(tarfile.TarInfo(name), b"unexpected content")])
                with self.assertRaisesRegex(ValueError, "Unsafe"):
                    self.check()

    def test_duplicate_and_cross_platform_colliding_members_are_rejected(self):
        for name in ("prismrepl/DESCRIPTION", "prismrepl/R/Repl.R"):
            with self.subTest(name=name):
                self.write([(tarfile.TarInfo(name), b"replacement")])
                with self.assertRaisesRegex(ValueError, "duplicate"):
                    self.check()

    def test_links_devices_and_missing_required_source_are_rejected(self):
        for kind in (tarfile.SYMTYPE, tarfile.LNKTYPE, tarfile.FIFOTYPE):
            with self.subTest(kind=kind):
                member = tarfile.TarInfo("prismrepl/R/link")
                member.type = kind
                member.linkname = "../../outside"
                self.write([(member, b"")])
                with self.assertRaisesRegex(ValueError, "member type"):
                    self.check()
        del self.files["src/terminal.c"]
        self.write()
        with self.assertRaisesRegex(ValueError, "Missing source files"):
            self.check()

    def test_duplicate_fields_and_invalid_build_dates_are_rejected(self):
        self.files["DESCRIPTION"] = (self.metadata + self.packaged + "Version: 9.9.9\n").encode()
        self.write()
        with self.assertRaisesRegex(ValueError, "unique DCF fields"):
            self.check()
        self.files["DESCRIPTION"] = (self.metadata + self.packaged.replace("2026-11-24", "2026-99-99")).encode()
        self.write()
        with self.assertRaisesRegex(ValueError, "Invalid R CMD build timestamp"):
            self.check()

    def test_command_line_exit_status_enforces_the_gate(self):
        self.write()
        command = [sys.executable, str(CHECKER), str(self.path), "--description", str(self.description)]
        valid = subprocess.run(command, capture_output=True, text=True, check=False)
        self.assertEqual(valid.returncode, 0, valid.stderr)
        self.assertIn("PASS: prismrepl 1.2.3", valid.stdout)
        self.assertRegex(valid.stdout, r"SHA256: [0-9a-f]{64}")
        self.files["DESCRIPTION"] = self.metadata.encode()
        self.write()
        invalid = subprocess.run(command, capture_output=True, text=True, check=False)
        self.assertNotEqual(invalid.returncode, 0)
        self.assertIn("Source archive rejected", invalid.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
