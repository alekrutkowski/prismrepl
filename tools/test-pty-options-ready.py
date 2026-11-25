"""Verify prompt synchronization against representative native terminal frames.

This standard-library test loads only the pure VT model and wait_prompt
function. It never starts R or opens a terminal.
"""
import ast
import codecs
from pathlib import Path
import unicodedata
import unittest


TOOLS = Path(__file__).resolve().parent


def load_definition(path, name, namespace):
    node = next(node for node in ast.parse(path.read_text(encoding="utf-8")).body
                if isinstance(node, (ast.ClassDef, ast.FunctionDef)) and node.name == name)
    exec(compile(ast.Module(body=[node], type_ignores=[]), str(path), "exec"), namespace)
    return namespace[name]


Screen = load_definition(TOOLS / "pty-fullscreen.py", "Screen",
                         {"codecs": codecs, "unicodedata": unicodedata})


def frame(prompt, controls=b"", title="PRISM REPL"):
    return (controls + b"\x1b[2J\x1b[H " + title.encode() + b"\x1b[3;1H " +
            prompt.encode() + b"\x1b[?25h")


class PromptReadinessTests(unittest.TestCase):
    def observed_readiness(self, frames, staged=""):
        model, raw, results = Screen(6, 100), bytearray(), []

        def wait_until(check, description):
            for chunk in frames:
                raw.extend(chunk)
                model.feed(chunk)
                results.append(check())
                if results[-1]:
                    return
            raise AssertionError(description)

        wait_prompt = load_definition(TOOLS / "pty-options.py", "wait_prompt",
                                      {"model": model, "raw": raw, "wait_until": wait_until})
        wait_prompt(staged)
        return results

    def test_parent_repaint_does_not_mean_the_selected_command_finished(self):
        frames = [
            frame("r> :unbind", b"\x1b[?2004h"),
            frame("newline", title="Disable a shortcut action"),
            frame("r>", b"\x1b[?2004l"),
            b"\x1b[?2004h",
        ]
        self.assertEqual(self.observed_readiness(frames), [False, False, False, True])

    def test_pending_text_does_not_count_as_an_empty_prompt(self):
        self.assertEqual(self.observed_readiness([
            frame("r> :bind newline F4", b"\x1b[?2004h"),
            frame("r>"),
        ]), [False, True])

    def test_staged_prompt_must_match_the_complete_expected_text(self):
        self.assertEqual(self.observed_readiness([
            frame("r> :bind newline F4", b"\x1b[?2004h"),
            frame("r> :bind newline "),
        ], staged=":bind newline"), [False, True])

    def test_closed_input_and_incomplete_reopen_never_count_as_ready(self):
        self.assertEqual(self.observed_readiness([
            frame("r>", b"\x1b[?2004h\x1b[?2004l"),
            b"\x1b[?2004",
            b"h",
        ]), [False, False, True])
        with self.assertRaisesRegex(AssertionError, "editable prompt"):
            self.observed_readiness([frame("r>", b"\x1b[?2004h\x1b[?2004l")])


if __name__ == "__main__":
    unittest.main(verbosity=2)
