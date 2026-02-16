#!/usr/bin/env python

########################################################################
# erase_history_test.py: Unit Tests for erase_history.py
#
#  Description:
#  This test suite validates the behavior of erase_history.py, which removes
#  the last N history lines from ~/.zsh_history with optional confirmation and
#  quiet mode. Tests focus on argument parsing, safety limits, confirmation
#  handling, output formatting, and atomic file updates.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      python test/erase_history_test.py
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Test Cases:
#    - Parse default args as (n=1, quiet=False)
#    - Parse shorthand "-2" as (n=2, quiet=False)
#    - Parse "-n 2" as (n=2, quiet=False)
#    - Parse "-q" as (n=1, quiet=True)
#    - Parse "-q -2" as (n=2, quiet=True)
#    - Reject invalid N (<=0) and too-large N (> MAX_REMOVE_LINES)
#    - In quiet mode, delete without prompting and produce no output
#    - In non-quiet mode, show "Will remove" lines and delete only on y/yes
#    - Abort on non-y/yes and keep the history file unchanged
#    - Keep erase_history invocation in history when it is the last entry
#    - Detect self invocation in EXTENDED_HISTORY format and keep it
#    - Delete the last line normally when the last entry is not self invocation
#
#  Version History:
#  v1.1 2026-02-15
#       Add tests to ensure erase_history invocation is preserved when it is
#       the last history entry and preceding lines are removed instead.
#  v1.0 2026-01-10
#       Initial release.
#
########################################################################

import os
import sys
import tempfile
import unittest

try:
    import builtins
except ImportError:  # pragma: no cover (Python 2 fallback, not used)
    import __builtin__ as builtins  # type: ignore

try:
    import io
except ImportError:  # pragma: no cover
    import StringIO as io  # type: ignore


# Allow running tests from repository root or from test/ directory.
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)

import erase_history  # noqa: E402


class _StdCapture(object):
    """
    Capture stdout/stderr for a context.
    """

    def __init__(self):
        self._old_out = None
        self._old_err = None
        self.out = None
        self.err = None

    def __enter__(self):
        self._old_out = sys.stdout
        self._old_err = sys.stderr
        self.out = io.StringIO()
        self.err = io.StringIO()
        sys.stdout = self.out
        sys.stderr = self.err
        return self

    def __exit__(self, exc_type, exc, tb):
        sys.stdout = self._old_out
        sys.stderr = self._old_err
        return False


class _InputPatch(object):
    """
    Patch builtins.input to return a fixed string.
    """

    def __init__(self, value):
        self._value = value
        self._old_input = None

    def __enter__(self):
        self._old_input = getattr(builtins, "input")
        setattr(builtins, "input", lambda prompt="": self._value)
        return self

    def __exit__(self, exc_type, exc, tb):
        setattr(builtins, "input", self._old_input)
        return False


def _write_file(path, lines):
    """
    Write lines to a file with UTF-8 encoding.
    """
    f = open(path, "w", encoding="utf-8", errors="replace")
    try:
        for line in lines:
            f.write(line)
    finally:
        f.close()


def _read_file(path):
    """
    Read entire file contents with UTF-8 encoding.
    """
    f = open(path, "r", encoding="utf-8", errors="replace")
    try:
        return f.read()
    finally:
        f.close()


class EraseHistoryTest(unittest.TestCase):

    def test_parse_args_default(self):
        n, quiet = erase_history.parse_args([])
        self.assertEqual(1, n)
        self.assertEqual(False, quiet)

    def test_parse_args_shorthand(self):
        n, quiet = erase_history.parse_args(["-2"])
        self.assertEqual(2, n)
        self.assertEqual(False, quiet)

    def test_parse_args_n_option(self):
        n, quiet = erase_history.parse_args(["-n", "2"])
        self.assertEqual(2, n)
        self.assertEqual(False, quiet)

    def test_parse_args_quiet(self):
        n, quiet = erase_history.parse_args(["-q"])
        self.assertEqual(1, n)
        self.assertEqual(True, quiet)

    def test_parse_args_quiet_shorthand(self):
        n, quiet = erase_history.parse_args(["-q", "-2"])
        self.assertEqual(2, n)
        self.assertEqual(True, quiet)

    def test_validate_n_rejects_non_positive(self):
        with _StdCapture() as cap:
            try:
                erase_history.validate_n(0)
                self.fail("Expected SystemExit")
            except SystemExit as e:
                self.assertEqual(2, e.code)
        self.assertIn("N must be >= 1", cap.err.getvalue())

    def test_validate_n_rejects_too_large(self):
        with _StdCapture() as cap:
            try:
                erase_history.validate_n(erase_history.MAX_REMOVE_LINES + 1)
                self.fail("Expected SystemExit")
            except SystemExit as e:
                self.assertEqual(2, e.code)
        self.assertIn("N must be <=", cap.err.getvalue())

    def test_erase_quiet_deletes_without_output(self):
        tmpdir = tempfile.mkdtemp()
        try:
            history_path = os.path.join(tmpdir, ".zsh_history")
            _write_file(history_path, ["one\n", "two\n", "three\n"])

            with _StdCapture() as cap:
                erase_history.erase_tail_lines(history_path, 1, True)

            self.assertEqual("", cap.out.getvalue())
            self.assertEqual("", cap.err.getvalue())
            self.assertEqual("one\ntwo\n", _read_file(history_path))
        finally:
            try:
                for fn in os.listdir(tmpdir):
                    os.unlink(os.path.join(tmpdir, fn))
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_erase_nonquiet_prompts_and_deletes_on_yes(self):
        tmpdir = tempfile.mkdtemp()
        try:
            history_path = os.path.join(tmpdir, ".zsh_history")
            _write_file(history_path, ["one\n", "two\n", "three\n"])

            with _InputPatch("y"), _StdCapture() as cap:
                erase_history.erase_tail_lines(history_path, 2, False)

            out = cap.out.getvalue()
            self.assertIn("[INFO] Will remove line: two", out)
            self.assertIn("[INFO] Will remove line: three", out)
            self.assertIn("[INFO] Deleted", out)
            self.assertEqual("one\n", _read_file(history_path))
        finally:
            try:
                for fn in os.listdir(tmpdir):
                    os.unlink(os.path.join(tmpdir, fn))
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_erase_nonquiet_aborts_on_non_yes_and_keeps_file(self):
        tmpdir = tempfile.mkdtemp()
        try:
            history_path = os.path.join(tmpdir, ".zsh_history")
            original_lines = ["one\n", "two\n", "three\n"]
            _write_file(history_path, original_lines)

            with _InputPatch("no"), _StdCapture() as cap:
                try:
                    erase_history.erase_tail_lines(history_path, 1, False)
                    self.fail("Expected SystemExit")
                except SystemExit as e:
                    self.assertEqual(1, e.code)

            out = cap.out.getvalue()
            self.assertIn("[INFO] Will remove line: three", out)
            self.assertIn("[INFO] Aborted", out)
            self.assertEqual("".join(original_lines), _read_file(history_path))
        finally:
            try:
                for fn in os.listdir(tmpdir):
                    os.unlink(os.path.join(tmpdir, fn))
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_keep_self_invocation_and_delete_preceding(self):
        tmpdir = tempfile.mkdtemp()
        try:
            history_path = os.path.join(tmpdir, ".zsh_history")
            _write_file(history_path, [
                "cmd1\n",
                "cmd2\n",
                "erase_history.py\n",
            ])

            erase_history.erase_tail_lines(history_path, 1, True)

            # Remove the preceding line and keep the self invocation
            self.assertEqual("cmd1\nerase_history.py\n", _read_file(history_path))
        finally:
            try:
                for fn in os.listdir(tmpdir):
                    os.unlink(os.path.join(tmpdir, fn))
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_keep_self_invocation_extended_history_format(self):
        tmpdir = tempfile.mkdtemp()
        try:
            history_path = os.path.join(tmpdir, ".zsh_history")
            _write_file(history_path, [
                ": 1700000000:0;cmd1\n",
                ": 1700000001:0;cmd2\n",
                ": 1700000002:0;erase_history.py\n",
            ])

            erase_history.erase_tail_lines(history_path, 1, True)

            self.assertEqual(
                ": 1700000000:0;cmd1\n: 1700000002:0;erase_history.py\n",
                _read_file(history_path),
            )
        finally:
            try:
                for fn in os.listdir(tmpdir):
                    os.unlink(os.path.join(tmpdir, fn))
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_delete_last_when_not_self_invocation(self):
        tmpdir = tempfile.mkdtemp()
        try:
            history_path = os.path.join(tmpdir, ".zsh_history")
            _write_file(history_path, [
                "cmd1\n",
                "cmd2\n",
                "othercmd\n",
            ])

            erase_history.erase_tail_lines(history_path, 1, True)

            # Remove the last line normally
            self.assertEqual("cmd1\ncmd2\n", _read_file(history_path))
        finally:
            try:
                for fn in os.listdir(tmpdir):
                    os.unlink(os.path.join(tmpdir, fn))
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass


def main():
    """
    Run unit tests.
    """
    suite = unittest.TestLoader().loadTestsFromTestCase(EraseHistoryTest)
    result = unittest.TextTestRunner(verbosity=0).run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
