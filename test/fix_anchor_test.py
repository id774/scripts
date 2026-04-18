#!/usr/bin/env python

########################################################################
# fix_anchor_test.py: Unit Tests for fix_anchor.py
#
#  Description:
#  This test suite validates the behavior of fix_anchor.py, which normalizes
#  HTML reference anchor placement so that anchors linked to "#ref<number>"
#  appear before Japanese full stops.
#
#  Tests focus on reference normalization, HTML entity handling, argument
#  parsing, input file validation, content validation, UTF-8 handling,
#  and file read/write behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      python test/fix_anchor_test.py
#
#  Requirements:
#  - Python Version: 3.6 or later
#
#  Test Cases:
#    - Normalize a literal Japanese full stop followed by one reference anchor
#    - Normalize consecutive reference anchors after a literal full stop
#    - Normalize HTML entity full stop followed by a reference anchor
#    - Leave text unchanged when no target pattern exists
#    - Extract script version from the header
#    - Parse INPUT only as in-place update
#    - Parse INPUT and OUTPUT correctly
#    - Parse -h/--help and -v/--version correctly
#    - Reject invalid argument counts
#    - Reject a missing input path
#    - Reject a directory path as input
#    - Reject binary input content
#    - Reject non-UTF-8 text input
#    - Reject non-target text input without anchors
#    - Accept valid HTML input content
#    - Read and write UTF-8 text files correctly
#
#  Version History:
#  v1.0 2026-03-26
#       Initial release.
#
########################################################################

import os
import sys
import tempfile
import unittest

try:
    import io
except ImportError:  # pragma: no cover
    import StringIO as io  # type: ignore


# Allow running tests from repository root or from test/ directory.
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)

import fix_anchor  # noqa: E402


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


def _write_text(path, text):
    """
    Write text to a file with UTF-8 encoding.
    """
    handle = open(path, "w", encoding="utf-8", newline="")
    try:
        handle.write(text)
    finally:
        handle.close()


def _write_binary(path, data):
    """
    Write binary data to a file.
    """
    handle = open(path, "wb")
    try:
        handle.write(data)
    finally:
        handle.close()


def _read_text(path):
    """
    Read text from a file with UTF-8 encoding.
    """
    handle = open(path, "r", encoding="utf-8")
    try:
        return handle.read()
    finally:
        handle.close()


class FixAnchorTest(unittest.TestCase):

    def test_fix_literal_full_stop_single_anchor(self):
        text = '<p>abc。<a href="#ref1">[1]</a></p>'
        updated, count = fix_anchor.fix(text)
        self.assertEqual('<p>abc<a href="#ref1">[1]</a>。</p>', updated)
        self.assertEqual(1, count)

    def test_fix_literal_full_stop_multiple_anchors(self):
        text = '<p>abc。 <a href="#ref1">[1]</a><a href="#ref2">[2]</a></p>'
        updated, count = fix_anchor.fix(text)
        self.assertEqual(
            '<p>abc<a href="#ref1">[1]</a><a href="#ref2">[2]</a>。</p>',
            updated,
        )
        self.assertEqual(1, count)

    def test_fix_entity_full_stop_single_anchor(self):
        text = '<p>abc&#12290;<a href="#ref1">[1]</a></p>'
        updated, count = fix_anchor.fix(text)
        self.assertEqual('<p>abc<a href="#ref1">[1]</a>&#12290;</p>', updated)
        self.assertEqual(1, count)

    def test_fix_no_target_pattern_keeps_text(self):
        text = '<p>abc。def</p>'
        updated, count = fix_anchor.fix(text)
        self.assertEqual(text, updated)
        self.assertEqual(0, count)

    def test_get_script_version(self):
        version = fix_anchor.get_script_version()
        self.assertTrue(version.startswith("v"))

    def test_parse_arguments_input_only(self):
        input_path, output_path, status = fix_anchor.parse_arguments(["a.html"])
        self.assertEqual("a.html", input_path)
        self.assertEqual("a.html", output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_input_output(self):
        input_path, output_path, status = fix_anchor.parse_arguments(
            ["a.html", "b.html"]
        )
        self.assertEqual("a.html", input_path)
        self.assertEqual("b.html", output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_help(self):
        input_path, output_path, status = fix_anchor.parse_arguments(["-h"])
        self.assertEqual("help", input_path)
        self.assertEqual(None, output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_version(self):
        input_path, output_path, status = fix_anchor.parse_arguments(["-v"])
        self.assertEqual("version", input_path)
        self.assertEqual(None, output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_invalid_count(self):
        with _StdCapture() as cap:
            input_path, output_path, status = fix_anchor.parse_arguments(
                ["a", "b", "c"]
            )
        self.assertEqual(None, input_path)
        self.assertEqual(None, output_path)
        self.assertEqual(1, status)
        self.assertIn("[ERROR] Invalid arguments", cap.err.getvalue())

    def test_validate_input_file_rejects_missing_path(self):
        with _StdCapture() as cap:
            status = fix_anchor.validate_input_file("/no/such/file.html")
        self.assertEqual(1, status)
        self.assertIn("Input file does not exist", cap.err.getvalue())

    def test_validate_input_file_rejects_directory(self):
        tmpdir = tempfile.mkdtemp()
        try:
            with _StdCapture() as cap:
                status = fix_anchor.validate_input_file(tmpdir)
            self.assertEqual(1, status)
            self.assertIn("Input path is not a file", cap.err.getvalue())
        finally:
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_validate_input_content_rejects_binary_file(self):
        tmpdir = tempfile.mkdtemp()
        try:
            path = os.path.join(tmpdir, "binary.dat")
            _write_binary(path, b"\x00\x01\x02\x03")
            with _StdCapture() as cap:
                status = fix_anchor.validate_input_content(path)
            self.assertEqual(1, status)
            self.assertIn("Binary file is not supported", cap.err.getvalue())
        finally:
            try:
                os.unlink(path)
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_validate_input_content_rejects_non_utf8_text(self):
        tmpdir = tempfile.mkdtemp()
        try:
            path = os.path.join(tmpdir, "non_utf8.txt")
            _write_binary(path, b"\x80\x81\x82")
            with _StdCapture() as cap:
                status = fix_anchor.validate_input_content(path)
            self.assertEqual(1, status)
            self.assertIn("Input file is not valid UTF-8 text", cap.err.getvalue())
        finally:
            try:
                os.unlink(path)
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_validate_input_content_rejects_non_target_text(self):
        tmpdir = tempfile.mkdtemp()
        try:
            path = os.path.join(tmpdir, "plain.txt")
            _write_text(path, "just a plain text file\n")
            with _StdCapture() as cap:
                status = fix_anchor.validate_input_content(path)
            self.assertEqual(1, status)
            self.assertIn("does not look like target HTML text", cap.err.getvalue())
        finally:
            try:
                os.unlink(path)
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_validate_input_content_accepts_valid_html(self):
        tmpdir = tempfile.mkdtemp()
        try:
            path = os.path.join(tmpdir, "sample.html")
            _write_text(path, '<p>abc。<a href="#ref1">[1]</a></p>\n')
            status = fix_anchor.validate_input_content(path)
            self.assertEqual(0, status)
        finally:
            try:
                os.unlink(path)
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_read_and_write_text_file(self):
        tmpdir = tempfile.mkdtemp()
        try:
            in_path = os.path.join(tmpdir, "in.html")
            out_path = os.path.join(tmpdir, "out.html")
            _write_text(in_path, '<p>abc。<a href="#ref1">[1]</a></p>\n')

            text, status = fix_anchor.read_text_file(in_path)
            self.assertEqual(0, status)
            self.assertEqual('<p>abc。<a href="#ref1">[1]</a></p>\n', text)

            updated, count = fix_anchor.fix(text)
            self.assertEqual(1, count)

            status = fix_anchor.write_text_file(out_path, updated)
            self.assertEqual(0, status)
            self.assertEqual(
                '<p>abc<a href="#ref1">[1]</a>。</p>\n',
                _read_text(out_path),
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


def main():
    """
    Run unit tests.
    """
    suite = unittest.TestLoader().loadTestsFromTestCase(FixAnchorTest)
    result = unittest.TextTestRunner(verbosity=0).run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
