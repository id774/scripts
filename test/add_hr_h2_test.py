#!/usr/bin/env python

########################################################################
# add_hr_h2_test.py: Unit Tests for add_hr_h2.py
#
#  Description:
#  This test suite validates the behavior of add_hr_h2.py, which inserts
#  an <hr> tag immediately before each <h2> tag unless an <hr> tag is
#  already present just before that <h2> block.
#
#  Tests focus on <hr> insertion, indentation preservation, existing
#  <hr> detection, newline style detection, argument parsing, input file
#  validation, UTF-8 handling, and file read/write behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      python test/add_hr_h2_test.py
#
#  Requirements:
#  - Python Version: 3.6 or later
#
#  Test Cases:
#    - Insert <hr> before a simple <h2> tag
#    - Preserve indentation when inserting <hr>
#    - Leave text unchanged when <hr> already exists before <h2>
#    - Insert <hr> before multiple <h2> tags
#    - Leave text unchanged when no <h2> tag exists
#    - Detect newline style correctly
#    - Detect an existing <hr> immediately before <h2>
#    - Extract script version from the header
#    - Parse INPUT only as in-place update
#    - Parse INPUT and OUTPUT correctly
#    - Parse -h/--help and -v/--version correctly
#    - Reject invalid argument counts
#    - Reject a missing input path
#    - Reject a directory path as input
#    - Reject binary input content
#    - Reject non-UTF-8 text input
#    - Reject non-target text input without <h2>
#    - Accept valid HTML input content
#    - Read and write UTF-8 text files correctly
#
#  Version History:
#  v1.0 2026-04-18
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

import add_hr_h2  # noqa: E402


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


class AddHrH2Test(unittest.TestCase):

    def test_add_hr_before_simple_h2(self):
        text = "<h2>Title</h2>\n"
        updated, count = add_hr_h2.add_hr_before_h2(text)
        self.assertEqual("<hr>\n<h2>Title</h2>\n", updated)
        self.assertEqual(1, count)

    def test_add_hr_preserves_indentation(self):
        text = '    <section class="py-3" id="about">\n      <h2>About Me</h2>\n'
        updated, count = add_hr_h2.add_hr_before_h2(text)
        self.assertEqual(
            '    <section class="py-3" id="about">\n'
            '      <hr>\n'
            '      <h2>About Me</h2>\n',
            updated,
        )
        self.assertEqual(1, count)

    def test_add_hr_keeps_existing_hr_before_h2(self):
        text = "<p>text</p>\n\n<hr>\n<h2>Title</h2>\n"
        updated, count = add_hr_h2.add_hr_before_h2(text)
        self.assertEqual(text, updated)
        self.assertEqual(0, count)

    def test_add_hr_multiple_h2_tags(self):
        text = (
            "<h2>One</h2>\n"
            "<p>text</p>\n"
            "  <h2>Two</h2>\n"
        )
        updated, count = add_hr_h2.add_hr_before_h2(text)
        self.assertEqual(
            "<hr>\n"
            "<h2>One</h2>\n"
            "<p>text</p>\n"
            "  <hr>\n"
            "  <h2>Two</h2>\n",
            updated,
        )
        self.assertEqual(2, count)

    def test_add_hr_no_h2_keeps_text(self):
        text = "<p>abc</p>\n"
        updated, count = add_hr_h2.add_hr_before_h2(text)
        self.assertEqual(text, updated)
        self.assertEqual(0, count)

    def test_detect_newline_lf(self):
        self.assertEqual("\n", add_hr_h2.detect_newline("a\nb\n"))

    def test_detect_newline_crlf(self):
        self.assertEqual("\r\n", add_hr_h2.detect_newline("a\r\nb\r\n"))

    def test_detect_newline_cr(self):
        self.assertEqual("\r", add_hr_h2.detect_newline("a\rb\r"))

    def test_has_hr_immediately_before_true(self):
        text = "<p>abc</p>\n<hr>\n<h2>Title</h2>\n"
        h2_start = text.index("<h2>")
        self.assertTrue(add_hr_h2.has_hr_immediately_before(text, h2_start))

    def test_has_hr_immediately_before_false(self):
        text = "<p>abc</p>\n<h2>Title</h2>\n"
        h2_start = text.index("<h2>")
        self.assertFalse(add_hr_h2.has_hr_immediately_before(text, h2_start))

    def test_get_script_version(self):
        version = add_hr_h2.get_script_version()
        self.assertTrue(version.startswith("v"))

    def test_parse_arguments_input_only(self):
        input_path, output_path, status = add_hr_h2.parse_arguments(["a.html"])
        self.assertEqual("a.html", input_path)
        self.assertEqual("a.html", output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_input_output(self):
        input_path, output_path, status = add_hr_h2.parse_arguments(
            ["a.html", "b.html"]
        )
        self.assertEqual("a.html", input_path)
        self.assertEqual("b.html", output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_help(self):
        input_path, output_path, status = add_hr_h2.parse_arguments(["-h"])
        self.assertEqual("help", input_path)
        self.assertEqual(None, output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_version(self):
        input_path, output_path, status = add_hr_h2.parse_arguments(["-v"])
        self.assertEqual("version", input_path)
        self.assertEqual(None, output_path)
        self.assertEqual(0, status)

    def test_parse_arguments_invalid_count(self):
        with _StdCapture() as cap:
            input_path, output_path, status = add_hr_h2.parse_arguments(
                ["a", "b", "c"]
            )
        self.assertEqual(None, input_path)
        self.assertEqual(None, output_path)
        self.assertEqual(1, status)
        self.assertIn("[ERROR] Invalid arguments", cap.err.getvalue())

    def test_validate_input_file_rejects_missing_path(self):
        with _StdCapture() as cap:
            status = add_hr_h2.validate_input_file("/no/such/file.html")
        self.assertEqual(1, status)
        self.assertIn("Input file does not exist", cap.err.getvalue())

    def test_validate_input_file_rejects_directory(self):
        tmpdir = tempfile.mkdtemp()
        try:
            with _StdCapture() as cap:
                status = add_hr_h2.validate_input_file(tmpdir)
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
                status = add_hr_h2.validate_input_content(path)
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
                status = add_hr_h2.validate_input_content(path)
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
                status = add_hr_h2.validate_input_content(path)
            self.assertEqual(1, status)
            self.assertIn("does not contain any <h2> tag", cap.err.getvalue())
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
            _write_text(path, "<p>abc</p>\n<h2>Title</h2>\n")
            status = add_hr_h2.validate_input_content(path)
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
            _write_text(in_path, "<p>abc</p>\n<h2>Title</h2>\n")

            text, status = add_hr_h2.read_text_file(in_path)
            self.assertEqual(0, status)
            self.assertEqual("<p>abc</p>\n<h2>Title</h2>\n", text)

            updated, count = add_hr_h2.add_hr_before_h2(text)
            self.assertEqual(1, count)

            status = add_hr_h2.write_text_file(out_path, updated)
            self.assertEqual(0, status)
            self.assertEqual(
                "<p>abc</p>\n<hr>\n<h2>Title</h2>\n",
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
    suite = unittest.TestLoader().loadTestsFromTestCase(AddHrH2Test)
    result = unittest.TextTestRunner(verbosity=0).run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
