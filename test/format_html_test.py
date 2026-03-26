#!/usr/bin/env python3

########################################################################
# format_html_test.py: Unit Tests for format_html.py
#
#  Description:
#  This test suite validates the behavior of format_html.py, which reformats
#  generic HTML into a readable and stable structure with normalized
#  indentation and whitespace handling.
#
#  Tests focus on whitespace normalization, attribute formatting,
#  block and inline element rendering, input/output validation,
#  argument parsing, UTF-8 file handling, and main entry point behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      python test/format_html_test.py
#
#  Requirements:
#  - Python Version: 3.6 or later
#
#  Test Cases:
#    - Whitespace collapsing
#    - Blank line normalization
#    - Attribute escaping
#    - Attribute formatting
#    - Nested element formatting
#    - Paragraph text normalization
#    - Table cell text normalization
#    - Preformatted text preservation
#    - Void element rendering
#    - Doctype and comment rendering
#    - INPUT-only argument parsing
#    - INPUT/OUTPUT argument parsing
#    - Help and version parsing
#    - Invalid argument rejection
#    - Unknown option rejection
#    - Missing input file rejection
#    - Invalid output directory rejection
#    - UTF-8 file I/O
#    - main() stdout output
#    - main() file output
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

import format_html  # noqa: E402


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


def _read_text(path):
    """
    Read text from a file with UTF-8 encoding.
    """
    handle = open(path, "r", encoding="utf-8")
    try:
        return handle.read()
    finally:
        handle.close()


class FormatHtmlTest(unittest.TestCase):

    def test_collapse_whitespace(self):
        text = "a   b\tc\n d"
        self.assertEqual("a b c d", format_html.collapse_whitespace(text))

    def test_normalize_blank_lines(self):
        text = "\n\n<a>\n\n\n<b>\n\n"
        self.assertEqual("<a>\n\n<b>\n", format_html.normalize_blank_lines(text))

    def test_escape_attr(self):
        self.assertEqual(
            "a&amp;b&quot;c",
            format_html.escape_attr('a&b"c'),
        )

    def test_format_attrs_sorts_attributes_and_escapes_values(self):
        attrs = [("z", "2"), ("a", 'x&"y'), ("disabled", None)]
        self.assertEqual(
            ' a="x&amp;&quot;y" disabled z="2"',
            format_html.format_attrs(attrs),
        )

    def test_format_html_formats_nested_elements(self):
        text = "<div><p>Hello</p><ul><li>One</li><li>Two</li></ul></div>"
        expected = (
            "<div>\n"
            "  <p>Hello</p>\n"
            "  <ul>\n"
            "    <li>One</li>\n"
            "    <li>Two</li>\n"
            "  </ul>\n"
            "</div>\n"
        )
        self.assertEqual(expected, format_html.format_html(text))

    def test_format_html_collapses_text_only_paragraph(self):
        text = "<p> Hello   world </p>"
        self.assertEqual("<p>Hello world</p>\n", format_html.format_html(text))

    def test_format_html_collapses_text_only_table_cells(self):
        text = "<table><tr><td> A   B </td><td>X\nY</td></tr></table>"
        expected = (
            "<table>\n"
            "  <tr>\n"
            "    <td>A B</td>\n"
            "    <td>X Y</td>\n"
            "  </tr>\n"
            "</table>\n"
        )
        self.assertEqual(expected, format_html.format_html(text))

    def test_format_html_preserves_pre_content(self):
        text = "<pre>  line1\n    line2\n</pre>"
        expected = (
            "<pre>\n"
            "  line1\n"
            "    line2\n"
            "\n"
            "</pre>\n"
        )
        self.assertEqual(expected, format_html.format_html(text))

    def test_format_html_renders_void_elements(self):
        text = "<div><br><img src='x.png'></div>"
        expected = (
            "<div>\n"
            "  <br>\n"
            "  <img src=\"x.png\">\n"
            "</div>\n"
        )
        self.assertEqual(expected, format_html.format_html(text))

    def test_format_html_renders_doctype_and_comment(self):
        text = "<!DOCTYPE html><!--note--><div>x</div>"
        expected = (
            "<!DOCTYPE html>\n\n"
            "<!--note-->\n\n"
            "<div>\n"
            "  x\n"
            "</div>\n"
        )
        self.assertEqual(expected, format_html.format_html(text))

    def test_parse_args_input_only(self):
        parsed = format_html.parse_args(["format_html.py", "in.html"])
        self.assertEqual(
            {"mode": "run", "input": "in.html", "output": None},
            parsed,
        )

    def test_parse_args_input_output(self):
        parsed = format_html.parse_args(["format_html.py", "in.html", "out.html"])
        self.assertEqual(
            {"mode": "run", "input": "in.html", "output": "out.html"},
            parsed,
        )

    def test_parse_args_help(self):
        parsed = format_html.parse_args(["format_html.py", "-h"])
        self.assertEqual({"mode": "help"}, parsed)

    def test_parse_args_version(self):
        parsed = format_html.parse_args(["format_html.py", "-v"])
        self.assertEqual({"mode": "version"}, parsed)

    def test_parse_args_invalid_count(self):
        with _StdCapture():
            parsed = format_html.parse_args(["format_html.py", "a", "b", "c"])
        self.assertEqual(None, parsed)

    def test_parse_args_unknown_option(self):
        with _StdCapture():
            parsed = format_html.parse_args(["format_html.py", "--unknown"])
        self.assertEqual(None, parsed)

    def test_validate_input_file_rejects_missing_path(self):
        with _StdCapture():
            status = format_html.validate_input_file("/no/such/file.html")
        self.assertFalse(status)

    def test_validate_output_file_rejects_missing_directory(self):
        path = os.path.join("/no/such/dir", "out.html")
        with _StdCapture():
            status = format_html.validate_output_file(path)
        self.assertFalse(status)

    def test_validate_input_file_accepts_regular_file(self):
        tmpdir = tempfile.mkdtemp()
        try:
            path = os.path.join(tmpdir, "sample.html")
            _write_text(path, "<p>test</p>\n")
            status = format_html.validate_input_file(path)
            self.assertTrue(status)
        finally:
            try:
                os.unlink(path)
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_validate_output_file_accepts_existing_directory(self):
        tmpdir = tempfile.mkdtemp()
        try:
            path = os.path.join(tmpdir, "out.html")
            status = format_html.validate_output_file(path)
            self.assertTrue(status)
        finally:
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_read_and_write_text_file(self):
        tmpdir = tempfile.mkdtemp()
        try:
            in_path = os.path.join(tmpdir, "in.html")
            out_path = os.path.join(tmpdir, "out.html")
            _write_text(in_path, "<div><p>hello</p></div>")

            text = format_html.read_text(in_path)
            self.assertEqual("<div><p>hello</p></div>", text)

            formatted = format_html.format_html(text)
            format_html.write_text(out_path, formatted)

            self.assertEqual(
                "<div>\n  <p>hello</p>\n</div>\n",
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

    def test_main_writes_to_stdout(self):
        tmpdir = tempfile.mkdtemp()
        old_argv = sys.argv[:]
        try:
            in_path = os.path.join(tmpdir, "in.html")
            _write_text(in_path, "<div><p>hello</p></div>")

            sys.argv = ["format_html.py", in_path]
            with _StdCapture() as cap:
                status = format_html.main()

            self.assertEqual(0, status)
            self.assertEqual(
                "<div>\n  <p>hello</p>\n</div>\n",
                cap.out.getvalue(),
            )
        finally:
            sys.argv = old_argv
            try:
                os.unlink(in_path)
            except Exception:
                pass
            try:
                os.rmdir(tmpdir)
            except Exception:
                pass

    def test_main_writes_to_output_file(self):
        tmpdir = tempfile.mkdtemp()
        old_argv = sys.argv[:]
        try:
            in_path = os.path.join(tmpdir, "in.html")
            out_path = os.path.join(tmpdir, "out.html")
            _write_text(in_path, "<div><p>hello</p></div>")

            sys.argv = ["format_html.py", in_path, out_path]
            status = format_html.main()

            self.assertEqual(0, status)
            self.assertEqual(
                "<div>\n  <p>hello</p>\n</div>\n",
                _read_text(out_path),
            )
        finally:
            sys.argv = old_argv
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
    suite = unittest.TestLoader().loadTestsFromTestCase(FormatHtmlTest)
    result = unittest.TextTestRunner(verbosity=0).run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
