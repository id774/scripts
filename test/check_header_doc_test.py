#!/usr/bin/env python

########################################################################
# check_header_doc_test.py: Unit tests for check_header_doc.py
#
#  Description:
#  This script tests the header documentation consistency checker.
#  It verifies that blank lines inside the header doc block (between the
#  first and second separator lines) are detected, that typo lines like "##"
#  inside the header doc block are detected, and that content outside
#  the header doc block is ignored. It also tests quiet mode, --all-files,
#  and directory scanning via --root.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - test_usage_shows_help:
#        Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - test_check_file_detects_blank_line_in_header_doc:
#        Confirms that a blank line inside the header doc block is reported.
#    - test_check_file_detects_typo_blank_comment_line_in_header_doc:
#        Confirms that a typo line like "##" inside the header doc block is reported.
#    - test_check_file_detects_non_comment_line_in_header_doc:
#        Confirms that a non-comment line inside the header doc block is reported.
#    - test_check_file_ignores_blank_line_outside_header_doc:
#        Confirms that blank lines outside the header doc block are not reported.
#    - test_check_file_quiet_mode_format:
#        Confirms that quiet mode prints only "file:line".
#    - test_main_all_files_includes_non_script_extension:
#        Confirms that --all-files checks non-script extensions under --root and reports issues.
#    - test_main_invalid_root_directory:
#        Confirms that specifying a non-existent --root directory results in an error exit.
#    - test_directory_scan_without_vcs_dependency:
#        Confirms that files are scanned via filesystem traversal under --root without relying on Git.
#    - test_check_file_propagates_read_error:
#        Confirms that check_file() propagates OSError instead of returning [] silently.
#    - test_looks_like_script_propagates_read_error:
#        Confirms that looks_like_script() propagates OSError for an unreadable extensionless file.
#    - test_main_reports_unreadable_file_and_continues:
#        Confirms that main() reports an unreadable file to stderr, returns 2, and still
#        scans the remaining readable files.
#    - test_main_read_error_takes_precedence_over_violation:
#        Confirms that a read error yields status 2 even when a violation is also found.
#    - test_main_violation_only_returns_1:
#        Confirms that a violation-only scan (no read errors) still returns 1.
#    - test_main_clean_scan_returns_0:
#        Confirms that a clean scan with no read errors and no violations returns 0.
#
#  Version History:
#  v1.3 2026-09-20
#       Cover unreadable-file scan failures and continued scanning.
#  v1.2 2026-01-12
#       Add test for detecting non-comment lines inside header doc block.
#  v1.1 2026-01-10
#       Add test for detecting typo blank-comment lines like "##" inside header doc block.
#  v1.0 2026-01-02
#       Initial release.
#
########################################################################

import os
import sys

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import contextlib
import io
import subprocess
import tempfile
import unittest
from unittest.mock import patch

import check_header_doc


class TestCheckHeaderDoc(unittest.TestCase):
    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'check_header_doc.py')

        proc = subprocess.Popen(['python', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_check_file_detects_blank_line_in_header_doc(self):
        content = (
            "#!/bin/sh\n"
            "\n"
            "########################################################################\n"
            "# test: header\n"
            "#\n"
            "\n"  # missing "#"
            "# after\n"
            "########################################################################\n"
            "echo ok\n"
        )

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "t.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)

            hits = check_header_doc.check_file(path, quiet_mode=False)
            self.assertEqual(len(hits), 1)
            self.assertIn("blank line inside header doc", hits[0])

    def test_check_file_detects_typo_blank_comment_line_in_header_doc(self):
        content = (
            "#!/bin/sh\n"
            "\n"
            "########################################################################\n"
            "# test: header\n"
            "#\n"
            "##\n"  # typo: should be "#"
            "# after\n"
            "########################################################################\n"
            "echo ok\n"
        )

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "t.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)

            hits = check_header_doc.check_file(path, quiet_mode=False)
            self.assertEqual(len(hits), 1)
            self.assertIn("invalid blank comment line inside header doc", hits[0])

    def test_check_file_detects_non_comment_line_in_header_doc(self):
        content = (
            "#!/bin/sh\n"
            "\n"
            "########################################################################\n"
            "# test: header\n"
            "#\n"
            "oops\n"  # missing "#"
            "# after\n"
            "########################################################################\n"
            "echo ok\n"
        )

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "t.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)

            hits = check_header_doc.check_file(path, quiet_mode=False)
            self.assertEqual(len(hits), 1)
            self.assertIn("non-comment line inside header doc", hits[0])

    def test_check_file_ignores_blank_line_outside_header_doc(self):
        content = (
            "#!/bin/sh\n"
            "########################################################################\n"
            "# test: header\n"
            "#\n"
            "# ok\n"
            "########################################################################\n"
            "\n"  # outside header doc
            "echo ok\n"
        )

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "t.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)

            hits = check_header_doc.check_file(path, quiet_mode=False)
            self.assertEqual(hits, [])

    def test_check_file_quiet_mode_format(self):
        content = (
            "#!/bin/sh\n"
            "########################################################################\n"
            "# test: header\n"
            "\n"  # missing "#"
            "# ok\n"
            "########################################################################\n"
        )

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "t.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)

            hits = check_header_doc.check_file(path, quiet_mode=True)
            self.assertEqual(len(hits), 1)
            self.assertRegex(hits[0], r".+:\d+$")

    def test_main_all_files_includes_non_script_extension(self):
        content = (
            "########################################################################\n"
            "# test: header\n"
            "\n"  # missing "#"
            "# ok\n"
            "########################################################################\n"
        )

        with tempfile.TemporaryDirectory() as d:
            fname = os.path.join(d, "note.txt")
            with open(fname, "w", encoding="utf-8") as f:
                f.write(content)

            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                argv_old = sys.argv[:]
                try:
                    sys.argv = ["check_header_doc.py", "--all-files", "--root", d]
                    rc = check_header_doc.main()
                finally:
                    sys.argv = argv_old

            out = buf.getvalue()
            self.assertEqual(rc, 1)
            self.assertIn("blank line inside header doc", out)

    def test_main_invalid_root_directory(self):
        buf_out = io.StringIO()
        buf_err = io.StringIO()

        with contextlib.redirect_stdout(buf_out), contextlib.redirect_stderr(buf_err):
            argv_old = sys.argv[:]
            try:
                sys.argv = ["check_header_doc.py", "--root", "/path/does/not/exist"]
                rc = check_header_doc.main()
            finally:
                sys.argv = argv_old

        self.assertEqual(rc, 2)
        self.assertIn("root directory not found", buf_err.getvalue())

    def test_directory_scan_without_vcs_dependency(self):
        content = (
            "#!/usr/bin/env python\n"
            "########################################################################\n"
            "# test: header\n"
            "\n"  # missing "#"
            "# ok\n"
            "########################################################################\n"
            "print('ok')\n"
        )

        with tempfile.TemporaryDirectory() as d:
            fname = os.path.join(d, "script")  # no extension
            with open(fname, "w", encoding="utf-8") as f:
                f.write(content)

            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                argv_old = sys.argv[:]
                try:
                    sys.argv = ["check_header_doc.py", "--root", d]
                    rc = check_header_doc.main()
                finally:
                    sys.argv = argv_old

            out = buf.getvalue()
            self.assertEqual(rc, 1)
            self.assertIn("blank line inside header doc", out)

    def test_check_file_propagates_read_error(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "unreadable.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write("#!/bin/sh\necho ok\n")

            with patch("builtins.open", side_effect=OSError("Permission denied")):
                with self.assertRaises(OSError):
                    check_header_doc.check_file(path, quiet_mode=False)

    def test_looks_like_script_propagates_read_error(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "extensionless_script")
            with open(path, "w", encoding="utf-8") as f:
                f.write("#!/bin/sh\necho ok\n")

            with patch("builtins.open", side_effect=OSError("Permission denied")):
                with self.assertRaises(OSError):
                    check_header_doc.looks_like_script(path)

    def test_main_reports_unreadable_file_and_continues(self):
        clean_content = (
            "#!/bin/sh\n"
            "########################################################################\n"
            "# ok\n"
            "########################################################################\n"
            "echo ok\n"
        )

        with tempfile.TemporaryDirectory() as d:
            good_path = os.path.join(d, "good.sh")
            bad_path = os.path.join(d, "bad.sh")
            with open(good_path, "w", encoding="utf-8") as f:
                f.write(clean_content)
            with open(bad_path, "w", encoding="utf-8") as f:
                f.write(clean_content)

            real_open = open

            def fake_open(path, *args, **kwargs):
                if os.path.abspath(path) == os.path.abspath(bad_path):
                    raise OSError("Permission denied")
                return real_open(path, *args, **kwargs)

            buf_out = io.StringIO()
            buf_err = io.StringIO()
            with patch("builtins.open", side_effect=fake_open):
                with contextlib.redirect_stdout(buf_out), contextlib.redirect_stderr(buf_err):
                    argv_old = sys.argv[:]
                    try:
                        sys.argv = ["check_header_doc.py", "--root", d]
                        rc = check_header_doc.main()
                    finally:
                        sys.argv = argv_old

            self.assertEqual(rc, 2)
            err_out = buf_err.getvalue()
            self.assertIn("[ERROR] Cannot read file:", err_out)
            self.assertIn(bad_path, err_out)
            # The good file's read error is not raised; the scan continues.
            self.assertNotIn(good_path, err_out)

    def test_main_read_error_takes_precedence_over_violation(self):
        violation_content = (
            "#!/bin/sh\n"
            "########################################################################\n"
            "# ok\n"
            "\n"  # missing "#"
            "# after\n"
            "########################################################################\n"
        )

        with tempfile.TemporaryDirectory() as d:
            violating_path = os.path.join(d, "violating.sh")
            bad_path = os.path.join(d, "bad.sh")
            with open(violating_path, "w", encoding="utf-8") as f:
                f.write(violation_content)
            with open(bad_path, "w", encoding="utf-8") as f:
                f.write(violation_content)

            real_open = open

            def fake_open(path, *args, **kwargs):
                if os.path.abspath(path) == os.path.abspath(bad_path):
                    raise OSError("Permission denied")
                return real_open(path, *args, **kwargs)

            buf_out = io.StringIO()
            buf_err = io.StringIO()
            with patch("builtins.open", side_effect=fake_open):
                with contextlib.redirect_stdout(buf_out), contextlib.redirect_stderr(buf_err):
                    argv_old = sys.argv[:]
                    try:
                        sys.argv = ["check_header_doc.py", "--root", d]
                        rc = check_header_doc.main()
                    finally:
                        sys.argv = argv_old

            self.assertEqual(rc, 2)
            self.assertIn("blank line inside header doc", buf_out.getvalue())
            self.assertIn("[ERROR] Cannot read file:", buf_err.getvalue())

    def test_main_violation_only_returns_1(self):
        content = (
            "#!/bin/sh\n"
            "########################################################################\n"
            "# ok\n"
            "\n"  # missing "#"
            "# after\n"
            "########################################################################\n"
        )

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "violating.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)

            argv_old = sys.argv[:]
            try:
                sys.argv = ["check_header_doc.py", "--root", d]
                with contextlib.redirect_stdout(io.StringIO()):
                    rc = check_header_doc.main()
            finally:
                sys.argv = argv_old

            self.assertEqual(rc, 1)

    def test_main_clean_scan_returns_0(self):
        content = (
            "#!/bin/sh\n"
            "########################################################################\n"
            "# ok\n"
            "########################################################################\n"
            "echo ok\n"
        )

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "clean.sh")
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)

            argv_old = sys.argv[:]
            try:
                sys.argv = ["check_header_doc.py", "--root", d]
                with contextlib.redirect_stdout(io.StringIO()):
                    rc = check_header_doc.main()
            finally:
                sys.argv = argv_old

            self.assertEqual(rc, 0)


if __name__ == '__main__':
    unittest.main()
