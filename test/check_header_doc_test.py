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
#  Author: id774 (More info: http://id774.net)
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
#
#  Version History:
#  v1.2 2026-01-12
#       Add test for detecting non-comment lines inside header doc block.
#  v1.1 2026-01-10
#       Add test for detecting typo blank-comment lines like "##" inside header doc block.
#  v1.0 2026-01-02
#       Initial test implementation for check_header_doc.py.
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

import check_header_doc


class TestCheckHeaderDoc(unittest.TestCase):
    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'check_header_doc.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
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


if __name__ == '__main__':
    unittest.main()
