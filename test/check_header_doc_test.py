#!/usr/bin/env python

########################################################################
# check_header_doc_test.py: Unit tests for check_header_doc.py
#
#  Description:
#  This script tests the header documentation consistency checker.
#  It verifies that blank lines inside the header doc block (between the
#  first and second separator lines) are detected, and that content outside
#  the header doc block is ignored. It also tests quiet mode and --all-files.
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
#    - test_check_file_ignores_blank_line_outside_header_doc:
#        Confirms that blank lines outside the header doc block are not reported.
#    - test_check_file_quiet_mode_format:
#        Confirms that quiet mode prints only "file:line".
#    - test_main_all_files_includes_non_script_extension:
#        Confirms that --all-files checks non-script extensions and reports issues.
#
#  Version History:
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
from unittest.mock import patch

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
            old_cwd = os.getcwd()
            try:
                os.chdir(d)

                fname = "note.txt"
                with open(fname, "w", encoding="utf-8") as f:
                    f.write(content)

                # Patch git ls-files to return our file
                with patch("subprocess.check_output", return_value=fname + "\n"):
                    buf = io.StringIO()
                    with contextlib.redirect_stdout(buf):
                        argv_old = sys.argv[:]
                        try:
                            sys.argv = ["check_header_doc.py", "--all-files"]
                            rc = check_header_doc.main()
                        finally:
                            sys.argv = argv_old

                    out = buf.getvalue()
                    self.assertEqual(rc, 1)
                    self.assertIn("blank line inside header doc", out)
            finally:
                os.chdir(old_cwd)


if __name__ == '__main__':
    unittest.main()
