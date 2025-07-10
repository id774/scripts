#!/usr/bin/env python

########################################################################
# du_test.py: Unit tests for du.py
#
#  Description:
#  This test suite validates the disk usage reporting functionality of du.py,
#  including directory checks, output parsing, and hidden directory filtering.
#
#  Test Cases:
#  - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#  - Valid and invalid directory detection
#  - parse_du_output behavior
#  - run_custom_du includes/excludes hidden directories
#
#  Version History:
#  v1.0 2025-06-24
#      Initial release.
#
########################################################################

import os
import platform
import subprocess
import sys
import tempfile
import unittest
from io import StringIO
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from du import check_directory, parse_du_output, run_custom_du


class TestDuScript(unittest.TestCase):
    def setUp(self):
        if platform.system() != 'Darwin':
            self.skipTest("du.py is intended for macOS only")
        self.test_dir = tempfile.mkdtemp()
        self.hidden_dir = os.path.join(self.test_dir, '.hidden')
        self.visible_dir = os.path.join(self.test_dir, 'visible')
        os.mkdir(self.hidden_dir)
        os.mkdir(self.visible_dir)

    def tearDown(self):
        subprocess.call(['rm', '-rf', self.test_dir])

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'du.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_check_directory_valid(self):
        check_directory(self.test_dir)  # Should not raise

    def test_check_directory_nonexistent(self):
        stderr = sys.stderr
        sys.stderr = StringIO()
        try:
            with self.assertRaises(SystemExit) as cm:
                check_directory('/nonexistent/path')
            self.assertEqual(cm.exception.code, 1)
        finally:
            sys.stderr = stderr

    def test_check_directory_not_directory(self):
        dummy_file = os.path.join(self.test_dir, 'file.txt')
        with open(dummy_file, 'w') as f:
            f.write("data")
        stderr = sys.stderr
        sys.stderr = StringIO()
        try:
            with self.assertRaises(SystemExit) as cm:
                check_directory(dummy_file)
            self.assertEqual(cm.exception.code, 1)
        finally:
            sys.stderr = stderr

    def test_parse_du_output(self):
        fake_output = "{}\t{}\n{}\t{}".format("4.0K", self.test_dir, "8.0K", os.path.join(self.test_dir, "visible"))
        size = parse_du_output(fake_output, self.test_dir)
        self.assertEqual(size, "4.0K")

    def test_run_custom_du_includes_hidden(self):
        with patch('builtins.print') as mock_print:
            run_custom_du("1", self.test_dir, include_hidden=True)
            output = "".join(call.args[0] for call in mock_print.call_args_list)
            self.assertIn('.hidden', output)
            self.assertIn('visible', output)

    def test_run_custom_du_excludes_hidden(self):
        with patch('builtins.print') as mock_print:
            run_custom_du("1", self.test_dir, include_hidden=False)
            output = "".join(call.args[0] for call in mock_print.call_args_list)
            self.assertNotIn('.hidden', output)
            self.assertIn('visible', output)


if __name__ == '__main__':
    unittest.main()
