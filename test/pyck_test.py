#!/usr/bin/env python

########################################################################
# pyck_test.py: Unit tests for pyck.py
#
#  Description:
#  This script contains unit tests for the pyck.py script. It tests
#  the functionality of code style checks, auto-formatting, and removal
#  of unused imports for Python files. The script tests both dry-run
#  and auto-fix modes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-13
#       Initial test script for pyck.py
#
#  Usage:
#  Run this script from the command line to perform the tests.
#  Example: python test/pyck_test.py
#
########################################################################

import unittest
from unittest.mock import patch, MagicMock
import sys
import os

# Adjusting the path to import pyck from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import pyck

class TestPyck(unittest.TestCase):
    """Unit tests for the pyck.py script."""

    @patch('pyck.subprocess.run')
    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_dry_run_formatting(self, mock_print, mock_popen, mock_run):
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.dry_run_formatting('path/to/directory', 'E302,E402')

        mock_popen.assert_any_call(
            "flake8 --ignore=E302,E402 path/to/directory", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "autoflake --imports=django,requests,urllib3 --check path/to/directory", shell=True, stdout=-1)

    @patch('pyck.subprocess.run')
    @patch('pyck.print')
    def test_execute_formatting(self, mock_print, mock_run):
        pyck.execute_formatting('path/to/file.py', 'E302,E402')

        mock_run.assert_any_call(
            "autoflake --imports=django,requests,urllib3 -i path/to/file.py", shell=True)
        mock_run.assert_any_call(
            "autopep8 --ignore=E302,E402 -v -i path/to/file.py", shell=True)


if __name__ == '__main__':
    unittest.main()
