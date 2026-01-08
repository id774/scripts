#!/usr/bin/env python3

########################################################################
# cal_test.py: Test for cal.py
#
#  Description:
#  This script tests cal.py, which wraps the Unix 'cal' command or
#  uses Python's calendar module as fallback. It verifies help output,
#  fallback behavior, and correct dispatch to system cal.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Shows usage and exits with code 0 when invoked with -h option
#    - Falls back to Python calendar module if 'cal' does not exist
#    - Invokes system cal with arguments if available
#
#  Version History:
#  v1.0 2025-07-07
#       Initial release.
#
########################################################################

import io
import os
import subprocess
import sys
import unittest
from contextlib import redirect_stdout
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import cal


class TestCal(unittest.TestCase):
    def run_script(self, args=None):
        script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../cal.py'))
        command = ['python3', script_path]
        if args:
            command += args
        proc = subprocess.Popen(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        out, err = proc.communicate()
        return proc.returncode, out.decode(), err.decode()

    def test_help_option(self):
        code, stdout, _ = self.run_script(['-h'])
        self.assertEqual(code, 0)
        self.assertIn('Usage', stdout)

    @patch('cal.is_unix_like', return_value=True)
    @patch('cal.command_exists', return_value=False)
    def test_fallback_to_python_calendar(self, mock_cmd, mock_unix):
        f = io.StringIO()
        with redirect_stdout(f):
            cal.main()
        output = f.getvalue()

        month_names = [
            "January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December"
        ]
        self.assertTrue(any(month in output for month in month_names))

    @patch('cal.is_unix_like', return_value=True)
    @patch('cal.command_exists', return_value=True)
    @patch('cal.get_command_path')
    @patch('cal.subprocess.call')
    def test_system_cal_with_args(self, mock_call, mock_get_path, mock_cmd, mock_unix):
        mock_get_path.return_value = '/usr/bin/cal'
        with patch.object(sys, 'argv', ['cal.py', '3', '2024']):
            cal.main()
        mock_call.assert_called_with(['cal', '3', '2024'])


if __name__ == '__main__':
    unittest.main()
