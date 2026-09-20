#!/usr/bin/env python

########################################################################
# userlist_test.py: Test for userlist.py
#
#  Description:
#  This script tests userlist.py, which displays user accounts with UIDs
#  above a system-specific threshold. It mocks different OS environments
#  and simulates expected input/output behavior.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Shows usage and exits with code 0 when invoked with -h option
#    - Displays user with UID >= 1000 on Debian-based system
#    - Displays user from dscacheutil on macOS if UID >= 500
#    - Returns 0 from main() on successful Debian /etc/passwd retrieval
#    - Returns 0 from main() on successful macOS dscacheutil retrieval
#    - Returns 1 from main() and reports the existing error text on stderr
#      when /etc/passwd cannot be read on a Debian-based system
#    - Returns 1 from main() and reports the existing error text on stderr
#      when dscacheutil fails on macOS
#
#  Version History:
#  v1.2 2026-09-20
#       Cover platform user-source failures and propagated status.
#  v1.1 2025-07-08
#       Fixed compatibility issues with Python 3.4.
#  v1.0 2025-07-07
#       Initial release.
#
########################################################################

import io
import os
import subprocess
import sys
import unittest
from collections import namedtuple
from contextlib import redirect_stderr, redirect_stdout
from unittest.mock import mock_open, patch

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import userlist


class TestUserlist(unittest.TestCase):
    script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../userlist.py'))

    def run_script(self, args=None, input_data=None):
        command = ['python', self.script_path]
        if args:
            command += args
        process = subprocess.Popen(
            command,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        stdout_data, stderr_data = process.communicate(input=input_data.encode('utf-8') if input_data else None)

        Result = namedtuple('Result', ['returncode', 'stdout', 'stderr'])
        return Result(
            returncode=process.returncode,
            stdout=stdout_data.decode('utf-8'),
            stderr=stderr_data.decode('utf-8')
        )

    def test_help_option(self):
        result = self.run_script(['-h'])
        self.assertEqual(result.returncode, 0)
        self.assertIn('Usage', result.stdout)

    @patch('os.path.isfile', side_effect=lambda path: path == '/etc/debian_version')
    @patch('platform.system', return_value='Linux')
    def test_debian_userlist(self, mock_system, mock_isfile):
        passwd_data = "testuser:x:1000:1000::/home/testuser:/bin/bash\n"
        m = mock_open(read_data=passwd_data)
        m.return_value.__iter__.return_value = passwd_data.splitlines()
        with patch('builtins.open', m):
            f = io.StringIO()
            with redirect_stdout(f):
                rc = userlist.main()
            output = f.getvalue()
            self.assertIn("testuser", output)
            self.assertEqual(rc, 0)

    @patch('platform.system', return_value='Darwin')
    @patch('subprocess.check_output', return_value=b'name: testmac\nuid: 501\n')
    @patch('os.path.isfile', return_value=False)
    def test_macos_userlist(self, mock_isfile, mock_check_output, mock_platform):
        f = io.StringIO()
        with redirect_stdout(f):
            rc = userlist.main()
        output = f.getvalue()
        self.assertIn("testmac", output)
        self.assertEqual(rc, 0)

    @patch('os.path.isfile', side_effect=lambda path: path == '/etc/debian_version')
    @patch('platform.system', return_value='Linux')
    def test_debian_userlist_read_failure(self, mock_system, mock_isfile):
        with patch('builtins.open', side_effect=OSError("Permission denied")):
            buf_err = io.StringIO()
            with redirect_stderr(buf_err):
                rc = userlist.main()
            self.assertEqual(rc, 1)
            self.assertIn("Error reading /etc/passwd:", buf_err.getvalue())

    @patch('platform.system', return_value='Darwin')
    @patch('subprocess.check_output', side_effect=OSError("dscacheutil not found"))
    @patch('os.path.isfile', return_value=False)
    def test_macos_userlist_failure(self, mock_isfile, mock_check_output, mock_platform):
        buf_err = io.StringIO()
        with redirect_stderr(buf_err):
            rc = userlist.main()
        self.assertEqual(rc, 1)
        self.assertIn("Error retrieving user list:", buf_err.getvalue())


if __name__ == '__main__':
    unittest.main()
