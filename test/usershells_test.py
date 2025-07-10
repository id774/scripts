#!/usr/bin/env python3

########################################################################
# usershells_test.py: Test for usershells.py
#
#  Description:
#  This script tests usershells.py, which lists user accounts and their
#  login shells, excluding non-interactive accounts. It mocks both
#  macOS (dscl) and Unix-like (/etc/passwd) environments.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#  - Shows usage and exits with code 0 when invoked with -h option
#  - Lists users with interactive shells from /etc/passwd (Linux)
#  - Lists users with interactive shells from dscl (macOS)
#
#  Version History:
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
from contextlib import redirect_stdout
from unittest.mock import mock_open, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import usershells


class TestUsershells(unittest.TestCase):
    script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../usershells.py'))

    def run_script(self, args=None, input_data=None):
        command = ['python3', self.script_path]
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

    @patch('platform.system', return_value='Linux')
    def test_get_shells_from_passwd(self, mock_platform):
        passwd_data = (
            "user1:x:1000:1000::/home/user1:/bin/bash\n"
            "user2:x:1001:1001::/home/user2:/usr/sbin/nologin\n"
            "user3:x:1002:1002::/home/user3:/bin/zsh\n"
        )
        m = mock_open(read_data=passwd_data)
        m.return_value.__iter__.return_value = passwd_data.splitlines()
        with patch('builtins.open', m):
            f = io.StringIO()
            with redirect_stdout(f):
                usershells.main()
            output = f.getvalue()
            self.assertIn("user1", output)
            self.assertIn("user3", output)
            self.assertNotIn("user2", output)

    @patch('platform.system', return_value='Darwin')
    @patch('subprocess.check_output')
    def test_get_shells_from_dscl(self, mock_check_output, mock_platform):
        def side_effect(cmd, *args, **kwargs):
            if cmd == ['dscl', '.', '-list', '/Users']:
                return b'user1\nuser2\nuser3\n'
            elif cmd == ['dscl', '.', '-read', '/Users/user1', 'UserShell']:
                return b'UserShell: /bin/bash'
            elif cmd == ['dscl', '.', '-read', '/Users/user2', 'UserShell']:
                return b'UserShell: /usr/bin/false'
            elif cmd == ['dscl', '.', '-read', '/Users/user3', 'UserShell']:
                return b'UserShell: /bin/zsh'
            return b''

        mock_check_output.side_effect = side_effect

        f = io.StringIO()
        with redirect_stdout(f):
            usershells.main()
        output = f.getvalue()
        self.assertIn("user1", output)
        self.assertIn("user3", output)
        self.assertNotIn("user2", output)


if __name__ == '__main__':
    unittest.main()
