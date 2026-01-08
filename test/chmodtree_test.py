#!/usr/bin/env python

########################################################################
# chmodtree_test.py: Tests for chmodtree.py
#
#  Description:
#  This test suite is designed to test the chmodtree.py script, focusing on
#  the functionality of changing file and directory permissions recursively
#  within a specified directory. It tests various combinations of options.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Verify check_command does nothing (no output, no exit) when the command exists and is executable.
#    - Verify check_command prints an error and exits with code 127 when the command does not exist.
#    - Verify check_command prints an error and exits with code 126 when the command exists but is not executable.
#    - Apply chmod to files only with a name pattern (no sudo, quiet mode).
#    - Apply chmod to directories only (with sudo, verbose mode).
#    - Apply chmod to both files and directories with a name pattern (with sudo, quiet mode).
#    - Apply chmod to both files and directories with a name pattern (with sudo, verbose mode).
#    - Apply chmod to both files and directories with a name pattern (no sudo, quiet mode).
#
#  Running the tests:
#  Execute the test script from the command line:
#      python test/chmodtree_test.py
#
#  Version History:
#  v1.2 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.1 2024-01-28
#       Added individual test cases for check_command function to verify
#       behavior with existing, nonexistent, and non-executable commands.
#  v1.0 2023-12-15
#       First release of the test suite for chmodtree.py.
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import chmodtree


class TestChmodTree(unittest.TestCase):
    """ Unit tests for the chmodtree.py script. """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'chmodtree.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @patch('chmodtree.os.access')
    @patch('chmodtree.find_command')
    @patch('chmodtree.sys.exit')
    @patch('chmodtree.print')
    def test_check_command_with_existing_executable_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that exists and is executable. """
        mock_find_command.return_value = '/usr/bin/command'
        mock_access.return_value = True
        chmodtree.check_command('command')
        mock_exit.assert_not_called()
        mock_print.assert_not_called()

    @patch('chmodtree.os.access')
    @patch('chmodtree.find_command')
    @patch('chmodtree.sys.exit')
    @patch('chmodtree.print')
    def test_check_command_with_nonexistent_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that does not exist. """
        mock_find_command.return_value = None
        chmodtree.check_command('nonexistent')
        mock_print.assert_called_with("[ERROR] Command 'nonexistent' is not installed. Please install nonexistent and try again.", file=sys.stderr)
        mock_exit.assert_called_with(127)

    @patch('chmodtree.os.access')
    @patch('chmodtree.find_command')
    @patch('chmodtree.sys.exit')
    @patch('chmodtree.print')
    def test_check_command_with_nonexecutable_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that exists but is not executable. """
        mock_find_command.return_value = '/usr/bin/nonexecutable'
        mock_access.return_value = False
        chmodtree.check_command('nonexecutable')
        mock_print.assert_called_with("[ERROR] Command 'nonexecutable' is not executable. Please check the permissions.", file=sys.stderr)
        mock_exit.assert_called_with(126)

    @patch('chmodtree.os_exec')
    def test_files_chmod(self, mock_os_exec):
        """ Test chmod application to files only, matching a specific pattern. """
        mock_os_exec.return_value = None  # Mock the os_exec function
        options = MagicMock()
        options.sudo = False
        options.quiet = True
        options.files = '644'
        options.dirs = None
        options.name = '*.py'
        chmodtree.chmodtree(options, 'testdir1')
        mock_os_exec.assert_called_with(
            'find testdir1 -name "*.py" -type f -exec chmod 644 {} \\;')

    @patch('chmodtree.os_exec')
    def test_dirs_chmod(self, mock_os_exec):
        """ Test chmod application to directories only, using sudo. """
        options = MagicMock()
        options.sudo = True
        options.quiet = False
        options.files = None
        options.dirs = '755'
        options.name = None
        chmodtree.chmodtree(options, 'testdir2')
        mock_os_exec.assert_called_with(
            'sudo find testdir2 -type d -exec chmod -v 755 {} \\;')

    @patch('chmodtree.os_exec')
    def test_files_and_dirs_chmod_with_pattern(self, mock_os_exec):
        """ Test chmod application to both files and directories, matching a specific pattern. """
        options = MagicMock()
        options.sudo = True
        options.quiet = True
        options.files = '600'
        options.dirs = '700'
        options.name = '*.rb'
        chmodtree.chmodtree(options, 'testdir3')

        expected_calls = [
            ('sudo find testdir3 -name "*.rb" -type f -exec chmod 600 {} \\;',),
            ('sudo find testdir3 -name "*.rb" -type d -exec chmod 700 {} \\;',)
        ]
        actual_calls = [call_args[0]
                        for call_args in mock_os_exec.call_args_list]
        self.assertEqual(actual_calls, expected_calls)

    @patch('chmodtree.os_exec')
    def test_files_and_dirs_sudo_verbose_chmod(self, mock_os_exec):
        """ Test chmod application to both files and directories with sudo, verbose output. """
        options = MagicMock()
        options.sudo = True
        options.quiet = False
        options.files = '640'
        options.dirs = '750'
        options.name = '*.txt'
        chmodtree.chmodtree(options, 'testdir4')

        expected_calls = [
            ('sudo find testdir4 -name "*.txt" -type f -exec chmod -v 640 {} \\;',),
            ('sudo find testdir4 -name "*.txt" -type d -exec chmod -v 750 {} \\;',)
        ]
        actual_calls = [call_args[0]
                        for call_args in mock_os_exec.call_args_list]
        self.assertEqual(actual_calls, expected_calls)

    @patch('chmodtree.os_exec')
    def test_files_and_dirs_quiet_chmod_with_pattern(self, mock_os_exec):
        """ Test chmod application to both files and directories, matching a specific pattern, in quiet mode. """
        options = MagicMock()
        options.sudo = False
        options.quiet = True
        options.files = '775'
        options.dirs = '750'
        options.name = '*.sh'
        chmodtree.chmodtree(options, 'testdir5')

        expected_calls = [
            ('find testdir5 -name "*.sh" -type f -exec chmod 775 {} \\;',),
            ('find testdir5 -name "*.sh" -type d -exec chmod 750 {} \\;',)
        ]
        actual_calls = [call_args[0]
                        for call_args in mock_os_exec.call_args_list]
        self.assertEqual(actual_calls, expected_calls)


if __name__ == '__main__':
    unittest.main()
