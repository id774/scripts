#!/usr/bin/env python

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
#    - find_command() resolves an executable command found on PATH
#    - find_command() returns None when the command is not found on PATH
#    - find_command() resolves an empty PATH component to the current directory
#    - command_exists() and get_command_path() are consistent with find_command()
#
#  Version History:
#  v1.1 2026-09-03
#       Add tests for the manual PATH search in find_command(), replacing the
#       previous 'command -v' based lookup.
#  v1.0 2025-07-07
#       Initial release.
#
########################################################################

import io
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import cal


class TestCal(unittest.TestCase):
    def run_script(self, args=None):
        script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../cal.py'))
        command = ['python', script_path]
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

    def make_fake_path(self, commands, non_executable):
        """ Create a temporary PATH directory containing the given commands; return its path. """
        directory = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, directory)
        for command in commands:
            path = os.path.join(directory, command)
            with open(path, 'w', encoding='utf-8') as f:
                f.write('#!/bin/sh\n')
            if command not in non_executable:
                os.chmod(path, 0o755)
            else:
                os.chmod(path, 0o644)
        return directory

    def test_find_command_resolves_executable_on_path(self):
        directory = self.make_fake_path(['fake_cal'], [])
        with patch.dict(os.environ, {'PATH': directory}):
            found = cal.find_command('fake_cal')
            self.assertTrue(cal.command_exists('fake_cal'))
            self.assertEqual(cal.get_command_path('fake_cal'), found)
        self.assertEqual(os.path.realpath(found),
                         os.path.realpath(os.path.join(directory, 'fake_cal')))

    def test_find_command_missing_returns_none(self):
        directory = self.make_fake_path([], [])
        with patch.dict(os.environ, {'PATH': directory}):
            self.assertIsNone(cal.find_command('nonexistent_command'))
            self.assertFalse(cal.command_exists('nonexistent_command'))
            self.assertIsNone(cal.get_command_path('nonexistent_command'))

    def test_find_command_empty_path_component_is_cwd(self):
        directory = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, directory)
        command_name = 'fake_command_for_cwd_test'
        path = os.path.join(directory, command_name)
        with open(path, 'w', encoding='utf-8') as f:
            f.write('#!/bin/sh\n')
        os.chmod(path, 0o755)
        original_cwd = os.getcwd()
        self.addCleanup(os.chdir, original_cwd)
        os.chdir(directory)
        with patch.dict(os.environ, {'PATH': ''}):
            found = cal.find_command(command_name)
        self.assertEqual(os.path.realpath(found), os.path.realpath(path))


if __name__ == '__main__':
    unittest.main()
