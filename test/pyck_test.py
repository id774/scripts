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
from unittest.mock import patch, MagicMock, call
import sys
import os

# Adjusting the path to import pyck from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import pyck

class TestPyck(unittest.TestCase):
    """Unit tests for the pyck.py script."""

    @patch('pyck.shutil.which')
    @patch('pyck.os.access')
    @patch('pyck.sys.exit')
    @patch('pyck.print')
    def test_check_command(self, mock_print, mock_exit, mock_access, mock_which):
        mock_which.return_value = None
        pyck.check_command('nonexistent_command')
        mock_print.assert_called_with(
            "Error: Command 'nonexistent_command' is not installed. Please install nonexistent_command and try again.")
        mock_exit.assert_called_with(127)

        mock_which.return_value = '/usr/bin/nonexecutable_command'
        mock_access.return_value = False
        pyck.check_command('nonexecutable_command')
        mock_print.assert_called_with(
            "Error: Command 'nonexecutable_command' is not executable. Please check the permissions.")
        mock_exit.assert_called_with(126)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_format_file(self, mock_print, mock_popen):
        pyck.format_file('path/to/file.py', 'E302,E402')

        expected_calls = [
            call(
                "autoflake --imports=django,requests,urllib3 -i path/to/file.py", shell=True),
            call().wait(),
            call("autopep8 --ignore=E302,E402 -v -i path/to/file.py", shell=True),
            call().wait()
        ]
        mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_run_command_success(self, mock_print, mock_popen):
        # Test scenario for successful command execution
        mock_process = MagicMock()
        mock_process.communicate.return_value = (b'output', b'')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.run_command('echo test', show_files=None)
        mock_print.assert_not_called()

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_run_command_error(self, mock_print, mock_popen):
        # Test scenario for command execution with an error
        mock_process = MagicMock()
        mock_process.communicate.return_value = (b'error output', b'')
        mock_process.returncode = 1
        mock_popen.return_value = mock_process

        pyck.run_command('echo test', show_files="Error occurred")
        mock_print.assert_called_with("Error occurred error output")

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_dry_run_formatting_single_file(self, mock_print, mock_popen):
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        # Testing dry-run for a single file
        pyck.dry_run_formatting(['path/to/single_file.py'], 'E302,E402')

        # Verify subprocess.Popen calls for the single file
        mock_popen.assert_any_call(
            "flake8 --ignore=E302,E402 path/to/single_file.py", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "autoflake --imports=django,requests,urllib3 --check path/to/single_file.py", shell=True, stdout=-1)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_dry_run_formatting_with_multiple_files(self, mock_print, mock_popen):
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.dry_run_formatting(
            ['path/to/file1.py', 'path/to/file2.py'], 'E302,E402')

        expected_calls = [
            call("flake8 --ignore=E302,E402 path/to/file1.py",
                 shell=True, stdout=-1),
            call("autoflake --imports=django,requests,urllib3 --check path/to/file1.py",
                 shell=True, stdout=-1),
            call("flake8 --ignore=E302,E402 path/to/file2.py",
                 shell=True, stdout=-1),
            call("autoflake --imports=django,requests,urllib3 --check path/to/file2.py",
                 shell=True, stdout=-1)
        ]
        mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_dry_run_formatting_single_directory(self, mock_print, mock_popen):
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        with patch('pyck.os.walk') as mock_walk:
            mock_walk.return_value = [
                ('path/to/directory', [], ['file1.py', 'file2.py'])]

            pyck.dry_run_formatting(['path/to/directory'], 'E302,E402')

            expected_calls = [
                call("flake8 --ignore=E302,E402 path/to/directory",
                     shell=True, stdout=-1),
                call("autoflake --imports=django,requests,urllib3 --check path/to/directory",
                     shell=True, stdout=-1)
            ]
            mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_dry_run_formatting_multiple_directories(self, mock_print, mock_popen):
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        with patch('pyck.os.walk') as mock_walk:
            mock_walk.side_effect = [
                [('path/to/dir1', [], ['file1.py', 'file2.py'])],
                [('path/to/dir2', [], ['file3.py', 'file4.py'])]
            ]

            pyck.dry_run_formatting(
                ['path/to/dir1', 'path/to/dir2'], 'E302,E402')

            expected_calls = [
                call("flake8 --ignore=E302,E402 path/to/dir1",
                     shell=True, stdout=-1),
                call("autoflake --imports=django,requests,urllib3 --check path/to/dir1",
                     shell=True, stdout=-1),
                call("flake8 --ignore=E302,E402 path/to/dir2",
                     shell=True, stdout=-1),
                call("autoflake --imports=django,requests,urllib3 --check path/to/dir2",
                     shell=True, stdout=-1)
            ]
            mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_single_file(self, mock_walk, mock_path, mock_print, mock_format_file):
        # Mocking file existence
        mock_path.isfile.return_value = True
        mock_path.isdir.return_value = False

        # Testing path for a single file
        pyck.execute_formatting(['path/to/single_file.py'], 'E302,E402')

        # Verify format_file call for the single file
        mock_format_file.assert_called_once_with(
            'path/to/single_file.py', 'E302,E402')

    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_with_multiple_files(self, mock_walk, mock_path, mock_print, mock_format_file):
        # Mocking file and directory existence
        mock_path.isfile.side_effect = lambda p: p == 'path/to/file.py'
        mock_path.isdir.side_effect = lambda p: p == 'path/to/directory'
        mock_walk.return_value = [
            ('path/to/directory', [], ['file1.py', 'file2.py'])]

        # Testing paths for a directory and a file
        pyck.execute_formatting(
            ['path/to/directory', 'path/to/file.py'], 'E302,E402')

        # Verify format_file calls for files in the directory
        expected_file1_path = os.path.join('path/to/directory', 'file1.py')
        expected_file2_path = os.path.join('path/to/directory', 'file2.py')
        mock_format_file.assert_any_call(expected_file1_path, 'E302,E402')
        mock_format_file.assert_any_call(expected_file2_path, 'E302,E402')

        # Verify format_file call for a single file
        mock_format_file.assert_any_call('path/to/file.py', 'E302,E402')

        # Test behavior when path is neither a file nor a directory
        pyck.execute_formatting(['invalid/path'], 'E302,E402')
        mock_print.assert_called_with(
            "Error: The specified path 'invalid/path' is neither a file nor a directory.")

    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_single_directory(self, mock_walk, mock_path, mock_print, mock_format_file):
        # Mocking directory existence
        mock_path.isfile.return_value = False
        mock_path.isdir.return_value = True
        mock_walk.return_value = [
            ('path/to/directory', [], ['file1.py', 'file2.py'])]

        # Testing path for a directory
        pyck.execute_formatting(['path/to/directory'], 'E302,E402')

        # Verify format_file calls for files in the directory
        expected_file1_path = os.path.join('path/to/directory', 'file1.py')
        expected_file2_path = os.path.join('path/to/directory', 'file2.py')
        mock_format_file.assert_any_call(expected_file1_path, 'E302,E402')
        mock_format_file.assert_any_call(expected_file2_path, 'E302,E402')

    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_multiple_directories(self, mock_walk, mock_path, mock_print, mock_format_file):
        # Mocking multiple directories
        mock_path.isfile.return_value = False
        mock_path.isdir.side_effect = lambda p: p in [
            'path/to/dir1', 'path/to/dir2']
        mock_walk.side_effect = [
            [('path/to/dir1', [], ['file1.py', 'file2.py'])],
            [('path/to/dir2', [], ['file3.py', 'file4.py'])]
        ]

        # Testing paths for multiple directories
        pyck.execute_formatting(['path/to/dir1', 'path/to/dir2'], 'E302,E402')

        # Verify format_file calls for files in each directory
        expected_calls = [
            os.path.join('path/to/dir1', 'file1.py'),
            os.path.join('path/to/dir1', 'file2.py'),
            os.path.join('path/to/dir2', 'file3.py'),
            os.path.join('path/to/dir2', 'file4.py')
        ]
        for call in expected_calls:
            mock_format_file.assert_any_call(call, 'E302,E402')


if __name__ == '__main__':
    unittest.main()
