#!/usr/bin/env python

########################################################################
# swapext_test.py: Test script for swapext.py
#
#  Description:
#  This script contains the unit tests for the swapext.py script. It tests
#  the functionality of the swap_extensions function to ensure that it
#  correctly changes the file extensions within a specified directory.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script from the command line using:
#      python test/swapext_test.py
#
#  The script tests the swap_extensions function from swapext.py with
#  and without the quiet mode (-q) option. It uses mock objects to
#  simulate file system operations and checks whether the file extensions
#  are correctly changed as per the specified parameters.
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - In dry-run mode, print the planned rename operation and do not call os.rename.
#    - In execute mode, rename all matching files and call os.rename with expected source/destination paths.
#    - Exit with code 1 when a rename operation fails with OSError during execute mode.
#    - Suppress all output in quiet mode.
#    - Rename matching files in subdirectories (recursive traversal via os.walk).
#    - Exit when validate_args() is given the same source and destination extensions.
#    - Exit when validate_args() is given extensions missing the leading dot.
#    - Exit when validate_args() is given a nonexistent directory.
#    - Exit when validate_args() is given a directory that is not readable.
#    - Exit when validate_args() is given a directory that is not writable.
#
#  Version History:
#  v2.1 2025-06-30
#       Added unit tests for argument validation in swapext.validate_args.
#       Covers same extension, missing dot, unreadable/unwritable or missing directory.
#  v2.0 2025-04-15
#       Replaced sys.argv parsing with OptionParser.
#       Added -x option to enable execution (default is dry-run).
#       Added confirmation prompt before executing changes.
#  v1.2 2024-03-23
#       Significantly expanded test cases to improve coverage and ensure
#       the script correctly handles case-sensitive extensions, empty directories,
#       subdirectories, files with special characters, and read-only files.
#  v1.1 2024-01-11
#       Added '-q' option for quiet mode.
#  v1.0 2024-01-10
#       Initial release of test script.
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import call, patch

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import swapext


class TestSwapExt(unittest.TestCase):
    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'swapext.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @patch('os.rename')
    @patch('os.walk')
    def test_dry_run_output(self, mock_walk, mock_rename):
        mock_walk.return_value = [('/testdir', [], ['file1.txt'])]
        with patch('builtins.print') as mock_print:
            swapext.swap_extensions('/testdir', '.txt', '.md', dry_run=True, quiet_mode=False)
            mock_print.assert_any_call('[INFO] DRY RUN: Renamed: /testdir/file1.txt -> /testdir/file1.md')
        mock_rename.assert_not_called()

    @patch('os.rename')
    @patch('os.walk')
    def test_execute_rename(self, mock_walk, mock_rename):
        mock_walk.return_value = [('/testdir', [], ['a.txt', 'b.txt'])]
        swapext.swap_extensions('/testdir', '.txt', '.md', dry_run=False, quiet_mode=True)
        expected = [
            call('/testdir/a.txt', '/testdir/a.md'),
            call('/testdir/b.txt', '/testdir/b.md')
        ]
        mock_rename.assert_has_calls(expected, any_order=True)

    @patch('sys.stderr')
    @patch('os.walk')
    @patch('os.rename', side_effect=OSError("fail"))
    def test_rename_failure_exits(self, mock_rename, mock_walk, mock_stderr):
        mock_walk.return_value = [('/faildir', [], ['bad.txt'])]
        with self.assertRaises(SystemExit) as cm:
            swapext.swap_extensions('/faildir', '.txt', '.md', dry_run=False, quiet_mode=True)
        self.assertEqual(cm.exception.code, 1)

    @patch('os.rename')
    @patch('os.walk')
    def test_quiet_mode_suppresses_output(self, mock_walk, mock_rename):
        mock_walk.return_value = [('/quietdir', [], ['file.txt'])]
        with patch('builtins.print') as mock_print:
            swapext.swap_extensions('/quietdir', '.txt', '.md', dry_run=True, quiet_mode=True)
            mock_print.assert_not_called()

    @patch('os.rename')
    @patch('os.walk')
    def test_subdirectories_are_handled(self, mock_walk, mock_rename):
        mock_walk.return_value = [
            ('/dir', ['sub'], ['a.txt']),
            ('/dir/sub', [], ['b.txt'])
        ]
        swapext.swap_extensions('/dir', '.txt', '.md', dry_run=False, quiet_mode=True)
        expected = [
            call('/dir/a.txt', '/dir/a.md'),
            call('/dir/sub/b.txt', '/dir/sub/b.md')
        ]
        mock_rename.assert_has_calls(expected, any_order=True)

    @patch('sys.stderr')
    def test_same_extension_exits(self, mock_stderr):
        args = ['/tmp', '.txt', '.txt']
        with patch('os.path.isdir', return_value=True), \
                patch('os.access', return_value=True), \
                self.assertRaises(SystemExit):
            swapext.validate_args(args)

    @patch('sys.stderr')
    def test_missing_dot_prefix_exits(self, mock_stderr):
        args = ['/tmp', 'txt', 'md']
        with patch('os.path.isdir', return_value=True), \
                patch('os.access', return_value=True), \
                self.assertRaises(SystemExit):
            swapext.validate_args(args)

    @patch('sys.stderr')
    def test_nonexistent_directory_exits(self, mock_stderr):
        args = ['/nonexistent', '.txt', '.md']
        with patch('os.path.isdir', return_value=False), \
                self.assertRaises(SystemExit):
            swapext.validate_args(args)

    @patch('sys.stderr')
    def test_unreadable_directory_exits(self, mock_stderr):
        args = ['/tmp', '.txt', '.md']
        with patch('os.path.isdir', return_value=True), \
                patch('os.access', side_effect=lambda path, mode: mode != os.R_OK), \
                self.assertRaises(SystemExit):
            swapext.validate_args(args)

    @patch('sys.stderr')
    def test_unwritable_directory_exits(self, mock_stderr):
        args = ['/tmp', '.txt', '.md']
        with patch('os.path.isdir', return_value=True), \
                patch('os.access', side_effect=lambda path, mode: mode != os.W_OK), \
                self.assertRaises(SystemExit):
            swapext.validate_args(args)


if __name__ == '__main__':
    unittest.main()
