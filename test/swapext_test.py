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
#  Version History:
#  v1.2 2024-03-23
#       Significantly expanded test cases to improve coverage and ensure
#       the script correctly handles case-sensitive extensions, empty directories,
#       subdirectories, files with special characters, and read-only files.
#  v1.1 2024-01-11
#       Added '-q' option for quiet mode.
#  v1.0 2024-01-10
#       Initial release of test script.
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
########################################################################

import os
import sys
import unittest
from unittest.mock import call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import swapext


class TestSwapExt(unittest.TestCase):
    """Test cases for the swap_extensions function in swapext.py."""

    @patch('os.rename')
    @patch('os.walk')
    def test_swap_extensions(self, mock_walk, mock_rename):
        """Test swapping file extensions."""
        # Mock the os.walk to simulate directory structure
        mock_walk.return_value = [
            ('/testdir', ['subdir'], ['file1.txt', 'file2.doc', 'file3.txt']),
            ('/testdir/subdir', [], ['file4.txt', 'file5.doc'])
        ]

        # Call the function with quiet_mode=False
        swapext.swap_extensions('/testdir', 'txt', 'md', False)

        # Define expected calls to os.rename
        expected_calls = [
            call('/testdir/file1.txt', '/testdir/file1.md'),
            call('/testdir/file3.txt', '/testdir/file3.md'),
            call('/testdir/subdir/file4.txt', '/testdir/subdir/file4.md')
        ]
        # Verify that os.rename is called with the expected arguments
        mock_rename.assert_has_calls(expected_calls, any_order=True)

    @patch('os.rename')
    @patch('os.walk')
    def test_swap_extensions_quiet_mode(self, mock_walk, mock_rename):
        """Test swapping file extensions with quiet mode."""
        # Mock the os.walk to simulate directory structure
        mock_walk.return_value = [
            ('/testdir', ['subdir'], ['file1.txt', 'file2.doc', 'file3.txt']),
            ('/testdir/subdir', [], ['file4.txt', 'file5.doc'])
        ]

        # Call the function with quiet_mode=True
        swapext.swap_extensions('/testdir', 'txt', 'md', True)

        # Define expected calls to os.rename
        expected_calls = [
            call('/testdir/file1.txt', '/testdir/file1.md'),
            call('/testdir/file3.txt', '/testdir/file3.md'),
            call('/testdir/subdir/file4.txt', '/testdir/subdir/file4.md')
        ]
        # Verify that os.rename is called with the expected arguments
        mock_rename.assert_has_calls(expected_calls, any_order=True)

    @patch('os.rename')
    @patch('os.walk')
    def test_swap_extensions_no_extension_files(self, mock_walk, mock_rename):
        """Test swapping file extensions when there are files without extensions."""
        mock_walk.return_value = [
            ('/testdir', [], ['file1', 'file2.txt', 'file3'])
        ]

        swapext.swap_extensions('/testdir', 'txt', 'md', False)

        expected_calls = [call('/testdir/file2.txt', '/testdir/file2.md')]
        mock_rename.assert_has_calls(expected_calls)

    @patch('os.rename')
    @patch('os.walk')
    def test_no_files_with_target_extension(self, mock_walk, mock_rename):
        """Test when there are no files with the target extension."""
        mock_walk.return_value = [
            ('/testdir', [], ['file1.jpg', 'file2.jpg', 'file3.jpg'])
        ]

        swapext.swap_extensions('/testdir', 'txt', 'md', False)

        mock_rename.assert_not_called()

    @patch('os.rename')
    @patch('os.walk')
    def test_files_with_same_name_different_extensions(self, mock_walk, mock_rename):
        """Test files with the same name but different extensions."""
        mock_walk.return_value = [
            ('/testdir', [], ['file1.txt', 'file1.jpg', 'file1.png'])
        ]

        swapext.swap_extensions('/testdir', 'txt', 'md', False)

        expected_calls = [call('/testdir/file1.txt', '/testdir/file1.md')]
        mock_rename.assert_has_calls(expected_calls)

    @patch('os.rename')
    @patch('os.walk')
    def test_empty_directory(self, mock_walk, mock_rename):
        """Test an empty directory."""
        mock_walk.return_value = [('/emptydir', [], [])]

        swapext.swap_extensions('/emptydir', 'txt', 'md', False)

        mock_rename.assert_not_called()

    @patch('os.rename')
    @patch('os.walk')
    def test_directory_with_subdirectories(self, mock_walk, mock_rename):
        """Test a directory that includes subdirectories."""
        mock_walk.return_value = [
            ('/testdir', ['subdir1', 'subdir2'], ['file1.txt']),
            ('/testdir/subdir1', [], ['file2.txt', 'file3.doc']),
            ('/testdir/subdir2', [], ['file4.txt'])
        ]

        swapext.swap_extensions('/testdir', 'txt', 'md', False)

        expected_calls = [
            call('/testdir/file1.txt', '/testdir/file1.md'),
            call('/testdir/subdir1/file2.txt', '/testdir/subdir1/file2.md'),
            call('/testdir/subdir2/file4.txt', '/testdir/subdir2/file4.md')
        ]
        mock_rename.assert_has_calls(expected_calls, any_order=True)

    @patch('os.rename')
    @patch('os.walk')
    def test_files_with_special_characters_in_name(self, mock_walk, mock_rename):
        """Test files with special characters in their names."""
        mock_walk.return_value = [
            ('/testdir', [], ['file #1.txt', 'file @2.txt'])
        ]

        swapext.swap_extensions('/testdir', 'txt', 'md', False)

        expected_calls = [
            call('/testdir/file #1.txt', '/testdir/file #1.md'),
            call('/testdir/file @2.txt', '/testdir/file @2.md')
        ]
        mock_rename.assert_has_calls(expected_calls, any_order=True)

    @patch('os.rename')
    @patch('os.walk')
    def test_case_sensitive_extension(self, mock_walk, mock_rename):
        """Test that the function correctly handles case-sensitive extensions."""
        mock_walk.return_value = [
            ('/testdir', [], ['file1.TXT', 'file2.txt', 'file3.TxT'])
        ]

        swapext.swap_extensions('/testdir', '.TXT', '.md', False)

        expected_calls = [
            call('/testdir/file1.TXT', '/testdir/file1.md')
        ]
        mock_rename.assert_has_calls(expected_calls, any_order=True)

    @patch('os.rename')
    @patch('os.walk')
    def test_read_only_files(self, mock_walk, mock_rename):
        """Test attempting to rename read-only files."""
        mock_walk.return_value = [
            ('/testdir', [], ['read_only_file.txt'])
        ]

        # Simulate an OSError for trying to rename a read-only file
        mock_rename.side_effect = OSError("Permission denied")

        with self.assertLogs(level='ERROR') as log:
            swapext.swap_extensions('/testdir', 'txt', 'md', False)

        # Check if the error was logged
        self.assertIn('ERROR:root:Error renaming', log.output[0])


if __name__ == '__main__':
    unittest.main()
