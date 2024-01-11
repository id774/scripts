#!/usr/bin/env python

########################################################################
# flatten_directories_test.py: Test script for flatten_directories.py
#
# Description:
# This script contains unit tests for the flatten_directories.py script.
# It verifies that the script correctly performs file operations such as
# moving, copying, renaming, and deleting empty directories. Mock objects
# are used to simulate file system operations and to verify the behavior
# of the script under different conditions.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
# v1.0 2024-01-11: Initial test script for flatten_directories.py
#
########################################################################

import unittest
from unittest.mock import patch, MagicMock
import sys
import os

# Adjusting the path to import flatten_directories from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import flatten_directories

class TestFlattenDirectories(unittest.TestCase):
    """Unit tests for the flatten_directories.py script."""

    @patch('flatten_directories.print_action')
    @patch('flatten_directories.shutil.move')
    @patch('flatten_directories.shutil.copy')
    @patch('flatten_directories.os.rename')
    @patch('flatten_directories.os.rmdir')
    @patch('flatten_directories.os.path.isdir')
    @patch('flatten_directories.os.listdir')
    def test_flatten_directories(self, mock_listdir, mock_isdir, mock_rmdir, mock_rename, mock_copy, mock_move, mock_print_action):
        """Test the main functionality of flatten_directories.py."""
        # Suppressing print_action output during testing
        mock_print_action.side_effect = lambda *args, **kwargs: None

        # Simulating a directory structure for testing
        dir_contents = {
            '.': ['subdir1', 'subdir2', 'file1.txt', 'file2.txt'],
            'subdir1': ['file3.txt'],
            'subdir2': ['file4.txt']
        }

        def mocked_listdir(path):
            """Mocked version of os.listdir."""
            return dir_contents.get(path, [])

        mock_listdir.side_effect = mocked_listdir
        mock_isdir.side_effect = lambda path: path in dir_contents
        mock_copy.side_effect = lambda src, dest: None
        mock_rename.side_effect = lambda src, dest: None
        mock_move.side_effect = lambda src, dest: None
        mock_rmdir.side_effect = lambda path: None

        # Setting up test options
        test_options = MagicMock()
        test_options.move_mode = False
        test_options.copy_mode = True
        test_options.delete_mode = False
        test_options.quiet_mode = False
        test_options.execute_mode = True
        test_options.rename_only_mode = False

        # Calling the main function with test options
        flatten_directories.main(test_options)

        # Assertions to verify the expected behavior
        mock_listdir.assert_called_with('subdir2')
        mock_isdir.assert_any_call('subdir1')
        mock_isdir.assert_any_call('subdir2')
        mock_copy.assert_any_call(os.path.join(
            'subdir1', 'file3.txt'), 'subdir1_file3.txt')
        mock_copy.assert_any_call(os.path.join(
            'subdir2', 'file4.txt'), 'subdir2_file4.txt')
        mock_rmdir.assert_not_called()


if __name__ == '__main__':
    unittest.main()
