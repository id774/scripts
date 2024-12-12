#!/usr/bin/env python

########################################################################
# flatdirs_test.py: Test script for flatdirs.py
#
#  Description:
#  This script contains unit tests for the flatdirs.py script.
#  It verifies basic functionalities such as file moving, copying, renaming,
#  dry-run mode, and quiet mode. The tests use mock objects to simulate file
#  system operations without altering the actual environment.
#
#  Note:
#  Advanced test cases for name conflicts, nested subdirectories, and
#  special characters have been temporarily removed to focus on primary
#  functionalities after recent script updates. These test cases will be
#  reintroduced in subsequent updates.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2024-12-12
#       Simplified test cases to match recent updates in flatdirs.py.
#       Temporarily removed advanced test cases for name conflicts,
#       nested directories, and special character handling.
#  v1.2 2024-03-11
#       Added new test cases to enhance coverage and ensure robustness.
#  v1.1 2024-01-13
#       Updated for script name change to flatdirs.py.
#       Enhanced test cases to cover all major functionalities.
#  v1.0 2024-01-11
#       Initial test script for flatdirs.py
#
########################################################################


import os
import sys
import unittest
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import flatdirs


class TestFlatDirs(unittest.TestCase):
    """Unit tests for the flatdirs.py script."""

    def setUp(self):
        """Common setup for all tests."""
        self.mock_listdir = patch('flatdirs.os.listdir').start()
        self.mock_isdir = patch('flatdirs.os.path.isdir').start()
        self.mock_rmdir = patch('flatdirs.os.rmdir').start()
        self.mock_rename = patch('flatdirs.os.rename').start()
        self.mock_copy = patch('flatdirs.shutil.copy').start()
        self.mock_move = patch('flatdirs.shutil.move').start()
        self.mock_print_action = patch('flatdirs.print_action').start()
        self.mock_exists = patch('flatdirs.os.path.exists').start()

        self.mock_print_action.side_effect = lambda *args, **kwargs: None
        self.dir_contents = {
            'target_dir': ['subdir1', 'subdir2', 'file1.txt', 'file2.txt'],
            'target_dir/subdir1': ['file3.txt'],
            'target_dir/subdir2': ['file4.txt']
        }
        self.mock_listdir.side_effect = lambda path: self.dir_contents.get(path, [])
        self.mock_isdir.side_effect = lambda path: path in self.dir_contents
        self.mock_exists.side_effect = lambda path: path in self.dir_contents
        self.mock_copy.side_effect = lambda src, dest: None
        self.mock_rename.side_effect = lambda src, dest: None
        self.mock_move.side_effect = lambda src, dest: None
        self.mock_rmdir.side_effect = lambda path: self.dir_contents.pop(path, None)

        # Default target directory
        self.default_target_dir = "target_dir"

    def tearDown(self):
        """Tear down mocks after each test."""
        patch.stopall()

    def test_move_files(self):
        """Test moving files."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False,
                                 target_dir=self.default_target_dir)
        flatdirs.main(test_options)
        self.mock_move.assert_called()

    def test_copy_files(self):
        """Test copying files."""
        test_options = MagicMock(move_mode=False, copy_mode=True, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False,
                                 target_dir=self.default_target_dir)
        flatdirs.main(test_options)
        self.mock_copy.assert_called()

    def test_rename_files(self):
        """Test renaming files without moving or copying."""
        test_options = MagicMock(move_mode=False, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=True,
                                 target_dir=self.default_target_dir)
        flatdirs.main(test_options)
        self.mock_rename.assert_called()

    def test_dry_run_mode(self):
        """Test dry run mode without actual file operations."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=False, rename_only_mode=False,
                                 target_dir=self.default_target_dir)
        flatdirs.main(test_options)
        self.mock_move.assert_not_called()
        self.mock_copy.assert_not_called()
        self.mock_rename.assert_not_called()
        self.mock_rmdir.assert_not_called()

    def test_quiet_mode(self):
        """Test quiet mode suppressing print statements."""
        test_options = MagicMock(move_mode=False, copy_mode=True, delete_mode=False,
                                 quiet_mode=True, execute_mode=True, rename_only_mode=False,
                                 target_dir=self.default_target_dir)
        flatdirs.main(test_options)
        self.mock_print_action.assert_not_called()


if __name__ == '__main__':
    unittest.main()
