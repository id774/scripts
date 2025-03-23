#!/usr/bin/env python

########################################################################
# flatdirs_test.py: Test script for flatdirs.py
#
#  Description:
#  This script contains comprehensive unit tests for the flatdirs.py script.
#  It verifies the script's functionality including file moving, copying,
#  renaming, deleting empty directories, and proper handling of different
#  operation modes such as dry run and quiet mode. Mock objects are used
#  to simulate file system operations, allowing for the verification of
#  script behavior under various conditions without altering the actual
#  file system.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2024-12-15
#       Updated tests to mock `input` and suppress print output
#       for confirmation prompt.
#  v1.2 2024-03-11
#       Added new test cases to enhance coverage and ensure the script's
#       robustness in handling various file system operations.
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
from unittest.mock import MagicMock, call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from unittest.mock import patch

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
        self.mock_input = patch('builtins.input', return_value='yes').start()  # Mock input to always return "yes"
        self.mock_print = patch('builtins.print').start()  # Mock print to suppress output

        self.mock_print_action.side_effect = lambda *args, **kwargs: None
        self.dir_contents = {
            '.': ['subdir1', 'subdir2', 'file1.txt', 'file2.txt'],
            'subdir1': ['file3.txt'],
            'subdir2': ['file4.txt']
        }
        # Simulate directory contents and behaviors
        self.mock_listdir.side_effect = lambda path: self.dir_contents.get(path, [])
        self.mock_isdir.side_effect = lambda path: path in self.dir_contents
        self.mock_copy.side_effect = lambda src, dest: None
        self.mock_rename.side_effect = lambda src, dest: None
        self.mock_move.side_effect = lambda src, dest: None
        self.mock_rmdir.side_effect = lambda path: None

    def tearDown(self):
        """Tear down all mocks after each test."""
        patch.stopall()

    def test_move_files(self):
        """Test moving files."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        self.mock_move.assert_called()

    def test_copy_files(self):
        """Test copying files."""
        test_options = MagicMock(move_mode=False, copy_mode=True, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        self.mock_copy.assert_called()

    def test_rename_files(self):
        """Test renaming files without moving or copying."""
        test_options = MagicMock(move_mode=False, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=True)
        flatdirs.main(test_options)
        self.mock_rename.assert_called()

    def test_delete_empty_directories(self):
        """Test deleting empty directories."""
        # Update directory contents to simulate empty directories
        self.dir_contents = {
            '.': ['subdir1', 'subdir2'],
            'subdir1': [],
            'subdir2': []
        }
        self.mock_listdir.side_effect = lambda path: self.dir_contents.get(path, [
        ])
        self.mock_isdir.side_effect = lambda path: path in self.dir_contents

        test_options = MagicMock(move_mode=False, copy_mode=False, delete_mode=True,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)

        # Check if rmdir was called for both empty subdirectories
        self.mock_rmdir.assert_any_call('subdir1')
        self.mock_rmdir.assert_any_call('subdir2')

    def test_dry_run_mode(self):
        """Test dry run mode without actual file operations."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=False, rename_only_mode=False)
        flatdirs.main(test_options)
        self.mock_move.assert_not_called()
        self.mock_copy.assert_not_called()
        self.mock_rename.assert_not_called()
        self.mock_rmdir.assert_not_called()

    def test_quiet_mode(self):
        """Test quiet mode suppressing print statements."""
        test_options = MagicMock(move_mode=False, copy_mode=True, delete_mode=False,
                                 quiet_mode=True, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        self.mock_print_action.assert_not_called()

    def test_move_files_permission_error(self):
        """Test handling permission error during file move within subdirectories."""
        self.mock_move.side_effect = PermissionError("Permission denied")
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        with self.assertRaises(PermissionError):
            flatdirs.main(test_options)

    def test_handle_nested_subdirectories(self):
        """Test handling files in nested subdirectories are moved correctly to the base directory."""
        self.dir_contents['subdir1'].append('nested_subdir')
        self.dir_contents['subdir1/nested_subdir'] = ['file5.txt']
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        expected_destination = 'subdir1_nested_subdir_file5.txt'
        self.mock_move.assert_any_call(os.path.join('subdir1', 'nested_subdir', 'file5.txt'), expected_destination)

    def test_move_files_to_correct_destination(self):
        """Test files are moved to the correct destination in the base directory."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        expected_calls = [
            call(os.path.join('subdir1', 'file3.txt'), 'subdir1_file3.txt'),
            call(os.path.join('subdir2', 'file4.txt'), 'subdir2_file4.txt')
        ]
        self.mock_move.assert_has_calls(expected_calls, any_order=True)

    def test_move_files_with_name_conflict(self):
        """Test moving files with name conflict results in overwriting in the base directory."""
        # Simulate a file that would cause a name conflict
        self.dir_contents['.'].append('subdir1_file3.txt')
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        # Expected that the existing file in the base directory is overwritten
        self.mock_move.assert_any_call(os.path.join('subdir1', 'file3.txt'), 'subdir1_file3.txt')

    def test_handle_special_characters_in_filenames(self):
        """Test handling files with special characters in their names are moved correctly to the base directory."""
        self.dir_contents['subdir2'].append('special@file$.txt')
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        # The special character file is moved to the base directory with its directory prefix
        self.mock_move.assert_any_call(os.path.join('subdir2', 'special@file$.txt'), 'subdir2_special@file$.txt')

    @patch('builtins.input', return_value='yes')
    def test_confirmation_prompt_yes(self, mock_input):
        """Test confirmation prompt with 'yes' response."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        self.mock_move.assert_called()  # File operations should proceed

    @patch('builtins.input', return_value='y')
    def test_confirmation_prompt_y(self, mock_input):
        """Test confirmation prompt with 'y' response."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        flatdirs.main(test_options)
        self.mock_move.assert_called()  # File operations should proceed

    @patch('builtins.input', return_value='no')
    def test_confirmation_prompt_no(self, mock_input):
        """Test confirmation prompt with 'no' response."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        with self.assertRaises(SystemExit):  # Script should exit
            flatdirs.main(test_options)
        self.mock_move.assert_not_called()  # No file operations should occur

    @patch('builtins.input', return_value='unexpected')
    def test_confirmation_prompt_unexpected(self, mock_input):
        """Test confirmation prompt with an unexpected response."""
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=True, rename_only_mode=False)
        with self.assertRaises(SystemExit):  # Script should exit
            flatdirs.main(test_options)
        self.mock_move.assert_not_called()  # No file operations should occur

    @patch('builtins.input', return_value='yes')
    def test_no_confirmation_without_execute_mode(self, mock_input):
        """Test that confirmation prompt is skipped and no execution occurs without execute mode."""
        # Prepare test options with execute_mode=False
        test_options = MagicMock(move_mode=True, copy_mode=False, delete_mode=False,
                                 quiet_mode=False, execute_mode=False, rename_only_mode=False)

        # Simulate directory contents and behaviors
        self.dir_contents = {
            '.': ['subdir1'],
            'subdir1': ['file1.txt']
        }
        self.mock_listdir.side_effect = lambda path: self.dir_contents.get(path, [])
        self.mock_isdir.side_effect = lambda path: path in self.dir_contents

        # Execute the main function
        flatdirs.main(test_options)

        # Assertions
        mock_input.assert_not_called()  # Ensure no prompt was shown
        self.mock_move.assert_not_called()  # Ensure no file operations occurred


if __name__ == '__main__':
    unittest.main()
