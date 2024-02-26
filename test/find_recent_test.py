#!/usr/bin/env python

########################################################################
# find_recent_test.py: Test script for find_recent.py
#
#  Description:
#  Tests the functionality of the find_recent.py script, ensuring it correctly
#  lists files modified after a specified date and time, and handles hidden
#  directories as specified.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-02-25
#       Initial release of test script.
#
#  Usage:
#  Run this script from the command line using:
#      python test/find_recent_test.py
#
########################################################################

import os
import sys
import unittest
from datetime import datetime, timezone
from unittest.mock import call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import find_recent


class TestFindRecent(unittest.TestCase):

    @patch('builtins.print')
    @patch('find_recent.os.path.getmtime')
    @patch('find_recent.os.walk')
    def test_recent_files(self, mock_walk, mock_getmtime, mock_print):
        """Test that only files modified after the specified date are listed."""
        test_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)
        test_path = "/path/to/directory"
        include_hidden = False

        # Mock the os.walk function to return a predefined file structure
        mock_walk.return_value = [
            (test_path, ["subdir"], ["recent.txt", "old.txt", ".hidden.txt"]),
        ]

        # Mock os.path.getmtime to return specific modification times for files
        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/recent.txt": test_date.timestamp() + 3600,  # 1 hour after the test date
            "/path/to/directory/old.txt": test_date.timestamp() - 86400,  # 1 day before the test date
            "/path/to/directory/.hidden.txt": test_date.timestamp() + 3600,  # 1 hour after the test date (hidden file)
        }[x]

        # Execute the function with the test parameters
        find_recent.list_recent_files(test_path, test_date, include_hidden)

        # Assertions to check if the 'print' function was called with the expected file
        # This requires patching 'print' function as well
        # Example: mock_print.assert_called_once_with('2024-02-24 08:00:00 - /path/to/directory/recent.txt')

    @patch('builtins.print')
    @patch('find_recent.os.path.getmtime')
    @patch('find_recent.os.walk')
    def test_hidden_directories_included(self, mock_walk, mock_getmtime, mock_print):
        """Test that hidden files are included when the '-a' option is used."""
        test_path = "/path/to/directory"
        include_hidden = True
        test_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["recent.txt", ".hidden_file.txt"]),
        ]
        mock_getmtime.return_value = test_date.timestamp() + 3600  # 1 hour after test_date

        find_recent.list_recent_files(test_path, test_date, include_hidden)

        mock_print.assert_any_call('2024-02-24 08:00:00 - /path/to/directory/recent.txt')
        mock_print.assert_any_call('2024-02-24 08:00:00 - /path/to/directory/.hidden_file.txt')

    @patch('builtins.print')
    @patch('find_recent.os.path.getmtime')
    @patch('find_recent.os.walk')
    def test_hidden_directories_excluded(self, mock_walk, mock_getmtime, mock_print):
        """Test that hidden files are excluded by default."""
        test_path = "/path/to/directory"
        include_hidden = False
        test_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["recent.txt", ".hidden_file.txt"]),
        ]
        mock_getmtime.return_value = test_date.timestamp() + 3600  # 1 hour after test_date

        find_recent.list_recent_files(test_path, test_date, include_hidden)

        mock_print.assert_called_once_with('2024-02-24 08:00:00 - /path/to/directory/recent.txt')
        # Ensure that the print function was not called for the hidden file

    @patch('builtins.print')
    @patch('find_recent.os.path.getmtime')
    @patch('find_recent.os.walk')
    def test_recent_files_with_various_file_structures(self, mock_walk, mock_getmtime, mock_print):
        """Test listing of recently modified files with various file structures."""
        test_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)
        test_path = "/path/to/directory"
        include_hidden = False

        # Define various file structures for testing
        file_structures = [
            (test_path, ["subdir1"], ["recent1.txt", "old1.txt"]),
            (os.path.join(test_path, "subdir1"), [], ["recent2.txt", ".hidden1.txt"]),
            (test_path, ["subdir2"], ["recent3.txt", "old2.txt", ".hidden2.txt"]),
            (os.path.join(test_path, "subdir2"), ["subsubdir1"], ["recent4.txt"]),
            (os.path.join(test_path, "subdir2", "subsubdir1"), [], ["recent5.txt", "old3.txt"]),
        ]

        # Define modification times for each file
        modification_times = {
            "/path/to/directory/recent1.txt": test_date.timestamp() + 3600,
            "/path/to/directory/old1.txt": test_date.timestamp() - 86400,
            "/path/to/directory/subdir1/recent2.txt": test_date.timestamp() + 7200,
            "/path/to/directory/subdir1/.hidden1.txt": test_date.timestamp() + 7200,
            "/path/to/directory/recent3.txt": test_date.timestamp() + 10800,
            "/path/to/directory/old2.txt": test_date.timestamp() - 86400,
            "/path/to/directory/.hidden2.txt": test_date.timestamp() + 10800,
            "/path/to/directory/subdir2/recent4.txt": test_date.timestamp() + 14400,
            "/path/to/directory/subdir2/subsubdir1/recent5.txt": test_date.timestamp() + 18000,
            "/path/to/directory/subdir2/subsubdir1/old3.txt": test_date.timestamp() - 86400,
        }

        # Mock the os.walk function to return the predefined file structures
        mock_walk.return_value = file_structures

        # Mock os.path.getmtime to return specific modification times for files
        mock_getmtime.side_effect = lambda x: modification_times[x]

        # Execute the function with the test parameters
        find_recent.list_recent_files(test_path, test_date, include_hidden)

        # Assertions to check if the 'print' function was called with the expected files
        expected_calls = [
            call('2024-02-24 08:00:00 - /path/to/directory/recent1.txt'),
            call('2024-02-24 09:00:00 - /path/to/directory/subdir1/recent2.txt'),
            call('2024-02-24 10:00:00 - /path/to/directory/recent3.txt'),
            call('2024-02-24 11:00:00 - /path/to/directory/subdir2/recent4.txt'),
            call('2024-02-24 12:00:00 - /path/to/directory/subdir2/subsubdir1/recent5.txt'),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

    @patch('builtins.print')
    @patch('find_recent.os.path.getmtime')
    @patch('find_recent.os.walk')
    def test_empty_directory(self, mock_walk, mock_getmtime, mock_print):
        """Test behavior when the specified directory is empty."""
        test_path = "/path/to/empty_directory"
        include_hidden = False
        test_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        # Mock the os.walk function to simulate an empty directory
        mock_walk.return_value = [(test_path, [], [])]

        find_recent.list_recent_files(test_path, test_date, include_hidden)

        # Ensure that the print function was not called
        mock_print.assert_not_called()

    @patch('builtins.print')
    @patch('find_recent.os.path.getmtime')
    @patch('find_recent.os.walk')
    def test_deep_directory_structure(self, mock_walk, mock_getmtime, mock_print):
        """Test behavior with a deep directory structure."""
        test_path = "/path/to/deep/directory/structure"
        include_hidden = False
        test_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        # Mock the os.walk function to return a deep directory structure
        mock_walk.return_value = [
            (test_path, ["subdir1", "subdir2"], ["recent1.txt"]),
            (os.path.join(test_path, "subdir1"), ["subsubdir"], ["recent2.txt"]),
            (os.path.join(test_path, "subdir1", "subsubdir"), [], ["recent3.txt"]),
        ]

        # Define modification times for each file
        modification_times = {
            os.path.join(test_path, "recent1.txt"): test_date.timestamp() + 3600,
            os.path.join(test_path, "subdir1", "recent2.txt"): test_date.timestamp() + 7200,
            os.path.join(test_path, "subdir1", "subsubdir", "recent3.txt"): test_date.timestamp() + 10800,
        }

        mock_getmtime.side_effect = lambda x: modification_times[x]

        find_recent.list_recent_files(test_path, test_date, include_hidden)

        # Assertions for expected output
        expected_calls = [
            call('2024-02-24 08:00:00 - {}'.format(os.path.join(test_path, "recent1.txt"))),
            call('2024-02-24 09:00:00 - {}'.format(os.path.join(test_path, "subdir1", "recent2.txt"))),
            call('2024-02-24 10:00:00 - {}'.format(os.path.join(test_path, "subdir1", "subsubdir", "recent3.txt"))),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)


if __name__ == '__main__':
    unittest.main()
