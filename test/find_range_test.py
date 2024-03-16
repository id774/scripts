#!/usr/bin/env python

########################################################################
# find_range_test.py: Test script for find_range.py
#
#  Description:
#  Tests the functionality of the find_range.py script, ensuring it correctly
#  lists files modified after a specified date and time, and handles hidden
#  directories and various options as specified. It now includes tests for
#  local timezone handling with the '-l' option, ensuring both UTC and local
#  timezone outputs are correctly formatted and calculated.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2024-03-16
#       Added test cases for local timezone handling.
#  v1.3 2024-03-14
#       Updated test cases to expect output in ISO 8601 format, indicating UTC dates and times.
#       Added tests for the '-l' option to ensure correct handling of local timezone.
#       Updated existing tests to use ISO 8601 format for UTC dates and times.
#  v1.2 2024-03-09
#       Updated existing test cases for enhanced coverage and clarity.
#       Added new test cases to cover various patterns including:
#       - Specifying end date/time only.
#       - Combining end date/time with hidden file option.
#       - Combining start and end date/time with filenames only option.
#       Renamed script to find_range_test.py to better reflect its functionality
#       of searching files within a specified datetime range.
#  v1.1 2024-03-03
#       Added test case for '-f' option to ensure filenames are listed correctly.
#  v1.0 2024-02-25
#       Initial release of test script. Focused on basic functionality tests including
#       listing files after a specified date/time and handling hidden directories.
#
#  Usage:
#  Run this script from the command line using:
#      python test/find_range_test.py
#
########################################################################

import argparse
import os
import sys
import unittest
from datetime import datetime, timedelta, timezone
from unittest.mock import call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import find_range


class TestFindRecent(unittest.TestCase):

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_recent_files(self, mock_walk, mock_getmtime, mock_print):
        """Test that only files modified after the specified date are listed."""
        test_start_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)
        test_path = "/path/to/directory"
        include_hidden = False

        # Mock the os.walk function to return a predefined file structure
        mock_walk.return_value = [
            (test_path, ["subdir"], ["recent.txt", "old.txt", ".hidden.txt"]),
        ]

        # Mock os.path.getmtime to return specific modification times for files
        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/recent.txt": test_start_date.timestamp() + 3600,  # 1 hour after the test date
            "/path/to/directory/old.txt": test_start_date.timestamp() - 86400,  # 1 day before the test date
            "/path/to/directory/.hidden.txt": test_start_date.timestamp() + 3600,  # 1 hour after the test date (hidden file)
        }[x]

        # Execute the function with the test parameters
        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False)

        # Assertions to check if the 'print' function was called with the expected file
        # This requires patching 'print' function as well
        # Example: mock_print.assert_called_once_with('2024-02-24 08:00:00 - /path/to/directory/recent.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_hidden_directories_included(self, mock_walk, mock_getmtime, mock_print):
        """Test that hidden files are included when the '-a' option is used."""
        test_path = "/path/to/directory"
        include_hidden = True
        test_start_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["recent.txt", ".hidden_file.txt"]),
        ]
        mock_getmtime.return_value = test_start_date.timestamp() + 3600  # 1 hour after test_start_date

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False)

        mock_print.assert_any_call('2024-02-24T08:00:00Z - /path/to/directory/recent.txt')
        mock_print.assert_any_call('2024-02-24T08:00:00Z - /path/to/directory/.hidden_file.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_hidden_directories_excluded(self, mock_walk, mock_getmtime, mock_print):
        """Test that hidden files are excluded by default."""
        test_path = "/path/to/directory"
        include_hidden = False
        test_start_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["recent.txt", ".hidden_file.txt"]),
        ]
        mock_getmtime.return_value = test_start_date.timestamp() + 3600  # 1 hour after test_start_date

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False)

        mock_print.assert_called_once_with('2024-02-24T08:00:00Z - /path/to/directory/recent.txt')
        # Ensure that the print function was not called for the hidden file

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_recent_files_with_various_file_structures(self, mock_walk, mock_getmtime, mock_print):
        """Test listing of recently modified files with various file structures."""
        test_start_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)
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
            "/path/to/directory/recent1.txt": test_start_date.timestamp() + 3600,
            "/path/to/directory/old1.txt": test_start_date.timestamp() - 86400,
            "/path/to/directory/subdir1/recent2.txt": test_start_date.timestamp() + 7200,
            "/path/to/directory/subdir1/.hidden1.txt": test_start_date.timestamp() + 7200,
            "/path/to/directory/recent3.txt": test_start_date.timestamp() + 10800,
            "/path/to/directory/old2.txt": test_start_date.timestamp() - 86400,
            "/path/to/directory/.hidden2.txt": test_start_date.timestamp() + 10800,
            "/path/to/directory/subdir2/recent4.txt": test_start_date.timestamp() + 14400,
            "/path/to/directory/subdir2/subsubdir1/recent5.txt": test_start_date.timestamp() + 18000,
            "/path/to/directory/subdir2/subsubdir1/old3.txt": test_start_date.timestamp() - 86400,
        }

        # Mock the os.walk function to return the predefined file structures
        mock_walk.return_value = file_structures

        # Mock os.path.getmtime to return specific modification times for files
        mock_getmtime.side_effect = lambda x: modification_times[x]

        # Execute the function with the test parameters
        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False)

        # Assertions to check if the 'print' function was called with the expected files
        expected_calls = [
            call('2024-02-24T08:00:00Z - /path/to/directory/recent1.txt'),
            call('2024-02-24T09:00:00Z - /path/to/directory/subdir1/recent2.txt'),
            call('2024-02-24T10:00:00Z - /path/to/directory/recent3.txt'),
            call('2024-02-24T11:00:00Z - /path/to/directory/subdir2/recent4.txt'),
            call('2024-02-24T12:00:00Z - /path/to/directory/subdir2/subsubdir1/recent5.txt'),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_empty_directory(self, mock_walk, mock_getmtime, mock_print):
        """Test behavior when the specified directory is empty."""
        test_path = "/path/to/empty_directory"
        include_hidden = False
        test_start_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        # Mock the os.walk function to simulate an empty directory
        mock_walk.return_value = [(test_path, [], [])]

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False)

        # Ensure that the print function was not called
        mock_print.assert_not_called()

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_deep_directory_structure(self, mock_walk, mock_getmtime, mock_print):
        """Test behavior with a deep directory structure."""
        test_path = "/path/to/deep/directory/structure"
        include_hidden = False
        test_start_date = datetime(2024, 2, 24, 7, 0, tzinfo=timezone.utc)

        # Mock the os.walk function to return a deep directory structure
        mock_walk.return_value = [
            (test_path, ["subdir1", "subdir2"], ["recent1.txt"]),
            (os.path.join(test_path, "subdir1"), ["subsubdir"], ["recent2.txt"]),
            (os.path.join(test_path, "subdir1", "subsubdir"), [], ["recent3.txt"]),
        ]

        # Define modification times for each file
        modification_times = {
            os.path.join(test_path, "recent1.txt"): test_start_date.timestamp() + 3600,
            os.path.join(test_path, "subdir1", "recent2.txt"): test_start_date.timestamp() + 7200,
            os.path.join(test_path, "subdir1", "subsubdir", "recent3.txt"): test_start_date.timestamp() + 10800,
        }

        mock_getmtime.side_effect = lambda x: modification_times[x]

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False)

        # Assertions for expected output
        expected_calls = [
            call('2024-02-24T08:00:00Z - {}'.format(os.path.join(test_path, "recent1.txt"))),
            call('2024-02-24T09:00:00Z - {}'.format(os.path.join(test_path, "subdir1", "recent2.txt"))),
            call('2024-02-24T10:00:00Z - {}'.format(os.path.join(test_path, "subdir1", "subsubdir", "recent3.txt"))),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_recent_files_with_filenames_only_option(self, mock_walk, mock_getmtime, mock_print):
        """Test that only filenames are listed when the '-f' option is used."""
        test_path = "/path/to/directory"
        include_hidden = False
        filenames_only = True
        test_start_date = datetime(2024, 3, 3, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["recent.txt", "old.txt", ".hidden.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/recent.txt": test_start_date.timestamp() + 3600,  # 1 hour after the test date
            "/path/to/directory/old.txt": test_start_date.timestamp() - 86400,  # 1 day before the test date
            "/path/to/directory/.hidden.txt": test_start_date.timestamp() + 3600,  # 1 hour after the test date (hidden file)
        }[x]

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, filenames_only)

        # Check if the 'print' function was called only with the filename of the recent file
        mock_print.assert_called_once_with('recent.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_files_before_end_date(self, mock_walk, mock_getmtime, mock_print):
        """Tests that files modified before the specified end date are listed."""
        test_start_date = None  # No start date specified
        test_end_date = datetime(2024, 3, 5, 23, 59, tzinfo=timezone.utc)
        test_path = "/path/to/directory"
        include_hidden = False

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_end_date.timestamp() - 3600,  # 1 hour before the test end date
            "/path/to/directory/file2.txt": test_end_date.timestamp() + 3600,  # 1 hour after the test end date
        }[x]

        find_range.list_recent_files(test_path, test_start_date, test_end_date, include_hidden, False)

        mock_print.assert_called_once_with('2024-03-05T22:59:00Z - /path/to/directory/file1.txt')
        self.assertEqual(mock_print.call_count, 1)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_files_before_end_date_only(self, mock_walk, mock_getmtime, mock_print):
        """Tests that files modified before the specified end date only are listed."""
        test_end_date = datetime(2024, 3, 5, tzinfo=timezone.utc)  # End date without time specified
        test_path = "/path/to/directory"
        include_hidden = False

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_end_date.timestamp() - 86400,  # 1 day before the end date
            "/path/to/directory/file2.txt": test_end_date.timestamp() + 3600,  # After the end date
        }[x]

        find_range.list_recent_files(test_path, None, test_end_date, include_hidden, False)

        mock_print.assert_called_once_with('2024-03-04T00:00:00Z - /path/to/directory/file1.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_files_before_end_datetime_only(self, mock_walk, mock_getmtime, mock_print):
        """Tests that files modified before the specified end datetime only are listed."""
        test_end_datetime = datetime(2024, 3, 5, 15, 30, tzinfo=timezone.utc)  # End datetime specified
        test_path = "/path/to/directory"
        include_hidden = False

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_end_datetime.timestamp() - 3600,  # 1 hour before the end datetime
            "/path/to/directory/file2.txt": test_end_datetime.timestamp() + 3600,  # 1 hour after the end datetime
        }[x]

        find_range.list_recent_files(test_path, None, test_end_datetime, include_hidden, False)

        mock_print.assert_called_once_with('2024-03-05T14:30:00Z - /path/to/directory/file1.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_files_within_start_end_datetime_range(self, mock_walk, mock_getmtime, mock_print):
        """Tests that files modified within the specified start and end datetime range are listed, including boundary values."""
        test_start_datetime = datetime(2024, 3, 4, 8, 0, tzinfo=timezone.utc)
        test_end_datetime = datetime(2024, 3, 5, 18, 0, tzinfo=timezone.utc)
        test_path = "/path/to/directory"
        include_hidden = False

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt", "file3.txt", "file4.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_start_datetime.timestamp() - 1,  # Just before start datetime
            "/path/to/directory/file2.txt": test_start_datetime.timestamp(),  # Exactly at start datetime
            "/path/to/directory/file3.txt": test_end_datetime.timestamp(),  # Exactly at end datetime
            "/path/to/directory/file4.txt": test_end_datetime.timestamp() + 1,  # Just after end datetime
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, False)

        expected_calls = [
            call('2024-03-04T08:00:00Z - /path/to/directory/file2.txt'),
            call('2024-03-05T18:00:00Z - /path/to/directory/file3.txt')
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)
        self.assertEqual(mock_print.call_count, 2)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_end_datetime_with_hidden_files_option(self, mock_walk, mock_getmtime, mock_print):
        """Tests listing files before specified end datetime including hidden files."""
        test_end_datetime = datetime(2024, 3, 5, 15, 30, tzinfo=timezone.utc)
        test_path = "/path/to/directory"
        include_hidden = True  # Include hidden files

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", ".hidden1.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_end_datetime.timestamp() - 3600,  # 1 hour before end datetime
            "/path/to/directory/.hidden1.txt": test_end_datetime.timestamp() - 1800,  # 30 minutes before end datetime
        }[x]

        find_range.list_recent_files(test_path, None, test_end_datetime, include_hidden, False)

        expected_calls = [
            call('2024-03-05T14:30:00Z - /path/to/directory/file1.txt'),
            call('2024-03-05T15:00:00Z - /path/to/directory/.hidden1.txt')
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_start_end_datetime_with_filenames_only_option(self, mock_walk, mock_getmtime, mock_print):
        """Tests listing filenames only within specified start and end datetime range."""
        test_start_datetime = datetime(2024, 3, 4, 8, 0, tzinfo=timezone.utc)
        test_end_datetime = datetime(2024, 3, 5, 18, 0, tzinfo=timezone.utc)
        test_path = "/path/to/directory"
        filenames_only = True  # List filenames only

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt", "file3.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_start_datetime.timestamp() + 3600,  # 1 hour after start datetime
            "/path/to/directory/file2.txt": test_end_datetime.timestamp() - 3600,  # 1 hour before end datetime
            "/path/to/directory/file3.txt": test_end_datetime.timestamp() + 3600,  # 1 hour after end datetime
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, False, filenames_only)

        expected_calls = [
            call('file1.txt'),
            call('file2.txt')
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)
        self.assertEqual(mock_print.call_count, 2)

    @patch('find_range.sys.exit')
    @patch('find_range.print')
    def test_invalid_datetime_format(self, mock_print, mock_sys_exit):
        """Test the script exits with error on incorrect datetime format."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(
                datetime=['2024-02-30'], start=None, end=None, path='.', all=False, filenames=False, localtime=False)):
            find_range.main()
            mock_sys_exit.assert_called_with(2)

    @patch('find_range.sys.exit')
    @patch('find_range.print')
    def test_nonexistent_directory_path(self, mock_print, mock_sys_exit):
        """Test the script exits with error when the specified path does not exist."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(
                datetime=['2024-02-25'], start=None, end=None, path='/nonexistent/path', all=False, filenames=False, localtime=False)):
            find_range.main()
            mock_sys_exit.assert_called_with(1)

    @patch('find_range.list_recent_files')
    @patch('find_range.print')
    def test_default_directory_path(self, mock_print, mock_list_recent_files):
        """Test the script uses the current directory as default when no path is specified."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(
                datetime=['2024-02-25'], start=None, end=None, path='.', all=False, filenames=False, localtime=False)):
            find_range.main()
            mock_list_recent_files.assert_called()

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_localtime_option(self, mock_walk, mock_getmtime, mock_print):
        # Set test path and options
        test_path = "/path/to/directory"
        include_hidden = False
        filenames_only = False
        use_localtime = True

        # Set start datetime in local timezone
        test_start_date = datetime(2024, 3, 4, 12, 0).astimezone()

        # Mock the os.walk function to return a predefined file structure
        mock_walk.return_value = [
            (test_path, [], ["file1.txt"]),
        ]

        # Mock os.path.getmtime to return modification time 1 hour after the test start date
        mock_getmtime.return_value = (test_start_date + timedelta(hours=1)).timestamp()

        # Call the function under test with local timezone option
        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, filenames_only, use_localtime)

        # Generate the expected time string in local timezone without timezone information
        expected_time_str = (test_start_date + timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S')
        # Assert the expected print call
        mock_print.assert_called_once_with('{} - {}'.format(expected_time_str, os.path.join(test_path, "file1.txt")))

    @patch('find_range.sys.exit')
    @patch('find_range.print')
    def test_localtime_option_with_incorrect_format(self, mock_print, mock_sys_exit):
        """Test the script exits with error when datetime format is incorrect with '-l' option."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(datetime=['2024-03-32'], start=None, end=None, path='.', all=False, filenames=False, localtime=True)):
            find_range.main()
            mock_sys_exit.assert_called_with(2)

    @patch('find_range.list_recent_files')
    @patch('find_range.print')
    def test_localtime_option_default_directory_path(self, mock_print, mock_list_recent_files):
        """Test the script uses the current directory as default when no path is specified with '-l' option."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(datetime=['2024-02-25'], start=None, end=None, path='.', all=False, filenames=False, localtime=True)):
            find_range.main()
            mock_list_recent_files.assert_called()

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_localtime_range_with_start_and_end(self, mock_walk, mock_getmtime, mock_print):
        """Test listing files within a specified start and end datetime range in local timezone."""
        test_path = "/path/to/directory"
        include_hidden = False
        filenames_only = False
        use_localtime = True

        # Set start and end datetime in local timezone
        test_start_datetime = datetime(2024, 3, 4, 8, 0).astimezone()
        test_end_datetime = datetime(2024, 3, 5, 18, 0).astimezone()

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt", "file3.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": (test_start_datetime + timedelta(hours=1)).timestamp(),  # Within range
            "/path/to/directory/file2.txt": (test_end_datetime - timedelta(hours=1)).timestamp(),  # Within range
            "/path/to/directory/file3.txt": (test_end_datetime + timedelta(hours=1)).timestamp(),  # Outside range
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, filenames_only, use_localtime)

        expected_calls = [
            call('{} - {}'.format((test_start_datetime + timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), os.path.join(test_path, "file1.txt"))),
            call('{} - {}'.format((test_end_datetime - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), os.path.join(test_path, "file2.txt")))
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)
        self.assertEqual(mock_print.call_count, 2)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_files_within_start_end_datetime_range_localtime(self, mock_walk, mock_getmtime, mock_print):
        """Tests that files modified within the specified start and end datetime range in local timezone are listed."""
        test_start_datetime = datetime(2024, 3, 4, 8, 0).astimezone()
        test_end_datetime = datetime(2024, 3, 5, 18, 0).astimezone()
        test_path = "/path/to/directory"
        include_hidden = False
        use_localtime = True

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt", "file3.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": (test_start_datetime + timedelta(hours=1)).timestamp(),  # Within range
            "/path/to/directory/file2.txt": (test_end_datetime - timedelta(hours=1)).timestamp(),  # Within range
            "/path/to/directory/file3.txt": (test_end_datetime + timedelta(hours=1)).timestamp(),  # Out of range
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, False, use_localtime)

        expected_calls = [
            call('{} - {}'.format((test_start_datetime + timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), os.path.join(test_path, "file1.txt"))),
            call('{} - {}'.format((test_end_datetime - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), os.path.join(test_path, "file2.txt"))),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)
        self.assertEqual(mock_print.call_count, 2)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_hidden_files_included_with_localtime(self, mock_walk, mock_getmtime, mock_print):
        """Test that hidden files are included when the '-a' option is used with local timezone."""
        test_path = "/path/to/directory"
        include_hidden = True
        use_localtime = True
        test_end_datetime = datetime.now().astimezone() + timedelta(hours=1)  # Future time in local timezone

        mock_walk.return_value = [
            (test_path, [], ["visible_file.txt", ".hidden_file.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: (datetime.now().astimezone() - timedelta(hours=1)).timestamp()

        find_range.list_recent_files(test_path, None, test_end_datetime, include_hidden, False, use_localtime)

        expected_calls = [
            call('{} - {}'.format((datetime.now().astimezone() - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), os.path.join(test_path, "visible_file.txt"))),
            call('{} - {}'.format((datetime.now().astimezone() - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), os.path.join(test_path, ".hidden_file.txt"))),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_filenames_only_with_localtime(self, mock_walk, mock_getmtime, mock_print):
        """Test listing filenames only with local timezone."""
        test_start_datetime = datetime.now().astimezone() - timedelta(hours=2)
        test_end_datetime = datetime.now().astimezone()
        test_path = "/path/to/directory"
        include_hidden = False
        filenames_only = True
        use_localtime = True

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            os.path.join(test_path, "file1.txt"): (test_start_datetime + timedelta(hours=1)).timestamp(),  # Within range
            os.path.join(test_path, "file2.txt"): (test_end_datetime + timedelta(hours=1)).timestamp(),  # Out of range
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, filenames_only, use_localtime)

        mock_print.assert_called_once_with("file1.txt")

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_exclude_hidden_files_and_directories(self, mock_walk, mock_getmtime, mock_print):
        """Test that hidden files and directories are excluded by default."""
        test_path = "/path/to/directory"
        include_hidden = False
        test_datetime = datetime.utcnow().replace(tzinfo=timezone.utc) - timedelta(hours=2)

        mock_walk.return_value = [
            (test_path, [".hidden_dir"], ["visible_file.txt", ".hidden_file.txt"]),
            (os.path.join(test_path, ".hidden_dir"), [], ["hidden_file_in_hidden_dir.txt"]),
        ]

        mock_getmtime.return_value = test_datetime.timestamp()

        find_range.list_recent_files(test_path, test_datetime, None, include_hidden, False, False)

        expected_output = '{} - {}'.format(test_datetime.strftime('%Y-%m-%dT%H:%M:%SZ'), os.path.join(test_path, "visible_file.txt"))
        mock_print.assert_called_once_with(expected_output)


if __name__ == '__main__':
    unittest.main()
