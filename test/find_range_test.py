#!/usr/bin/env python

########################################################################
# find_range_test.py: Test script for find_range.py
#
#  Description:
#  Tests the functionality of the find_range.py script, ensuring it correctly
#  lists files modified after a specified date and time, and handles hidden
#  directories and various options as specified. It now includes tests for
#  the newly added '-fp' option to verify that the full path and filename
#  are listed without modification time. Tests cover local timezone handling
#  with the '-l' option, ensuring both UTC and local timezone outputs are
#  correctly formatted and calculated.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.8 2024-11-16
#       Updated test cases to include support for the new '-fp' option.
#       Verified proper function with full path output without modification time.
#  v1.7 2024-06-07
#       Added new test cases to detect issues with local timezone handling,
#       ensuring proper functionality for the '-l' option.
#  v1.6 2024-03-28
#       Updated tests to accommodate changes in output format for local timezone
#       that now includes timezone offset, aligning with ISO 8601.
#  v1.5 2024-03-19
#       Replaced deprecated datetime.utcnow() with datetime.now(timezone.utc) in tests.
#  v1.4 2024-03-16
#       Added test cases for local timezone handling.
#       Added more specific tests for ensuring hidden directories and their files
#       are properly excluded by default and included when using the '-a' option.
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
        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False, False)

        # Assertions to check if the 'print' function was called with the expected file
        mock_print.assert_called_once_with('2024-02-24T08:00:00Z - /path/to/directory/recent.txt')

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

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False, False)

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

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False, False)

        mock_print.assert_called_once_with('2024-02-24T08:00:00Z - /path/to/directory/recent.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_filenames_only_option(self, mock_walk, mock_getmtime, mock_print):
        """Test that only filenames are listed when the '-f' option is used."""
        test_path = "/path/to/directory"
        include_hidden = False
        filenames_only = True
        test_start_date = datetime(2024, 3, 3, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["recent.txt", "old.txt", ".hidden.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/recent.txt": test_start_date.timestamp() + 3600,
            "/path/to/directory/old.txt": test_start_date.timestamp() - 86400,
            "/path/to/directory/.hidden.txt": test_start_date.timestamp() + 3600,
        }[x]

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, filenames_only, False)

        mock_print.assert_called_once_with('recent.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_fullpath_only_option(self, mock_walk, mock_getmtime, mock_print):
        """Test that only full paths are listed when the '-fp' option is used."""
        test_path = "/path/to/directory"
        include_hidden = False
        fullpath_only = True
        test_start_date = datetime(2024, 3, 3, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["recent.txt", "old.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/recent.txt": test_start_date.timestamp() + 3600,
            "/path/to/directory/old.txt": test_start_date.timestamp() - 86400,
        }[x]

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, False, fullpath_only)

        mock_print.assert_called_once_with('/path/to/directory/recent.txt')

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

        find_range.list_recent_files(test_path, test_start_date, test_end_date, include_hidden, False, False)

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

        find_range.list_recent_files(test_path, None, test_end_date, include_hidden, False, False)

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

        find_range.list_recent_files(test_path, None, test_end_datetime, include_hidden, False, False)

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

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, False, False)

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

        find_range.list_recent_files(test_path, None, test_end_datetime, include_hidden, False, False)

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

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, False, filenames_only, False)

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
                datetime=['2024-02-30'], start=None, end=None, path='.', all=False, filenames=False, fullpath=False, localtime=False)):
            find_range.main()
            mock_sys_exit.assert_called_with(2)

    @patch('find_range.sys.exit')
    @patch('find_range.print')
    def test_nonexistent_directory_path(self, mock_print, mock_sys_exit):
        """Test the script exits with error when the specified path does not exist."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(
                datetime=['2024-02-25'], start=None, end=None, path='/nonexistent/path', all=False, filenames=False, fullpath=False, localtime=False)):
            find_range.main()
            mock_sys_exit.assert_called_with(1)

    @patch('find_range.list_recent_files')
    @patch('find_range.print')
    def test_default_directory_path(self, mock_print, mock_list_recent_files):
        """Test the script uses the current directory as default when no path is specified."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(
                datetime=['2024-02-25'], start=None, end=None, path='.', all=False, filenames=False, fullpath=False, localtime=False)):
            find_range.main()
            mock_list_recent_files.assert_called()

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_localtime_option(self, mock_walk, mock_getmtime, mock_print):
        """Test listing files with the local timezone option."""
        # Set test path and options
        test_path = "/path/to/directory"
        include_hidden = False
        filenames_only = False
        use_localtime = True

        # Set start datetime in local timezone
        test_start_date = datetime(2024, 3, 4, 13, 0).astimezone()

        mock_walk.return_value = [
            (test_path, [], ["file1.txt"]),
        ]

        mock_getmtime.return_value = test_start_date.timestamp()

        find_range.list_recent_files(test_path, test_start_date, None, include_hidden, filenames_only, False, use_localtime)

        tz_offset = test_start_date.strftime('%z')
        tz_formatted = "{}:{}".format(tz_offset[:-2], tz_offset[-2:])  # Format to '±hh:mm'

        expected_time_str = '{}{}'.format(test_start_date.strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted)
        mock_print.assert_called_once_with('{} - {}'.format(expected_time_str, os.path.join(test_path, "file1.txt")))

    @patch('find_range.sys.exit')
    @patch('find_range.print')
    def test_localtime_option_with_incorrect_format(self, mock_print, mock_sys_exit):
        """Test the script exits with error when datetime format is incorrect with '-l' option."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(
                datetime=['2024-03-32'], start=None, end=None, path='.', all=False, filenames=False, fullpath=False, localtime=True)):
            find_range.main()
            mock_sys_exit.assert_called_with(2)

    @patch('find_range.list_recent_files')
    @patch('find_range.print')
    def test_localtime_option_default_directory_path(self, mock_print, mock_list_recent_files):
        """Test the script uses the current directory as default when no path is specified with '-l' option."""
        with patch('find_range.parse_arguments', return_value=argparse.Namespace(
                datetime=['2024-02-25'], start=None, end=None, path='.', all=False, filenames=False, fullpath=False, localtime=True)):
            find_range.main()
            mock_list_recent_files.assert_called()

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_localtime_range_with_start_and_end(self, mock_walk, mock_getmtime, mock_print):
        """Test listing files within a specified start and end datetime range in local timezone."""
        test_path = "/path/to/directory"
        include_hidden = False
        use_localtime = True

        test_start_datetime = datetime(2024, 3, 4, 9, 0).astimezone()
        test_end_datetime = datetime(2024, 3, 5, 17, 0).astimezone()

        mock_walk.return_value = [
            (test_path, [], [
                "file_far_before_start.txt",
                "file_just_before_start.txt",
                "file_at_start.txt",
                "file_just_after_start.txt",
                "file_within_range1.txt",
                "file_within_range2.txt",
                "file_just_before_end.txt",
                "file_at_end.txt",
                "file_just_after_end.txt",
                "file_far_after_end.txt",
            ]),
        ]

        mock_getmtime.side_effect = lambda x: {
            os.path.join(test_path, "file_far_before_start.txt"): (test_start_datetime - timedelta(days=1)).timestamp(),
            os.path.join(test_path, "file_just_before_start.txt"): (test_start_datetime - timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_at_start.txt"): test_start_datetime.timestamp(),
            os.path.join(test_path, "file_just_after_start.txt"): (test_start_datetime + timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_within_range1.txt"): (test_start_datetime + timedelta(hours=1)).timestamp(),
            os.path.join(test_path, "file_within_range2.txt"): (test_end_datetime - timedelta(hours=1)).timestamp(),
            os.path.join(test_path, "file_just_before_end.txt"): (test_end_datetime - timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_at_end.txt"): test_end_datetime.timestamp(),
            os.path.join(test_path, "file_just_after_end.txt"): (test_end_datetime + timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_far_after_end.txt"): (test_end_datetime + timedelta(days=1)).timestamp(),
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, False, False, use_localtime)

        tz_offset = test_start_datetime.strftime('%z')
        tz_formatted = "{}:{}".format(tz_offset[:-2], tz_offset[-2:])

        expected_calls = [
            call('{}{} - {}'.format(test_start_datetime.strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_at_start.txt"))),
            call('{}{} - {}'.format((test_start_datetime + timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_after_start.txt"))),
            call('{}{} - {}'.format((test_start_datetime + timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_within_range1.txt"))),
            call('{}{} - {}'.format((test_end_datetime - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_within_range2.txt"))),
            call('{}{} - {}'.format((test_end_datetime - timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_before_end.txt"))),
            call('{}{} - {}'.format(test_end_datetime.strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_at_end.txt"))),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

        unexpected_calls = [
            call('{}{} - {}'.format((test_start_datetime - timedelta(days=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_far_before_start.txt"))),
            call('{}{} - {}'.format((test_start_datetime - timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_before_start.txt"))),
            call('{}{} - {}'.format((test_end_datetime + timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_after_end.txt"))),
            call('{}{} - {}'.format((test_end_datetime + timedelta(days=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_far_after_end.txt"))),
        ]
        for unexpected_call in unexpected_calls:
            self.assertNotIn(unexpected_call, mock_print.mock_calls)

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

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, False, False, use_localtime)

        tz_offset = test_start_datetime.strftime('%z')
        tz_formatted = "{}:{}".format(tz_offset[:-2], tz_offset[-2:])

        expected_calls = [
            call('{}{} - {}'.format((test_start_datetime + timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file1.txt"))),
            call('{}{} - {}'.format((test_end_datetime - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file2.txt"))),
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

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

        find_range.list_recent_files(test_path, None, test_end_datetime, include_hidden, False, False, use_localtime)

        tz_offset = test_end_datetime.strftime('%z')
        tz_formatted = "{}:{}".format(tz_offset[:-2], tz_offset[-2:])

        expected_calls = [
            call('{}{} - {}'.format((datetime.now().astimezone() - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "visible_file.txt"))),
            call('{}{} - {}'.format((datetime.now().astimezone() - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, ".hidden_file.txt"))),
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

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, filenames_only, False, use_localtime)

        mock_print.assert_called_once_with("file1.txt")

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_exclude_hidden_files_and_directories(self, mock_walk, mock_getmtime, mock_print):
        """Test that hidden files and directories are excluded by default."""
        test_path = "/path/to/directory"
        include_hidden = False
        test_datetime = datetime.now(timezone.utc) - timedelta(hours=2)

        mock_walk.return_value = [
            (test_path, [".hidden_dir"], ["visible_file.txt", ".hidden_file.txt"]),
            (os.path.join(test_path, ".hidden_dir"), [], ["hidden_file_in_hidden_dir.txt"]),
        ]

        mock_getmtime.return_value = test_datetime.timestamp()

        find_range.list_recent_files(test_path, test_datetime, None, include_hidden, False, False)

        expected_output = '{} - {}'.format(test_datetime.strftime('%Y-%m-%dT%H:%M:%SZ'), os.path.join(test_path, "visible_file.txt"))
        mock_print.assert_called_once_with(expected_output)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_hidden_directory_files_excluded_by_default(self, mock_walk, mock_getmtime, mock_print):
        """Test that files in hidden directories are excluded by default."""
        test_path = "/path/to/directory"
        include_hidden = False
        test_datetime = datetime.now(timezone.utc) - timedelta(hours=2)

        mock_walk.return_value = [
            (test_path, [".hidden_dir"], ["visible_file.txt"]),
            (os.path.join(test_path, ".hidden_dir"), [], ["hidden_file.txt"]),
        ]

        mock_getmtime.return_value = test_datetime.timestamp()

        find_range.list_recent_files(test_path, test_datetime, None, include_hidden, False, False)

        expected_output = '{} - {}'.format(test_datetime.strftime('%Y-%m-%dT%H:%M:%SZ'), os.path.join(test_path, "visible_file.txt"))
        mock_print.assert_called_once_with(expected_output)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_hidden_directory_files_included_with_all_option(self, mock_walk, mock_getmtime, mock_print):
        """Test that files in hidden directories are included when the '-a' option is used."""
        test_path = "/path/to/directory"
        include_hidden = True
        test_datetime = datetime.now(timezone.utc) - timedelta(hours=2)

        mock_walk.return_value = [
            (test_path, [".hidden_dir"], ["visible_file.txt"]),
            (os.path.join(test_path, ".hidden_dir"), [], ["hidden_file.txt"]),
        ]

        mock_getmtime.return_value = test_datetime.timestamp()

        find_range.list_recent_files(test_path, test_datetime, None, include_hidden, False, False)

        expected_outputs = [
            '{} - {}'.format(test_datetime.strftime('%Y-%m-%dT%H:%M:%SZ'), os.path.join(test_path, "visible_file.txt")),
            '{} - {}'.format(test_datetime.strftime('%Y-%m-%dT%H:%M:%SZ'), os.path.join(test_path, ".hidden_dir", "hidden_file.txt"))
        ]
        mock_print.assert_has_calls([call(output) for output in expected_outputs], any_order=True)
        self.assertEqual(mock_print.call_count, len(expected_outputs))

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_files_within_localtime_range_with_start_and_end(self, mock_walk, mock_getmtime, mock_print):
        """Test listing files within a specified start and end datetime range in local timezone."""
        test_path = "/path/to/directory"
        include_hidden = False
        use_localtime = True

        test_start_datetime = datetime(2024, 3, 4, 9, 0).astimezone()
        test_end_datetime = datetime(2024, 3, 5, 17, 0).astimezone()

        mock_walk.return_value = [
            (test_path, [], [
                "file_far_before_start.txt",
                "file_just_before_start.txt",
                "file_at_start.txt",
                "file_just_after_start.txt",
                "file_within_range1.txt",
                "file_within_range2.txt",
                "file_just_before_end.txt",
                "file_at_end.txt",
                "file_just_after_end.txt",
                "file_far_after_end.txt",
            ]),
        ]

        # Mock file modification times around and within the start and end datetime
        mock_getmtime.side_effect = lambda x: {
            os.path.join(test_path, "file_far_before_start.txt"): (test_start_datetime - timedelta(days=1)).timestamp(),
            os.path.join(test_path, "file_just_before_start.txt"): (test_start_datetime - timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_at_start.txt"): test_start_datetime.timestamp(),
            os.path.join(test_path, "file_just_after_start.txt"): (test_start_datetime + timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_within_range1.txt"): (test_start_datetime + timedelta(hours=1)).timestamp(),
            os.path.join(test_path, "file_within_range2.txt"): (test_end_datetime - timedelta(hours=1)).timestamp(),
            os.path.join(test_path, "file_just_before_end.txt"): (test_end_datetime - timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_at_end.txt"): test_end_datetime.timestamp(),
            os.path.join(test_path, "file_just_after_end.txt"): (test_end_datetime + timedelta(minutes=1)).timestamp(),
            os.path.join(test_path, "file_far_after_end.txt"): (test_end_datetime + timedelta(days=1)).timestamp(),
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, test_end_datetime, include_hidden, False, False, use_localtime)

        tz_offset = test_start_datetime.strftime('%z')
        tz_formatted = "{}:{}".format(tz_offset[:-2], tz_offset[-2:])  # Format to '±hh:mm'

        expected_calls = [
            call('{}{} - {}'.format(test_start_datetime.strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_at_start.txt"))),
            call('{}{} - {}'.format((test_start_datetime + timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_after_start.txt"))),
            call('{}{} - {}'.format((test_start_datetime + timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_within_range1.txt"))),
            call('{}{} - {}'.format((test_end_datetime - timedelta(hours=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_within_range2.txt"))),
            call('{}{} - {}'.format((test_end_datetime - timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_before_end.txt"))),
            call('{}{} - {}'.format(test_end_datetime.strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_at_end.txt"))),
        ]
        # Ensure expected files within and at the boundaries are printed
        mock_print.assert_has_calls(expected_calls, any_order=True)

        # Ensure files outside the specified range are not printed
        unexpected_calls = [
            call('{}{} - {}'.format((test_start_datetime - timedelta(days=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_far_before_start.txt"))),
            call('{}{} - {}'.format((test_start_datetime - timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_before_start.txt"))),
            call('{}{} - {}'.format((test_end_datetime + timedelta(minutes=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_just_after_end.txt"))),
            call('{}{} - {}'.format((test_end_datetime + timedelta(days=1)).strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, os.path.join(test_path, "file_far_after_end.txt"))),
        ]
        for unexpected_call in unexpected_calls:
            self.assertNotIn(unexpected_call, mock_print.mock_calls)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_fp_option_with_localtime(self, mock_walk, mock_getmtime, mock_print):
        """Test that the '-fp' option works correctly with local timezone."""
        test_path = "/path/to/directory"
        include_hidden = False
        fullpath_only = True
        use_localtime = True
        test_start_datetime = datetime(2024, 3, 4, 8, 0).astimezone()

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_start_datetime.timestamp() + 3600,
            "/path/to/directory/file2.txt": test_start_datetime.timestamp() - 3600,
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, None, include_hidden, False, fullpath_only, use_localtime)

        mock_print.assert_called_once_with('/path/to/directory/file1.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_fp_option_with_hidden_files(self, mock_walk, mock_getmtime, mock_print):
        """Test that the '-fp' option includes hidden files when '-a' is specified."""
        test_path = "/path/to/directory"
        include_hidden = True
        fullpath_only = True
        test_start_datetime = datetime(2024, 3, 4, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", ".hidden_file.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_start_datetime.timestamp() + 3600,
            "/path/to/directory/.hidden_file.txt": test_start_datetime.timestamp() + 7200,
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, None, include_hidden, False, fullpath_only)

        expected_calls = [
            call('/path/to/directory/file1.txt'),
            call('/path/to/directory/.hidden_file.txt')
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_fp_option_with_complex_path_structure(self, mock_walk, mock_getmtime, mock_print):
        """Test that the '-fp' option outputs full paths correctly for complex path structures."""
        test_path = "/path/to/directory"
        fullpath_only = True
        test_start_datetime = datetime(2024, 3, 4, 8, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, ["subdir"], ["file1.txt"]),
            (os.path.join(test_path, "subdir"), [], ["file2.txt"]),
        ]

        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_start_datetime.timestamp() + 3600,
            "/path/to/directory/subdir/file2.txt": test_start_datetime.timestamp() + 7200,
        }[x]

        find_range.list_recent_files(test_path, test_start_datetime, None, False, False, fullpath_only)

        expected_calls = [
            call('/path/to/directory/file1.txt'),
            call('/path/to/directory/subdir/file2.txt')
        ]
        mock_print.assert_has_calls(expected_calls, any_order=True)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_empty_directory(self, mock_walk, mock_getmtime, mock_print):
        """Test that an empty directory does not produce any output."""
        test_path = "/path/to/empty_directory"
        mock_walk.return_value = [
            (test_path, [], [])  # No files or subdirectories
        ]

        find_range.list_recent_files(test_path, datetime(2024, 3, 1, 7, 0, tzinfo=timezone.utc), None, False, False, False)
        mock_print.assert_not_called()

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_recent_files_with_various_file_structures(self, mock_walk, mock_getmtime, mock_print):
        """Test listing recent files in a directory with various file structures."""
        test_path = "/path/to/directory"
        mock_walk.return_value = [
            (test_path, ["subdir"], ["recent1.txt", "old1.txt"]),
            (os.path.join(test_path, "subdir"), [], ["recent2.txt", "old2.txt"])
        ]

        test_datetime = datetime(2024, 3, 1, 7, 0, tzinfo=timezone.utc)
        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/recent1.txt": test_datetime.timestamp() + 3600,
            "/path/to/directory/old1.txt": test_datetime.timestamp() - 86400,
            "/path/to/directory/subdir/recent2.txt": test_datetime.timestamp() + 7200,
            "/path/to/directory/subdir/old2.txt": test_datetime.timestamp() - 172800
        }[x]

        find_range.list_recent_files(test_path, test_datetime, None, False, False, False)
        mock_print.assert_any_call('2024-03-01T08:00:00Z - /path/to/directory/recent1.txt')
        mock_print.assert_any_call('2024-03-01T09:00:00Z - /path/to/directory/subdir/recent2.txt')
        self.assertEqual(mock_print.call_count, 2)

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_recent_files_with_filenames_only_option(self, mock_walk, mock_getmtime, mock_print):
        """Test that only filenames are listed when the '-f' option is used."""
        test_path = "/path/to/directory"
        filenames_only = True
        test_datetime = datetime(2024, 3, 1, 7, 0, tzinfo=timezone.utc)

        mock_walk.return_value = [
            (test_path, [], ["file1.txt", "file2.txt"])
        ]
        mock_getmtime.side_effect = lambda x: {
            "/path/to/directory/file1.txt": test_datetime.timestamp() + 3600,
            "/path/to/directory/file2.txt": test_datetime.timestamp() - 86400
        }[x]

        find_range.list_recent_files(test_path, test_datetime, None, False, filenames_only, False)
        mock_print.assert_called_once_with('file1.txt')

    @patch('builtins.print')
    @patch('find_range.os.path.getmtime')
    @patch('find_range.os.walk')
    def test_deep_directory_structure(self, mock_walk, mock_getmtime, mock_print):
        """Test listing recent files in a deep directory structure."""
        test_path = "/path/to/deep_directory"
        mock_walk.return_value = [
            (test_path, ["level1"], []),
            (os.path.join(test_path, "level1"), ["level2"], ["file1.txt"]),
            (os.path.join(test_path, "level1", "level2"), [], ["file2.txt"])
        ]

        test_datetime = datetime(2024, 3, 1, 7, 0, tzinfo=timezone.utc)
        mock_getmtime.side_effect = lambda x: {
            "/path/to/deep_directory/level1/file1.txt": test_datetime.timestamp() + 3600,
            "/path/to/deep_directory/level1/level2/file2.txt": test_datetime.timestamp() - 86400
        }[x]

        find_range.list_recent_files(test_path, test_datetime, None, False, False, False)
        mock_print.assert_called_once_with('2024-03-01T08:00:00Z - /path/to/deep_directory/level1/file1.txt')


if __name__ == '__main__':
    unittest.main()
