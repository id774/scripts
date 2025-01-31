#!/usr/bin/env python

########################################################################
# els_test.py: Test suite for els.py
#
#  Description:
#  This test suite verifies the functionality of the els.py script.
#  It tests individual functions such as file metadata retrieval,
#  timestamp formatting, and directory listing while ensuring correct
#  error handling and edge cases. The test suite does not modify
#  actual file system contents but uses mocking where applicable.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-31
#       Initial release. Test suite for els.py script.
#
########################################################################

import os
import sys
import unittest
import time
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from els import format_time, get_owner, get_group, format_file_entry, list_directory


class TestEls(unittest.TestCase):
    """Test suite for els.py"""

    def test_format_time(self):
        """
        Test case: Correct formatting of timestamps.
        Ensures timestamps are converted to 'YYYY-MM-DD HH:MM:SS' format.
        """
        test_timestamp = 1706700000  # Corresponds to 2024-02-01 00:00:00 UTC
        expected_output = "2024-02-01 00:00:00"
        self.assertEqual(format_time(test_timestamp), expected_output)

    @patch('pwd.getpwuid')
    def test_get_owner(self, mock_getpwuid):
        """
        Test case: Retrieve user name from UID.
        Ensures get_owner() returns the correct user name or UID as a string.
        """
        mock_getpwuid.return_value.pw_name = "testuser"
        self.assertEqual(get_owner(1000), "testuser")

        # Simulate unknown UID
        mock_getpwuid.side_effect = KeyError
        self.assertEqual(get_owner(9999), "9999")

    @patch('grp.getgrgid')
    def test_get_group(self, mock_getgrgid):
        """
        Test case: Retrieve group name from GID.
        Ensures get_group() returns the correct group name or GID as a string.
        """
        mock_getgrgid.return_value.gr_name = "testgroup"
        self.assertEqual(get_group(1000), "testgroup")

        # Simulate unknown GID
        mock_getgrgid.side_effect = KeyError
        self.assertEqual(get_group(9999), "9999")

    @patch('os.stat')
    def test_format_file_entry(self, mock_stat):
        """
        Test case: Verify file metadata formatting.
        Ensures format_file_entry() returns a correctly structured dictionary.
        """
        mock_stat.return_value.st_mode = 0o100644
        mock_stat.return_value.st_size = 1234
        mock_stat.return_value.st_uid = 1000
        mock_stat.return_value.st_gid = 1000
        mock_stat.return_value.st_atime = 1706700000
        mock_stat.return_value.st_mtime = 1706700000
        mock_stat.return_value.st_ctime = 1706700000
        mock_stat.return_value.st_birthtime = 1706700000  # macOS only

        mock_entry = MagicMock()
        mock_entry.stat.return_value = mock_stat.return_value
        mock_entry.name = "testfile.txt"

        expected_output = {
            "mode": "-rw-r--r--",
            "size": 1234,
            "owner": "testuser",
            "group": "testgroup",
            "atime": "2024-02-01 00:00:00",
            "mtime": "2024-02-01 00:00:00",
            "ctime": "2024-02-01 00:00:00",
            "birth": "2024-02-01 00:00:00",
            "name": "testfile.txt"
        }

        with patch('els.get_owner', return_value="testuser"), \
             patch('els.get_group', return_value="testgroup"), \
             patch('stat.filemode', return_value="-rw-r--r--"):
            self.assertEqual(format_file_entry(mock_entry), expected_output)

    @patch('os.scandir')
    def test_list_directory(self, mock_scandir):
        """
        Test case: Verify directory listing.
        Ensures list_directory() returns correct metadata for multiple files.
        """
        mock_entry1 = MagicMock()
        mock_entry1.name = "file1.txt"
        mock_entry1.stat.return_value = MagicMock(
            st_mode=0o100644, st_size=1234, st_uid=1000, st_gid=1000,
            st_atime=1706700000, st_mtime=1706700000, st_ctime=1706700000,
            st_birthtime=1706700000  # macOS only
        )

        mock_entry2 = MagicMock()
        mock_entry2.name = "file2.txt"
        mock_entry2.stat.return_value = MagicMock(
            st_mode=0o100600, st_size=4321, st_uid=1001, st_gid=1001,
            st_atime=1706700000, st_mtime=1706700000, st_ctime=1706700000,
            st_birthtime=1706700000  # macOS only
        )

        mock_scandir.return_value.__enter__.return_value = [mock_entry1, mock_entry2]

        with patch('els.get_owner', side_effect=["testuser1", "testuser2"]), \
             patch('els.get_group', side_effect=["testgroup1", "testgroup2"]), \
             patch('stat.filemode', side_effect=["-rw-r--r--", "-rw-------"]):

            result = list_directory("/fake/path")

        expected_result = [
            {
                "mode": "-rw-r--r--",
                "size": 1234,
                "owner": "testuser1",
                "group": "testgroup1",
                "atime": "2024-02-01 00:00:00",
                "mtime": "2024-02-01 00:00:00",
                "ctime": "2024-02-01 00:00:00",
                "birth": "2024-02-01 00:00:00",
                "name": "file1.txt"
            },
            {
                "mode": "-rw-------",
                "size": 4321,
                "owner": "testuser2",
                "group": "testgroup2",
                "atime": "2024-02-01 00:00:00",
                "mtime": "2024-02-01 00:00:00",
                "ctime": "2024-02-01 00:00:00",
                "birth": "2024-02-01 00:00:00",
                "name": "file2.txt"
            }
        ]

        self.assertEqual(result, expected_result)

    def test_list_directory_error(self):
        """
        Test case: Handle errors in directory listing.
        Ensures list_directory() correctly handles non-existent and inaccessible paths.
        """
        self.assertEqual(list_directory("/non/existent/path"), "Error: '/non/existent/path' does not exist.")


if __name__ == '__main__':
    unittest.main()
