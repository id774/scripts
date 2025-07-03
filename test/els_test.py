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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-01-31
#       Initial release. Test suite for els.py script.
#
########################################################################

import os
import subprocess
import sys
import time
import unittest
from unittest.mock import MagicMock, patch

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import els


class TestEls(unittest.TestCase):
    """ Test suite for els.py """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'els.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    # Test Cases for format_time()
    def test_format_time(self):
        """ Ensure timestamps are converted to 'YYYY-MM-DD HH:MM:SS' in local time. """
        test_timestamp = 1706700000  # Corresponds to 2024-02-01 00:00:00 UTC
        expected_output = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(test_timestamp))
        self.assertEqual(els.format_time(test_timestamp), expected_output)

    def test_format_time_none(self):
        """ Ensure format_time() handles None gracefully by raising ValueError. """
        with self.assertRaises(ValueError):
            els.format_time(None)

    def test_format_time_negative(self):
        """ Ensure format_time() handles timestamps before 1970. """
        test_timestamp = -1000000000  # Before Unix epoch
        expected_output = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(test_timestamp))
        self.assertEqual(els.format_time(test_timestamp), expected_output)

    def test_format_time_float(self):
        """ Ensure format_time() handles float timestamps. """
        test_timestamp = 1706700000.123456  # Float timestamp
        expected_output = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(test_timestamp))
        self.assertEqual(els.format_time(test_timestamp), expected_output)

    # Test Cases for get_file_info()
    @patch('os.path.isfile', return_value=True)
    @patch('os.stat')
    def test_get_file_info_single_file(self, mock_stat, mock_isfile):
        """ Ensure get_file_info() returns correct metadata for a single file. """
        mock_stat.return_value.st_mode = 0o100644
        mock_stat.return_value.st_size = 1234
        mock_stat.return_value.st_uid = 1000
        mock_stat.return_value.st_gid = 1000
        mock_stat.return_value.st_atime = 1706700000
        mock_stat.return_value.st_mtime = 1706700000
        mock_stat.return_value.st_ctime = 1706700000
        mock_stat.return_value.st_birthtime = 1706700000  # macOS only

        with patch('els.get_owner', return_value="testuser"), \
                patch('els.get_group', return_value="testgroup"), \
                patch('stat.filemode', return_value="-rw-r--r--"):

            result = els.get_file_info("testfile.txt")

        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["name"], "testfile.txt")

    @patch('os.path.islink', return_value=True)
    @patch('os.path.isfile', return_value=True)
    @patch('os.stat')
    def test_get_file_info_symlink(self, mock_stat, mock_isfile, mock_islink):
        """ Ensure get_file_info() correctly handles symbolic links. """
        mock_stat.return_value.st_mode = 0o120777  # Symlink mode
        mock_stat.return_value.st_size = 0
        mock_stat.return_value.st_uid = 1000
        mock_stat.return_value.st_gid = 1000
        mock_stat.return_value.st_atime = 1706700000
        mock_stat.return_value.st_mtime = 1706700000
        mock_stat.return_value.st_ctime = 1706700000

        with patch('els.get_owner', return_value="testuser"), \
                patch('els.get_group', return_value="testgroup"), \
                patch('stat.filemode', return_value="lrwxrwxrwx"):

            result = els.get_file_info("symlink.txt")

        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["mode"], "lrwxrwxrwx")

    @patch('os.scandir')
    @patch('os.path.exists', return_value=True)
    @patch('os.path.isdir', return_value=True)
    def test_get_file_info_directory(self, mock_isdir, mock_exists, mock_scandir):
        """ Ensure get_file_info() returns correct metadata for a directory. """
        mock_entry = MagicMock()
        mock_entry.path = "/fake/path/file.txt"
        mock_scandir.return_value.__enter__.return_value = [mock_entry]

        with patch('els.format_file_entry', return_value={"name": "file.txt"}):
            result = els.get_file_info("/fake/path")

        self.assertEqual(result, [{"name": "file.txt"}])

    @patch('os.path.exists', return_value=False)
    def test_get_file_info_not_found(self, mock_exists):
        """ Ensure get_file_info() handles nonexistent paths correctly. """
        result = els.get_file_info("/nonexistent")
        self.assertEqual(result, "[ERROR] '/nonexistent' does not exist.")

    @patch('os.scandir', side_effect=PermissionError)
    @patch('os.path.exists', return_value=True)
    @patch('os.path.isdir', return_value=True)
    def test_get_file_info_permission_error(self, mock_isdir, mock_exists, mock_scandir):
        """ Ensure get_file_info() handles permission errors correctly. """
        result = els.get_file_info("/restricted")
        self.assertEqual(result, "[ERROR] Permission denied for '/restricted'.")


if __name__ == '__main__':
    unittest.main()
