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
import time
import unittest
from unittest.mock import MagicMock, patch

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import els


class TestEls(unittest.TestCase):
    """Test suite for els.py"""

    def test_format_time(self):
        """Ensure timestamps are converted to 'YYYY-MM-DD HH:MM:SS' in local time."""
        test_timestamp = 1706700000  # Corresponds to 2024-02-01 00:00:00 UTC
        expected_output = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(test_timestamp))
        self.assertEqual(els.format_time(test_timestamp), expected_output)

    @patch('os.scandir')
    @patch('os.path.exists', return_value=True)
    @patch('os.path.isdir', return_value=True)
    def test_get_file_info_directory(self, mock_isdir, mock_exists, mock_scandir):
        """Ensure get_file_info() returns correct metadata for a directory."""
        mock_entry = MagicMock()
        mock_entry.path = "/fake/path/file.txt"
        mock_scandir.return_value.__enter__.return_value = [mock_entry]

        with patch('els.format_file_entry', return_value={"name": "file.txt"}):
            result = els.get_file_info("/fake/path")

        self.assertEqual(result, [{"name": "file.txt"}])

    @patch('os.path.exists', return_value=False)
    def test_get_file_info_not_found(self, mock_exists):
        """Ensure get_file_info() handles nonexistent paths correctly."""
        result = els.get_file_info("/nonexistent")
        self.assertEqual(result, "Error: '/nonexistent' does not exist.")


if __name__ == '__main__':
    unittest.main()
