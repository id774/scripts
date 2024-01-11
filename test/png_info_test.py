#!/usr/bin/env python

########################################################################
# png_info_test.py: Test script for png_info.py
#
#  Description:
#  This test script contains unit tests for the png_info.py script.
#  It verifies the functionality of reading basic information from PNG files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-12
#       Initial test script for png_info.py
#
########################################################################

import unittest
from unittest.mock import patch, mock_open
import sys
import os
import struct

# Adjusting the path to import png_info from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from png_info import read_png_info

class TestPngInfo(unittest.TestCase):
    """Unit tests for the png_info.py script."""

    @patch('builtins.open', new_callable=mock_open, read_data=b'\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x03\x20\x00\x00\x02\x58\x08\x06\x00\x00\x00')
    def test_valid_png(self, mock_file):
        """Test reading information from a valid PNG file."""
        expected_result = (800, 600, 8, 'PNG-32')
        result = read_png_info('valid.png')
        self.assertEqual(result, expected_result)

    @patch('builtins.open', new_callable=mock_open, read_data=b'Invalid data')
    def test_invalid_png(self, mock_file):
        """Test reading information from an invalid PNG file."""
        with self.assertRaises(ValueError):
            read_png_info('invalid.png')

    @patch('builtins.open', new_callable=mock_open, read_data=b'\x89PNG\r\n\x1a\n\x00\x00\x00\x02IHDR\x00\x00\x00\x00')
    def test_incomplete_png(self, mock_file):
        """Test reading information from an incomplete PNG file."""
        with self.assertRaises(struct.error):
            read_png_info('incomplete.png')

    # Additional tests can be added here to cover more cases


if __name__ == '__main__':
    unittest.main()
