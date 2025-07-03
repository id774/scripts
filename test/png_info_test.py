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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2024-01-30
#       Updated test cases to handle multiple reads with mock_open.
#  v1.0 2024-01-11
#       Initial test script for png_info.py
#
########################################################################

import os
import struct
import subprocess
import sys
import unittest
from unittest.mock import mock_open, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from png_info import read_png_info


class TestPngInfo(unittest.TestCase):
    """ Unit tests for the png_info.py script. """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'png_info.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @patch('builtins.open', new_callable=mock_open)
    def test_valid_png(self, mock_file):
        """ Test reading information from a valid PNG file. """
        # Define the behavior of the fake read method to simulate file content reading
        mock_file.return_value.read.side_effect = [
            b'\x89PNG\r\n\x1a\n',  # PNG signature
            b'\x00\x00\x00\rIHDR',  # IHDR chunk length and type
            b'\x00\x00\x03\x20\x00\x00\x02\x58\x08\x06\x00\x00\x00',  # IHDR chunk data (width, height, bit depth, color type)
            b'',  # Indicate no more data
        ]

        expected_result = (800, 600, 8, 'PNG-32')
        result = read_png_info('valid.png')
        self.assertEqual(result, expected_result)

    @patch('builtins.open', new_callable=mock_open, read_data=b'Invalid data')
    def test_invalid_png(self, mock_file):
        """ Test reading information from an invalid PNG file. """
        with self.assertRaises(ValueError):
            read_png_info('invalid.png')

    @patch('builtins.open', new_callable=mock_open)
    def test_incomplete_png(self, mock_file):
        """ Test reading information from an incomplete PNG file. """
        # Define the behavior of the fake read method to simulate file content reading
        mock_file.return_value.read.side_effect = [
            b'\x89PNG\r\n\x1a\n',  # PNG signature
            b'\x00\x00\x00\x02IHDR',  # IHDR chunk length and type (incomplete)
            b'',  # Indicate data is missing
        ]

        with self.assertRaises(struct.error):
            read_png_info('incomplete.png')


if __name__ == '__main__':
    unittest.main()
