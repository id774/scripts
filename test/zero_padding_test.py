#!/usr/bin/env python

########################################################################
# zero_padding_test.py: Test script for zero_padding.py
#
#  Description:
#  This script contains unit tests for the zero_padding.py script. It tests
#  the functionality of the rename_files function to ensure that file names
#  are correctly zero-padded to the specified number of digits.
#
#  Author: id774
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-11
#      Initial release of test script.
#
#  Usage:
#  Run this script from the command line using:
#      python test/zero_padding_test.py
#
########################################################################

import os
import sys
import unittest
from unittest.mock import call, patch

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import zero_padding


class TestZeroPadding(unittest.TestCase):
    """Test cases for the rename_files function in zero_padding.py."""

    @patch('os.rename')
    @patch('os.listdir')
    def test_rename_files(self, mock_listdir, mock_rename):
        mock_listdir.return_value = ['file1.txt',
                                     'file2_123.txt', 'file3_12345.txt']

        # Expected new file names, including the file with no numeric part
        expected_renames = [
            call(os.path.join('/testdir', 'file1.txt'),
                 os.path.join('/testdir', 'file0001.txt')),
            call(os.path.join('/testdir', 'file2_123.txt'),
                 os.path.join('/testdir', 'file2_0123.txt')),
            call(os.path.join('/testdir', 'file3_12345.txt'),
                 os.path.join('/testdir', 'file3_2345.txt'))
        ]

        zero_padding.rename_files('/testdir', 4, True)  # Add quiet_mode=True

        # Verify that os.rename is called with the expected arguments
        mock_rename.assert_has_calls(expected_renames, any_order=True)


if __name__ == '__main__':
    unittest.main()
