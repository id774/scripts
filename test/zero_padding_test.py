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
#  v1.1 2024-03-27
#       Expanded test cases to cover a wider range of scenarios, including files
#       with already correct padding, files without numeric parts, and files with
#       special characters and spaces in their names.
#  v1.0 2024-01-11
#       Initial release of test script.
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

# Adjust the path to import script from the parent directory
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

    @patch('os.rename')
    @patch('os.listdir')
    def test_rename_files_with_longer_digits(self, mock_listdir, mock_rename):
        mock_listdir.return_value = ['data10001.csv', 'analysis20001.csv']
        expected_renames = [
            call(os.path.join('/testdir', 'data10001.csv'),
                 os.path.join('/testdir', 'data0001.csv')),
            call(os.path.join('/testdir', 'analysis20001.csv'),
                 os.path.join('/testdir', 'analysis0001.csv'))
        ]
        zero_padding.rename_files('/testdir', 4, True)
        mock_rename.assert_has_calls(expected_renames, any_order=True)

    @patch('os.rename')
    @patch('os.listdir')
    def test_rename_files_without_numbers(self, mock_listdir, mock_rename):
        mock_listdir.return_value = ['notes.txt', 'summary.docx']
        zero_padding.rename_files('/testdir', 4, True)
        mock_rename.assert_not_called()

    @patch('os.rename')
    @patch('os.listdir')
    def test_empty_directory(self, mock_listdir, mock_rename):
        mock_listdir.return_value = []
        zero_padding.rename_files('/testdir', 4, True)
        mock_rename.assert_not_called()

    @patch('logging.info')
    @patch('os.rename')
    @patch('os.listdir')
    def test_quiet_mode(self, mock_listdir, mock_rename, mock_logging):
        mock_listdir.return_value = ['report1.pdf', 'report10.pdf']
        zero_padding.rename_files('/testdir', 4, True)  # quiet_mode=True
        mock_logging.assert_not_called()

    @patch('os.rename')
    @patch('os.listdir')
    def test_rename_files_with_already_padded_names(self, mock_listdir, mock_rename):
        mock_listdir.return_value = ['doc001.txt', 'report005.pdf']

        zero_padding.rename_files('/testdir', 3, True)  # quiet_mode=True

        # Verify that os.rename is not called since names are already correctly padded
        mock_rename.assert_not_called()

    @patch('os.rename')
    @patch('os.listdir')
    def test_rename_files_without_numeric_part(self, mock_listdir, mock_rename):
        mock_listdir.return_value = ['notes.txt', 'summary.pdf']

        zero_padding.rename_files('/testdir', 4, True)  # quiet_mode=True

        # Verify that os.rename is not called for files without a numeric part
        mock_rename.assert_not_called()

    @patch('os.rename')
    @patch('os.listdir')
    def test_rename_files_with_special_characters(self, mock_listdir, mock_rename):
        mock_listdir.return_value = ['data_#1.txt', 'report 2.pdf']

        expected_renames = [
            call(os.path.join('/testdir', 'data_#1.txt'),
                 os.path.join('/testdir', 'data_#0001.txt')),
            call(os.path.join('/testdir', 'report 2.pdf'),
                 os.path.join('/testdir', 'report 0002.pdf'))
        ]

        zero_padding.rename_files('/testdir', 4, True)  # quiet_mode=True

        # Verify that os.rename is called with the expected arguments for files with special characters
        mock_rename.assert_has_calls(expected_renames, any_order=True)

    @patch('os.rename')
    @patch('os.listdir')
    def test_rename_files_in_directory_with_numbers(self, mock_listdir, mock_rename):
        mock_listdir.return_value = ['image1.jpg', 'video2.mp4']

        expected_renames = [
            call(os.path.join('/2024_photos', 'image1.jpg'),
                 os.path.join('/2024_photos', 'image0001.jpg')),
            call(os.path.join('/2024_photos', 'video2.mp4'),
                 os.path.join('/2024_photos', 'video0002.mp4'))
        ]

        zero_padding.rename_files('/2024_photos', 4, True)  # quiet_mode=True

        # Verify that os.rename is called with the expected arguments, ignoring the directory's numeric part
        mock_rename.assert_has_calls(expected_renames, any_order=True)


if __name__ == '__main__':
    unittest.main()
