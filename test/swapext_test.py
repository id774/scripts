#!/usr/bin/env python

########################################################################
# swapext_test.py: Test script for swapext.py
#
#  Description:
#  This script contains the unit tests for the swapext.py script. It tests
#  the functionality of the swap_extensions function to ensure that it
#  correctly changes the file extensions within a specified directory.
#
#  Author: id774
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2024-01-11
#       Added '-q' option for quiet mode.
#  v1.0 2024-01-10
#       Initial release of test script.
#
#  Usage:
#  Run this script from the command line using:
#      python test/swapext_test.py
#
#  The script tests the swap_extensions function from swapext.py with
#  and without the quiet mode (-q) option. It uses mock objects to
#  simulate file system operations and checks whether the file extensions
#  are correctly changed as per the specified parameters.
#
########################################################################

import os
import sys
import unittest
from unittest.mock import call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import swapext


class TestSwapExt(unittest.TestCase):
    """Test cases for the swap_extensions function in swapext.py."""

    @patch('os.rename')
    @patch('os.walk')
    def test_swap_extensions(self, mock_walk, mock_rename):
        """Test swapping file extensions."""
        # Mock the os.walk to simulate directory structure
        mock_walk.return_value = [
            ('/testdir', ['subdir'], ['file1.txt', 'file2.doc', 'file3.txt']),
            ('/testdir/subdir', [], ['file4.txt', 'file5.doc'])
        ]

        # Call the function with quiet_mode=False
        swapext.swap_extensions('/testdir', 'txt', 'md', False)

        # Define expected calls to os.rename
        expected_calls = [
            call('/testdir/file1.txt', '/testdir/file1.md'),
            call('/testdir/file3.txt', '/testdir/file3.md'),
            call('/testdir/subdir/file4.txt', '/testdir/subdir/file4.md')
        ]
        # Verify that os.rename is called with the expected arguments
        mock_rename.assert_has_calls(expected_calls, any_order=True)

    @patch('os.rename')
    @patch('os.walk')
    def test_swap_extensions_quiet_mode(self, mock_walk, mock_rename):
        """Test swapping file extensions with quiet mode."""
        # Mock the os.walk to simulate directory structure
        mock_walk.return_value = [
            ('/testdir', ['subdir'], ['file1.txt', 'file2.doc', 'file3.txt']),
            ('/testdir/subdir', [], ['file4.txt', 'file5.doc'])
        ]

        # Call the function with quiet_mode=True
        swapext.swap_extensions('/testdir', 'txt', 'md', True)

        # Define expected calls to os.rename
        expected_calls = [
            call('/testdir/file1.txt', '/testdir/file1.md'),
            call('/testdir/file3.txt', '/testdir/file3.md'),
            call('/testdir/subdir/file4.txt', '/testdir/subdir/file4.md')
        ]
        # Verify that os.rename is called with the expected arguments
        mock_rename.assert_has_calls(expected_calls, any_order=True)


if __name__ == '__main__':
    unittest.main()
