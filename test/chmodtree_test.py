#!/usr/bin/env python

########################################################################
# test/chmodtree_test.py: Tests for chmodtree.py
#
#  Description:
#  This test suite is designed to test the chmodtree.py script, focusing on
#  the functionality of changing file and directory permissions recursively
#  within a specified directory. It tests various combinations of options.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2023-12-15
#       First release of the test suite for chmodtree.py.
#
#  Running the tests:
#  Execute the test script from the command line:
#  `python test/chmodtree_test.py`
#
########################################################################

import unittest
import sys
import os
from unittest.mock import patch, MagicMock

# Import the script/module to be tested
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import chmodtree

class TestChmodTree(unittest.TestCase):
    @patch('chmodtree.os_exec')
    def test_files_chmod(self, mock_os_exec):
        """ Test chmod application to files only, matching a specific pattern. """
        mock_os_exec.return_value = None  # Mock the os_exec function
        options = MagicMock()
        options.sudo = False
        options.quiet = True
        options.files = '644'
        options.dirs = None
        options.name = '*.py'
        chmodtree.chmodtree(options, 'testdir1')
        mock_os_exec.assert_called_with(
            'find testdir1 -name "*.py" -type f -exec chmod 644 {} \\;')

    @patch('chmodtree.os_exec')
    def test_dirs_chmod(self, mock_os_exec):
        """ Test chmod application to directories only, using sudo. """
        options = MagicMock()
        options.sudo = True
        options.quiet = False
        options.files = None
        options.dirs = '755'
        options.name = None
        chmodtree.chmodtree(options, 'testdir2')
        mock_os_exec.assert_called_with(
            'sudo find testdir2 -type d -exec chmod -v 755 {} \\;')

    @patch('chmodtree.os_exec')
    def test_files_and_dirs_chmod_with_pattern(self, mock_os_exec):
        """ Test chmod application to both files and directories, matching a specific pattern. """
        options = MagicMock()
        options.sudo = True
        options.quiet = True
        options.files = '600'
        options.dirs = '700'
        options.name = '*.rb'
        chmodtree.chmodtree(options, 'testdir3')

        expected_calls = [
            ('sudo find testdir3 -name "*.rb" -type f -exec chmod 600 {} \\;',),
            ('sudo find testdir3 -name "*.rb" -type d -exec chmod 700 {} \\;',)
        ]
        actual_calls = [call_args[0]
                        for call_args in mock_os_exec.call_args_list]
        self.assertEqual(actual_calls, expected_calls)

    @patch('chmodtree.os_exec')
    def test_files_and_dirs_sudo_verbose_chmod(self, mock_os_exec):
        """ Test chmod application to both files and directories with sudo, verbose output. """
        options = MagicMock()
        options.sudo = True
        options.quiet = False
        options.files = '640'
        options.dirs = '750'
        options.name = '*.txt'
        chmodtree.chmodtree(options, 'testdir4')

        expected_calls = [
            ('sudo find testdir4 -name "*.txt" -type f -exec chmod -v 640 {} \\;',),
            ('sudo find testdir4 -name "*.txt" -type d -exec chmod -v 750 {} \\;',)
        ]
        actual_calls = [call_args[0]
                        for call_args in mock_os_exec.call_args_list]
        self.assertEqual(actual_calls, expected_calls)

    @patch('chmodtree.os_exec')
    def test_files_and_dirs_quiet_chmod_with_pattern(self, mock_os_exec):
        """ Test chmod application to both files and directories, matching a specific pattern, in quiet mode. """
        options = MagicMock()
        options.sudo = False
        options.quiet = True
        options.files = '775'
        options.dirs = '750'
        options.name = '*.sh'
        chmodtree.chmodtree(options, 'testdir5')

        expected_calls = [
            ('find testdir5 -name "*.sh" -type f -exec chmod 775 {} \\;',),
            ('find testdir5 -name "*.sh" -type d -exec chmod 750 {} \\;',)
        ]
        actual_calls = [call_args[0]
                        for call_args in mock_os_exec.call_args_list]
        self.assertEqual(actual_calls, expected_calls)


if __name__ == '__main__':
    unittest.main()
