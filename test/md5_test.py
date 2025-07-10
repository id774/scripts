#!/usr/bin/env python

########################################################################
# md5_test.py: Test script for md5.py
#
#  Description:
#  Tests the functionality of the md5.py script, ensuring correct MD5
#  checksum calculation for files and strings.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script from the command line using:
#      python test/md5_test.py
#
#  Version History:
#  v1.0 2024-01-08
#       Initial release of test script.
#
########################################################################

import os
import subprocess
import sys
import unittest

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import md5


class TestMd5Checksum(unittest.TestCase):
    """ Test cases for Md5Checksum class in md5.py. """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'md5.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_calculate_checksum_for_string(self):
        """ Test MD5 checksum calculation for a given string. """
        test_string = "hello world"
        expected_checksum = "5eb63bbbe01eeed093cb22bb8f5acdc3"
        self.assertEqual(md5.Md5Checksum.calculate_checksum_for_string(
            test_string), expected_checksum)

    def test_calculate_checksum_for_file(self):
        """ Test MD5 checksum calculation for a given file. """
        # Getting the absolute path of the test file
        test_file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                      "md5_testdata.txt")
        expected_checksum = "712bd8c1c1e9b6cf95aa763cc7e951f7"
        self.assertEqual(md5.Md5Checksum.calculate_checksum(
            test_file_path), expected_checksum)


if __name__ == '__main__':
    unittest.main()
