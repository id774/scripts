#!/usr/bin/env python

########################################################################
# test/tcmount_test.py: Tests for tcmount.py
#
#  Description:
#  This test suite is designed to test the tcmount.py script, focusing on the
#  functionality of building commands for mounting and unmounting TrueCrypt
#  encrypted devices.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2023-12-15
#       First release of the test suite for tcmount.py.
#
#  Running the tests:
#  Execute the test script from the command line:
#  `python test/tcmount_test.py`
#
########################################################################

import unittest
import sys
import os
from unittest.mock import patch

# Import the script/module to be tested
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import tcmount

class TestTcMount(unittest.TestCase):

    def test_build_mount_command(self):
        expected = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 /dev/sdb ~/mnt/sdb'
        result = tcmount.build_mount_command('sdb', 'utf8')
        self.assertEqual(result, expected)

    def test_build_unmount_command(self):
        expected = 'sudo truecrypt -d ~/mnt/sdb'
        result = tcmount.build_unmount_command('sdb')
        self.assertEqual(result, expected)

    def test_build_mount_all_command(self):
        result = tcmount.build_mount_all_command('utf8')
        self.assertTrue('sdz' in result[-1])

    def test_build_mount_expansion_command(self):
        expected = 'test -f ~/mnt/Expansion/container.tc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 ~/mnt/Expansion/container.tc ~/mnt/sdb'
        result = tcmount.build_mount_expansion_command('sdb', 'utf8')
        self.assertEqual(result, expected)

    @patch('tcmount.subprocess.call')
    def test_is_truecrypt_installed(self, mock_call):
        mock_call.return_value = 0
        self.assertTrue(tcmount.is_truecrypt_installed())


if __name__ == '__main__':
    unittest.main()
