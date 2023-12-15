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

    def test_build_mount_command_with_readonly(self):
        expected = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=ro /dev/sdb ~/mnt/sdb'
        result = tcmount.build_mount_command('sdb', 'ro')
        self.assertEqual(result, expected)

    def test_build_mount_command_without_utf8(self):
        expected = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options= /dev/sdb ~/mnt/sdb'
        result = tcmount.build_mount_command('sdb', '')
        self.assertEqual(result, expected)

    @patch('tcmount.subprocess.call')
    def test_is_truecrypt_installed(self, mock_call):
        mock_call.return_value = 0
        self.assertTrue(tcmount.is_truecrypt_installed())

    @patch('tcmount.subprocess.call')
    def test_os_exec(self, mock_call):
        command = 'echo "Test Command"'
        tcmount.os_exec(command)
        mock_call.assert_called_with(command, shell=True)

    @patch('tcmount.build_mount_command')
    @patch('tcmount.build_unmount_command')
    @patch('tcmount.os_exec')
    def test_process_mounting(self, mock_os_exec, mock_build_unmount, mock_build_mount):
        # Set up mock responses
        mock_build_mount.return_value = 'mocked mount command'
        mock_build_unmount.return_value = 'mocked unmount command'

        # Test mounting
        def options(): return None
        options.veracrypt = False
        options.no_utf8 = False
        options.readonly = False
        options.all = False
        options.expansion = None
        tcmount.process_mounting(options, ['sdb'])
        mock_build_mount.assert_called_with('sdb', 'utf8')
        mock_os_exec.assert_called_with('mocked mount command')

        # Test unmounting
        tcmount.process_mounting(options, ['sdb', 'unmount'])
        mock_build_unmount.assert_called_with('sdb')
        mock_os_exec.assert_called_with('mocked unmount command')

    @patch('tcmount.os_exec')
    def test_process_mounting_readonly_no_utf8(self, mock_os_exec):
        def options(): return None
        options.veracrypt = False
        options.no_utf8 = True
        options.readonly = True
        options.all = False
        options.expansion = None
        tcmount.process_mounting(options, ['sdb'])
        expected_command = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=ro /dev/sdb ~/mnt/sdb'
        mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.build_mount_command')
    @patch('tcmount.build_unmount_command')
    @patch('tcmount.os_exec')
    def test_process_mounting_different_devices(self, mock_os_exec, mock_build_unmount, mock_build_mount):
        # Set up mock responses
        mock_build_mount.return_value = 'mocked mount command'
        mock_build_unmount.return_value = 'mocked unmount command'

        for device in ['sdb', 'sdc', 'sde', 'sdz']:
            with self.subTest(device=device):
                # Test mounting
                def options(): return None
                options.veracrypt = False
                options.no_utf8 = False
                options.readonly = False
                options.all = False
                options.expansion = None
                tcmount.process_mounting(options, [device])
                mock_build_mount.assert_called_with(device, 'utf8')
                mock_os_exec.assert_called_with('mocked mount command')

                # Test unmounting
                tcmount.process_mounting(options, [device, 'unmount'])
                mock_build_unmount.assert_called_with(device)
                mock_os_exec.assert_called_with('mocked unmount command')

    @patch('tcmount.os_exec')
    def test_process_mounting_readonly_no_utf8_different_devices(self, mock_os_exec):
        for device in ['sdb', 'sdc', 'sde', 'sdz']:
            with self.subTest(device=device):
                def options(): return None
                options.veracrypt = False
                options.no_utf8 = True
                options.readonly = True
                options.all = False
                options.expansion = None
                tcmount.process_mounting(options, [device])
                expected_command = 'test -b /dev/{} && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=ro /dev/{} ~/mnt/{}'.format(
                    device, device, device)
                mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.os_exec')
    def test_process_mounting_combinations(self, mock_os_exec):
        test_cases = [
            (False, False, False, None, 'utf8'),
            (True, False, False, None, ''),
            (False, True, False, None, 'utf8,ro'),
            (True, True, False, None, 'ro'),
            (False, False, True, None, 'utf8'),
            (True, False, True, None, ''),
            (False, True, True, None, 'utf8,ro'),
            (True, True, True, None, 'ro'),
            (False, False, False, 'sdb', 'utf8'),
            (True, False, False, 'sdb', ''),
            (False, True, False, 'sdb', 'utf8,ro'),
            (True, True, False, 'sdb', 'ro'),
            (False, False, True, 'sdb', 'utf8'),
            (True, False, True, 'sdb', ''),
            (False, True, True, 'sdb', 'utf8,ro'),
            (True, True, True, 'sdb', 'ro'),
            (False, False, False, 'sdc', 'utf8'),
            (True, False, False, 'sdc', ''),
            (False, True, False, 'sdc', 'utf8,ro'),
            (True, True, False, 'sdc', 'ro'),
            (False, False, True, 'sdc', 'utf8'),
            (True, False, True, 'sdc', ''),
            (False, True, True, 'sdc', 'utf8,ro'),
            (True, True, True, 'sdc', 'ro'),
        ]

        for no_utf8, readonly, all_devices, expansion, expected_options in test_cases:
            with self.subTest(no_utf8=no_utf8, readonly=readonly, all_devices=all_devices, expansion=expansion):
                def options(): return None
                options.no_utf8 = no_utf8
                options.readonly = readonly
                options.all = all_devices
                options.expansion = expansion
                options.veracrypt = False
                tcmount.process_mounting(options, ['sdb'])

                if expansion:
                    expected_command = 'test -f ~/mnt/Expansion/container.tc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options={} ~/mnt/Expansion/container.tc ~/mnt/{}'.format(
                        expected_options, expansion)
                elif all_devices:
                    expected_command = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options={} /dev/sdb ~/mnt/sdb'.format(
                        expected_options)
                else:
                    expected_command = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options={} /dev/sdb ~/mnt/sdb'.format(
                        expected_options)

                mock_os_exec.assert_called_with(expected_command)
                mock_os_exec.reset_mock()


if __name__ == '__main__':
    unittest.main()
