#!/usr/bin/env python

########################################################################
# tcmount_test.py: Tests for tcmount.py
#
#  Description:
#  This test suite is designed to test the tcmount.py script, focusing on
#  the functionality of building commands for mounting and unmounting
#  TrueCrypt and VeraCrypt encrypted devices. It tests various combinations
#  of options, including TrueCrypt and VeraCrypt compatibility modes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Running the tests:
#  Execute the test script from the command line:
#      python test/tcmount_test.py
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Build a default TrueCrypt mount command for a block device with UTF-8 filesystem options.
#    - Build a default TrueCrypt unmount command for a device mountpoint resolved via get-device/get-mountpoint.
#    - Build mount-all command list and include the last device (sdz) in the generated commands.
#    - Build an external container mount command using the default mount target (device name).
#    - Build a mount command with read-only filesystem option.
#    - Build a mount command without UTF-8 filesystem option.
#    - Build a mount command with an explicit target mount directory.
#    - Build an unmount command with an explicit target mount directory.
#    - Detect TrueCrypt installation when the truecrypt command is available.
#    - Detect VeraCrypt installation when the veracrypt command is available.
#    - Return True from command_exists() when the command is found.
#    - Return False from command_exists() when the command is not found.
#    - Execute a shell command via os_exec() using subprocess.call with shell=True.
#    - In process_mounting(), mount and unmount using mocked builders and os_exec (TrueCrypt path).
#    - In process_mounting(), mount and unmount using mocked builders and os_exec (VeraCrypt path).
#    - In process_mounting(), mount and unmount using mocked builders and os_exec (VeraCrypt TC-compat path).
#    - In process_mounting(), pass an explicit target to build_mount_command and build_unmount_command.
#    - In process_mounting(), build read-only/no-UTF8 mount command for TrueCrypt.
#    - In process_mounting(), build read-only mount command for VeraCrypt (utf8,ro).
#    - In process_mounting(), build no-UTF8 mount command for VeraCrypt TC-compat mode.
#    - In process_mounting(), mount with explicit target using VeraCrypt when TrueCrypt is unavailable.
#    - In process_mounting(), mount with explicit target using VeraCrypt TC-compat when TrueCrypt is unavailable.
#    - In process_mounting(), mount and unmount multiple device names via mocked builders.
#    - In process_mounting(), build read-only/no-UTF8 mount commands for multiple device names.
#    - In process_mounting(), generate expected mount commands across option combinations (veracrypt/tc_compat/no_utf8/readonly/all/external).
#    - In process_mounting(), generate expected mount commands for representative option combinations.
#    - Build an external container mount command using the default target when explicit target is not provided.
#    - Build an external container mount command honoring an explicit target mount directory.
#    - In process_mounting(), delegate external mount (-e) to build_mount_external_command with an explicit target.
#    - In process_mounting(), unmount an external container with default arguments.
#    - In process_mounting(), unmount an external container even when an explicit target is provided.
#
#  Version History:
#  v1.3 2025-08-31
#       Add 5 tests for external container (-e): mount default/explicit target,
#       process_mounting explicit target, and unmount default/explicit target.
#  v1.2 2025-08-29
#       Add tests for explicit target argument (mount and unmount).
#  v1.1 2025-05-14
#       Added unit tests for command_exists() to verify command detection logic.
#  v1.0 2023-12-15
#       First release of the test suite for tcmount.py, with expanded tests
#       covering both TrueCrypt and VeraCrypt compatibility.
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import tcmount


class TestTcMount(unittest.TestCase):
    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'tcmount.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @classmethod
    def setUp(self):
        self.truecrypt_installed = tcmount.is_truecrypt_installed()
        self.veracrypt_installed = tcmount.is_veracrypt_installed()

    def check_truecrypt_installed(self):
        if not self.truecrypt_installed:
            self.skipTest("TrueCrypt is not installed, skipping this test.")

    def check_veracrypt_installed(self):
        if not self.veracrypt_installed:
            self.skipTest("VeraCrypt is not installed, skipping this test.")

    def check_both_installed(self):
        if not (self.truecrypt_installed and self.veracrypt_installed):
            self.skipTest(
                "Neither TrueCrypt nor VeraCrypt is installed, skipping this test.")

    def test_build_mount_command(self):
        expected = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 /dev/sdb ~/mnt/sdb'
        result = tcmount.build_mount_command('sdb', 'utf8')
        self.assertEqual(result, expected)

    def test_build_unmount_command(self):
        expected = 'dev=$(get-device ~/mnt/sdb) && mp=$(get-mountpoint "$dev") && sudo truecrypt -d "$mp"'
        result = tcmount.build_unmount_command('sdb')
        self.assertEqual(result, expected)

    def test_build_mount_all_command(self):
        result = tcmount.build_mount_all_command('utf8')
        self.assertTrue('sdz' in result[-1])

    def test_build_mount_external_command(self):
        expected = 'test -f ~/mnt/external/container.tc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 ~/mnt/external/container.tc ~/mnt/sdb'
        result = tcmount.build_mount_external_command('sdb', 'utf8')
        self.assertEqual(result, expected)

    def test_build_mount_command_with_readonly(self):
        expected = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=ro /dev/sdb ~/mnt/sdb'
        result = tcmount.build_mount_command('sdb', 'ro')
        self.assertEqual(result, expected)

    def test_build_mount_command_without_utf8(self):
        expected = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options= /dev/sdb ~/mnt/sdb'
        result = tcmount.build_mount_command('sdb', '')
        self.assertEqual(result, expected)

    def test_build_mount_command_with_explicit_target(self):
        expected = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 /dev/sdb ~/mnt/disk1'
        result = tcmount.build_mount_command('sdb', 'utf8', 'disk1')
        self.assertEqual(result, expected)

    def test_build_unmount_command_with_explicit_target(self):
        expected = 'dev=$(get-device ~/mnt/disk1) && mp=$(get-mountpoint "$dev") && sudo truecrypt -d "$mp"'
        result = tcmount.build_unmount_command('disk1')
        self.assertEqual(result, expected)

    @patch('tcmount.subprocess.call')
    def test_is_truecrypt_installed(self, mock_call):
        mock_call.return_value = 0
        self.assertTrue(tcmount.is_truecrypt_installed())

    @patch('tcmount.subprocess.call')
    def test_is_veracrypt_installed(self, mock_call):
        mock_call.return_value = 0
        self.assertTrue(tcmount.is_veracrypt_installed())

    @patch('tcmount.subprocess.call')
    def test_command_exists_true(self, mock_call):
        """
        Tests that command_exists returns True when the command is found.
        """
        mock_call.return_value = 0
        self.assertTrue(tcmount.command_exists('truecrypt'))

    @patch('tcmount.subprocess.call')
    def test_command_exists_false(self, mock_call):
        """
        Tests that command_exists returns False when the command is not found.
        """
        mock_call.return_value = 1
        self.assertFalse(tcmount.command_exists('nonexistent'))

    @patch('tcmount.subprocess.call')
    def test_os_exec(self, mock_call):
        command = 'echo "Test Command"'
        tcmount.os_exec(command)
        mock_call.assert_called_with(command, shell=True)

    def process_mounting_test_helper(self, veracrypt=False, tc_compat=False):
        # Set up mock responses
        with patch('tcmount.build_mount_command') as mock_build_mount, \
                patch('tcmount.build_unmount_command') as mock_build_unmount, \
                patch('tcmount.os_exec') as mock_os_exec:
            mock_build_mount.return_value = 'mocked mount command'
            mock_build_unmount.return_value = 'mocked unmount command'

            # Test mounting
            def options(): return None
            options.veracrypt = veracrypt
            options.tc_compat = tc_compat
            options.no_utf8 = False
            options.readonly = False
            options.all = False
            options.external = None
            tcmount.process_mounting(options, ['sdb'])
            mock_build_mount.assert_called_with('sdb', 'utf8')
            mock_os_exec.assert_called_with('mocked mount command')

            # Test unmounting
            tcmount.process_mounting(options, ['sdb', 'unmount'])
            mock_build_unmount.assert_called_with('sdb')
            mock_os_exec.assert_called_with('mocked unmount command')

    def test_process_mounting_truecrypt(self):
        self.check_truecrypt_installed()
        self.process_mounting_test_helper(veracrypt=False, tc_compat=False)

    def test_process_mounting_veracrypt(self):
        self.check_veracrypt_installed()
        self.process_mounting_test_helper(veracrypt=True, tc_compat=False)

    def test_process_mounting_tc_compat(self):
        self.check_veracrypt_installed()
        self.process_mounting_test_helper(veracrypt=False, tc_compat=True)

    def test_process_mounting_with_explicit_target_calls(self):
        # Verify that explicit target is passed to builders
        with patch('tcmount.build_mount_command') as mock_build_mount, \
                patch('tcmount.build_unmount_command') as mock_build_unmount, \
                patch('tcmount.os_exec') as mock_os_exec, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            mock_build_mount.return_value = 'mocked mount command'
            mock_build_unmount.return_value = 'mocked unmount command'

            def options(): return None
            options.veracrypt = False
            options.tc_compat = False
            options.no_utf8 = False
            options.readonly = False
            options.all = False
            options.external = None

            # Mount with explicit target
            tcmount.process_mounting(options, ['sdb', 'disk1'])
            mock_build_mount.assert_called_with('sdb', 'utf8', 'disk1')
            mock_os_exec.assert_called_with('mocked mount command')
            mock_os_exec.reset_mock()

            # Unmount with explicit target (uses build_unmount_command which resolves real mountpoint)
            with patch('tcmount.build_unmount_command') as mock_build_unmount:
                mock_build_unmount.return_value = 'mocked unmount command'
                tcmount.process_mounting(options, ['sdb', 'disk1', 'unmount'])
                mock_build_unmount.assert_called_with('disk1')
                mock_os_exec.assert_called_with('mocked unmount command')

    @patch('tcmount.os_exec')
    def test_process_mounting_readonly_no_utf8_with_truecrypt(self, mock_os_exec):
        self.check_truecrypt_installed()

        def options(): return None
        options.veracrypt = False
        options.tc_compat = False
        options.no_utf8 = True
        options.readonly = True
        options.all = False
        options.external = None
        tcmount.process_mounting(options, ['sdb'])
        expected_command = 'test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=ro /dev/sdb ~/mnt/sdb'
        mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.os_exec')
    def test_process_mounting_readonly_with_veracrypt(self, mock_os_exec):
        self.check_veracrypt_installed()

        def options(): return None
        options.veracrypt = True
        options.tc_compat = False
        options.no_utf8 = False
        options.readonly = True
        options.all = False
        options.external = None
        tcmount.process_mounting(options, ['sdb'])
        expected_command = 'test -b /dev/sdb && sudo veracrypt -t -k "" --protect-hidden=no --fs-options=utf8,ro /dev/sdb ~/mnt/sdb'
        mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.os_exec')
    def test_process_mounting_no_utf8_with_tc_compat(self, mock_os_exec):
        self.check_veracrypt_installed()

        def options(): return None
        options.veracrypt = False
        options.tc_compat = True
        options.no_utf8 = True
        options.readonly = False
        options.all = False
        options.external = None
        tcmount.process_mounting(options, ['sdb'])
        expected_command = 'test -b /dev/sdb && sudo veracrypt -tc -t -k "" --protect-hidden=no --fs-options= /dev/sdb ~/mnt/sdb'
        mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.os_exec')
    @patch('tcmount.is_truecrypt_installed', return_value=False)
    @patch('tcmount.is_veracrypt_installed', return_value=True)
    def test_process_mounting_with_explicit_target_veracrypt(self, mock_vc, mock_tc, mock_os_exec):
        # Mount with explicit target using veracrypt
        def options(): return None
        options.veracrypt = True
        options.tc_compat = False
        options.no_utf8 = False
        options.readonly = False
        options.all = False
        options.external = None
        tcmount.process_mounting(options, ['sdb', 'disk1'])
        expected_command = 'test -b /dev/sdb && sudo veracrypt -t -k "" --protect-hidden=no --fs-options=utf8 /dev/sdb ~/mnt/disk1'
        mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.os_exec')
    @patch('tcmount.is_truecrypt_installed', return_value=False)
    @patch('tcmount.is_veracrypt_installed', return_value=True)
    def test_process_mounting_with_explicit_target_tc_compat(self, mock_vc, mock_tc, mock_os_exec):
        # Mount with explicit target using veracrypt in TC-compat mode
        def options(): return None
        options.veracrypt = False
        options.tc_compat = True
        options.no_utf8 = False
        options.readonly = False
        options.all = False
        options.external = None
        tcmount.process_mounting(options, ['sdb', 'disk1'])
        expected_command = 'test -b /dev/sdb && sudo veracrypt -tc -t -k "" --protect-hidden=no --fs-options=utf8 /dev/sdb ~/mnt/disk1'
        mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.build_mount_command')
    @patch('tcmount.build_unmount_command')
    @patch('tcmount.os_exec')
    def test_process_mounting_different_devices(self, mock_os_exec, mock_build_unmount, mock_build_mount):
        self.check_truecrypt_installed()

        # Set up mock responses
        mock_build_mount.return_value = 'mocked mount command'
        mock_build_unmount.return_value = 'mocked unmount command'

        for device in ['sdb', 'sdc', 'sde', 'sdz']:
            with self.subTest(device=device):
                # Test mounting
                def options(): return None
                options.veracrypt = False
                options.tc_compat = False
                options.no_utf8 = False
                options.readonly = False
                options.all = False
                options.external = None
                tcmount.process_mounting(options, [device])
                mock_build_mount.assert_called_with(device, 'utf8')
                mock_os_exec.assert_called_with('mocked mount command')

                # Test unmounting
                tcmount.process_mounting(options, [device, 'unmount'])
                mock_build_unmount.assert_called_with(device)
                mock_os_exec.assert_called_with('mocked unmount command')

    @patch('tcmount.os_exec')
    def test_process_mounting_readonly_no_utf8_different_devices(self, mock_os_exec):
        self.check_truecrypt_installed()

        for device in ['sdb', 'sdc', 'sde', 'sdz']:
            with self.subTest(device=device):
                def options(): return None
                options.veracrypt = False
                options.tc_compat = False
                options.no_utf8 = True
                options.readonly = True
                options.all = False
                options.external = None
                tcmount.process_mounting(options, [device])
                expected_command = 'test -b /dev/{} && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=ro /dev/{} ~/mnt/{}'.format(
                    device, device, device)
                mock_os_exec.assert_called_with(expected_command)

    @patch('tcmount.os_exec')
    def test_process_mounting_combinations(self, mock_os_exec):
        self.check_both_installed()

        test_cases = [
            (False, False, False, False, False, None, 'utf8'),
            (False, False, True, False, False, None, ''),
            (False, False, False, True, False, None, 'utf8,ro'),
            (False, False, True, True, False, None, 'ro'),
            (False, False, False, False, True, None, 'utf8'),
            (False, False, True, False, True, None, ''),
            (False, False, False, True, True, None, 'utf8,ro'),
            (False, False, True, True, True, None, 'ro'),
            (False, False, False, False, False, 'sdb', 'utf8'),
            (False, False, True, False, False, 'sdb', ''),
            (False, False, False, True, False, 'sdb', 'utf8,ro'),
            (False, False, True, True, False, 'sdb', 'ro'),
            (False, False, False, False, True, 'sdb', 'utf8'),
            (False, False, True, False, True, 'sdb', ''),
            (False, False, False, True, True, 'sdb', 'utf8,ro'),
            (False, False, True, True, True, 'sdb', 'ro'),
            (False, False, False, False, False, 'sdc', 'utf8'),
            (False, False, True, False, False, 'sdc', ''),
            (False, False, False, True, False, 'sdc', 'utf8,ro'),
            (False, False, True, True, False, 'sdc', 'ro'),
            (False, False, False, False, True, 'sdc', 'utf8'),
            (False, False, True, False, True, 'sdc', ''),
            (False, False, False, True, True, 'sdc', 'utf8,ro'),
            (False, False, True, True, True, 'sdc', 'ro'),
            (True, False, False, False, False, None, 'utf8'),
            (True, False, True, False, False, None, ''),
            (True, False, False, True, False, None, 'utf8,ro'),
            (True, False, True, True, False, None, 'ro'),
            (True, False, False, False, True, None, 'utf8'),
            (True, False, True, False, True, None, ''),
            (True, False, False, True, True, None, 'utf8,ro'),
            (True, False, True, True, True, None, 'ro'),
            (True, False, False, False, False, 'sdb', 'utf8'),
            (True, False, True, False, False, 'sdb', ''),
            (True, False, False, True, False, 'sdb', 'utf8,ro'),
            (True, False, True, True, False, 'sdb', 'ro'),
            (True, False, False, False, True, 'sdb', 'utf8'),
            (True, False, True, False, True, 'sdb', ''),
            (True, False, False, True, True, 'sdb', 'utf8,ro'),
            (True, False, True, True, True, 'sdb', 'ro'),
            (True, False, False, False, False, 'sdc', 'utf8'),
            (True, False, True, False, False, 'sdc', ''),
            (True, False, False, True, False, 'sdc', 'utf8,ro'),
            (True, False, True, True, False, 'sdc', 'ro'),
            (True, False, False, False, True, 'sdc', 'utf8'),
            (True, False, True, False, True, 'sdc', ''),
            (True, False, False, True, True, 'sdc', 'utf8,ro'),
            (True, False, True, True, True, 'sdc', 'ro'),
            (False, True, False, False, False, None, 'utf8'),
            (False, True, True, False, False, None, ''),
            (False, True, False, True, False, None, 'utf8,ro'),
            (False, True, True, True, False, None, 'ro'),
            (False, True, False, False, True, None, 'utf8'),
            (False, True, True, False, True, None, ''),
            (False, True, False, True, True, None, 'utf8,ro'),
            (False, True, True, True, True, None, 'ro'),
            (False, True, False, False, False, 'sdb', 'utf8'),
            (False, True, True, False, False, 'sdb', ''),
            (False, True, False, True, False, 'sdb', 'utf8,ro'),
            (False, True, True, True, False, 'sdb', 'ro'),
            (False, True, False, False, True, 'sdb', 'utf8'),
            (False, True, True, False, True, 'sdb', ''),
            (False, True, False, True, True, 'sdb', 'utf8,ro'),
            (False, True, True, True, True, 'sdb', 'ro'),
            (False, True, False, False, False, 'sdc', 'utf8'),
            (False, True, True, False, False, 'sdc', ''),
            (False, True, False, True, False, 'sdc', 'utf8,ro'),
            (False, True, True, True, False, 'sdc', 'ro'),
            (False, True, False, False, True, 'sdc', 'utf8'),
            (False, True, True, False, True, 'sdc', ''),
            (False, True, False, True, True, 'sdc', 'utf8,ro'),
            (False, True, True, True, True, 'sdc', 'ro'),
        ]

        for veracrypt, tc_compat, no_utf8, readonly, all_devices, external, expected_options in test_cases:
            with self.subTest(veracrypt=veracrypt, tc_compat=tc_compat, no_utf8=no_utf8, readonly=readonly, all_devices=all_devices, external=external):
                def options(): return None
                options.veracrypt = veracrypt
                options.tc_compat = tc_compat
                options.no_utf8 = no_utf8
                options.readonly = readonly
                options.all = all_devices
                options.external = external
                tcmount.process_mounting(options, ['sdb'])

                if tc_compat:
                    cmd_prefix = 'veracrypt -tc'
                elif veracrypt:
                    cmd_prefix = 'veracrypt'
                else:
                    cmd_prefix = 'truecrypt'

                if external:
                    expected_command = 'test -f ~/mnt/external/container.tc && sudo {} -t -k "" --protect-hidden=no --fs-options={} ~/mnt/external/container.tc ~/mnt/{}'.format(
                        cmd_prefix, expected_options, external)
                else:
                    expected_command = 'test -b /dev/sdb && sudo {} -t -k "" --protect-hidden=no --fs-options={} /dev/sdb ~/mnt/sdb'.format(
                        cmd_prefix, expected_options)

                mock_os_exec.assert_called_with(expected_command)
                mock_os_exec.reset_mock()

    @patch('tcmount.os_exec')
    def test_mounting_various_options(self, mock_os_exec):
        self.check_both_installed()

        # Define various option combinations for testing
        options_combinations = [
            {'veracrypt': False, 'tc_compat': False,
                'no_utf8': False, 'readonly': False},
            {'veracrypt': True, 'tc_compat': False,
                'no_utf8': False, 'readonly': False},
            {'veracrypt': False, 'tc_compat': True,
                'no_utf8': False, 'readonly': False},
            {'veracrypt': False, 'tc_compat': False,
                'no_utf8': True, 'readonly': False},
            {'veracrypt': True, 'tc_compat': False,
                'no_utf8': True, 'readonly': False},
            {'veracrypt': False, 'tc_compat': True,
                'no_utf8': True, 'readonly': False},
            {'veracrypt': False, 'tc_compat': False,
                'no_utf8': False, 'readonly': True},
            {'veracrypt': True, 'tc_compat': False,
                'no_utf8': False, 'readonly': True},
            {'veracrypt': False, 'tc_compat': True,
                'no_utf8': False, 'readonly': True},
        ]

        for options_dict in options_combinations:
            with self.subTest(**options_dict):
                # Setup options based on the current combination
                def options(): return None
                options.veracrypt = options_dict['veracrypt']
                options.tc_compat = options_dict['tc_compat']
                options.no_utf8 = options_dict['no_utf8']
                options.readonly = options_dict['readonly']
                options.all = False
                options.external = None

                # Call process_mounting with the current options
                tcmount.process_mounting(options, ['sdb'])

                # Construct the expected command based on options
                if options.tc_compat:
                    cmd_prefix = 'veracrypt -tc'
                elif options.veracrypt:
                    cmd_prefix = 'veracrypt'
                else:
                    cmd_prefix = 'truecrypt'

                fs_options = 'utf8' if not options.no_utf8 else ''
                fs_options += ',ro' if options.readonly else ''
                expected_command = 'test -b /dev/sdb && sudo {} -t -k "" --protect-hidden=no --fs-options={} /dev/sdb ~/mnt/sdb'.format(
                    cmd_prefix, fs_options)

                mock_os_exec.assert_called_with(expected_command)
                mock_os_exec.reset_mock()

    def test_build_mount_external_command_default_target(self):
        # Use external_device when target is not specified
        expected = 'test -f ~/mnt/external/container.tc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 ~/mnt/external/container.tc ~/mnt/sde'
        result = tcmount.build_mount_external_command('sde', 'utf8', None)
        self.assertEqual(result, expected)

    def test_build_mount_external_command_explicit_target(self):
        # Honor explicit target over external_device name
        expected = 'test -f ~/mnt/external/container.tc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 ~/mnt/external/container.tc ~/mnt/disk3'
        result = tcmount.build_mount_external_command('sde', 'utf8', 'disk3')
        self.assertEqual(result, expected)

    def test_process_mounting_external_with_explicit_target(self):
        # Verify process_mounting delegates to build_mount_external_command with explicit target
        with patch('tcmount.build_mount_external_command') as mock_build_ext, \
                patch('tcmount.os_exec') as mock_os_exec, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            mock_build_ext.return_value = 'mocked external mount command'

            def options(): return None
            options.veracrypt = False
            options.tc_compat = False
            options.no_utf8 = False
            options.readonly = False
            options.all = False
            options.external = 'sde'  # -e sde

            # args includes an unrelated device ('sdb') and an explicit target ('disk3')
            tcmount.process_mounting(options, ['sdb', 'disk3'])

            mock_build_ext.assert_called_with('sde', 'utf8', 'disk3')
            mock_os_exec.assert_called_with('mocked external mount command')

    def test_process_mounting_external_unmount_default(self):
        # -e sde unmount -> detach by container path
        with patch('tcmount.build_unmount_external_command') as mock_build_unmount_ext, \
                patch('tcmount.os_exec') as mock_os_exec, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            mock_build_unmount_ext.return_value = 'mocked external unmount command'

            def options(): return None
            options.veracrypt = False
            options.tc_compat = False
            options.no_utf8 = False
            options.readonly = False
            options.all = False
            options.external = 'sde'

            tcmount.process_mounting(options, ['unmount'])

            mock_build_unmount_ext.assert_called_once()
            mock_os_exec.assert_called_with('mocked external unmount command')

    def test_process_mounting_external_unmount_explicit_target(self):
        # -e sde disk3 unmount -> detach by container path (target ignored)
        with patch('tcmount.build_unmount_external_command') as mock_build_unmount_ext, \
                patch('tcmount.os_exec') as mock_os_exec, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            mock_build_unmount_ext.return_value = 'mocked external unmount command'

            def options(): return None
            options.veracrypt = False
            options.tc_compat = False
            options.no_utf8 = False
            options.readonly = False
            options.all = False
            options.external = 'sde'

            tcmount.process_mounting(options, ['disk3', 'unmount'])

            mock_build_unmount_ext.assert_called_once()
            mock_os_exec.assert_called_with('mocked external unmount command')


if __name__ == '__main__':
    unittest.main()
