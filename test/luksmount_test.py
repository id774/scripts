#!/usr/bin/env python

########################################################################
# luksmount_test.py: Tests for luksmount.py
#
#  Description:
#  This test suite verifies the control flow, failure semantics, command
#  arguments, and side effect boundary of luksmount.py. All external
#  commands (get-serial, sudo, cryptsetup, mount) and filesystem checks
#  are mocked; no real block device, passphrase, or privilege is used.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Running the tests:
#  Execute the test script from the command line:
#      python test/luksmount_test.py
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Verifies that --version prints 'luksmount.py v1.0' and exits with code 0.
#    - Build source, mapper, and target paths from a device and a name.
#    - Build the cryptsetup open command as an argument list.
#    - Build the mount command as an argument list.
#    - Reject a device containing '/' without running cryptsetup or mount.
#    - Reject a mapper name outside the allowed pattern without running cryptsetup or mount.
#    - A get-serial failure stops before confirmation, sudo, cryptsetup, and mount.
#    - Declining the confirmation prompt causes no side effect and returns 0.
#    - A cryptsetup open failure skips mount and returns 1.
#    - A mount failure leaves the mapper open, never runs a close command, and returns 1.
#    - A successful run executes cryptsetup open and then mount in order.
#    - An existing mapper is rejected before serial, sudo, cryptsetup, and mount.
#    - A missing mount point is rejected before serial, sudo, cryptsetup, and mount.
#    - A missing required command returns 127 before main processing.
#    - A non-executable required command returns 126 before main processing.
#
#  Version History:
#  v1.0 2026-09-03
#       Initial release.
#
########################################################################

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import luksmount


class TestLuksMount(unittest.TestCase):
    """ Test cases for luksmount.py. """

    def script_path(self):
        """ Return the absolute path of luksmount.py. """
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        return os.path.join(script_dir, 'luksmount.py')

    def run_script(self, *args):
        """ Run luksmount.py as a subprocess and return (returncode, stdout). """
        proc = subprocess.Popen(['python', self.script_path()] + list(args),
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()
        return proc.returncode, out.decode('utf-8')

    def test_usage_shows_help(self):
        returncode, out = self.run_script('-h')
        self.assertEqual(returncode, 0)
        self.assertIn('Usage:', out)

    def test_version_shows_v1_0(self):
        returncode, out = self.run_script('--version')
        self.assertEqual(returncode, 0)
        self.assertIn('luksmount.py v1.0', out)

    def test_build_paths(self):
        self.assertEqual(luksmount.build_paths('sdb', 'disk3'),
                         ('/dev/sdb', '/dev/mapper/disk3', '/mnt/user/disk3'))

    def test_build_open_command(self):
        self.assertEqual(luksmount.build_open_command('/dev/sdb', 'disk3'),
                         ['sudo', 'cryptsetup', 'open', '/dev/sdb', 'disk3'])

    def test_build_mount_command(self):
        self.assertEqual(luksmount.build_mount_command('/dev/mapper/disk3', '/mnt/user/disk3'),
                         ['sudo', 'mount', '/dev/mapper/disk3', '/mnt/user/disk3'])

    @patch('luksmount.run_command')
    @patch('luksmount.check_required_commands', return_value=0)
    def test_invalid_device_name_is_rejected(self, mock_check, mock_run):
        with patch.object(sys, 'argv', ['luksmount.py', '/dev/sdb', 'disk3']):
            self.assertEqual(luksmount.main(), 1)
        mock_run.assert_not_called()

    @patch('luksmount.run_command')
    @patch('luksmount.check_required_commands', return_value=0)
    def test_invalid_mapper_name_is_rejected(self, mock_check, mock_run):
        with patch.object(sys, 'argv', ['luksmount.py', 'sdb', '../disk3']):
            self.assertEqual(luksmount.main(), 1)
        mock_run.assert_not_called()

    @patch('luksmount.run_command')
    @patch('luksmount.check_sudo')
    @patch('luksmount.confirm')
    @patch('luksmount.get_serial', return_value=None)
    @patch('luksmount.validate_paths', return_value=0)
    def test_serial_failure_has_no_side_effect(self, mock_validate, mock_serial,
                                               mock_confirm, mock_sudo, mock_run):
        self.assertEqual(luksmount.process_mount('sdb', 'disk3'), 1)
        mock_confirm.assert_not_called()
        mock_sudo.assert_not_called()
        mock_run.assert_not_called()

    @patch('luksmount.run_command')
    @patch('luksmount.check_sudo')
    @patch('luksmount.confirm', return_value=False)
    @patch('luksmount.get_serial', return_value='SERIAL123')
    @patch('luksmount.validate_paths', return_value=0)
    def test_decline_has_no_side_effect(self, mock_validate, mock_serial,
                                        mock_confirm, mock_sudo, mock_run):
        self.assertEqual(luksmount.process_mount('sdb', 'disk3'), 0)
        mock_sudo.assert_not_called()
        mock_run.assert_not_called()

    @patch('luksmount.run_command', return_value=2)
    @patch('luksmount.check_sudo', return_value=True)
    @patch('luksmount.confirm', return_value=True)
    @patch('luksmount.get_serial', return_value='SERIAL123')
    @patch('luksmount.validate_paths', return_value=0)
    def test_cryptsetup_failure_skips_mount(self, mock_validate, mock_serial,
                                            mock_confirm, mock_sudo, mock_run):
        self.assertEqual(luksmount.process_mount('sdb', 'disk3'), 1)
        self.assertEqual(mock_run.call_count, 1)
        mock_run.assert_called_once_with(['sudo', 'cryptsetup', 'open', '/dev/sdb', 'disk3'])

    @patch('luksmount.run_command', side_effect=[0, 32])
    @patch('luksmount.check_sudo', return_value=True)
    @patch('luksmount.confirm', return_value=True)
    @patch('luksmount.get_serial', return_value='SERIAL123')
    @patch('luksmount.validate_paths', return_value=0)
    def test_mount_failure_leaves_mapper_open(self, mock_validate, mock_serial,
                                              mock_confirm, mock_sudo, mock_run):
        self.assertEqual(luksmount.process_mount('sdb', 'disk3'), 1)
        self.assertEqual(mock_run.call_count, 2)
        executed = [call[0][0] for call in mock_run.call_args_list]
        self.assertEqual(executed[0], ['sudo', 'cryptsetup', 'open', '/dev/sdb', 'disk3'])
        self.assertEqual(executed[1], ['sudo', 'mount', '/dev/mapper/disk3', '/mnt/user/disk3'])
        for command in executed:
            self.assertNotIn('close', command)
            self.assertNotIn('luksClose', command)
            self.assertNotIn('dmsetup', command)

    @patch('luksmount.run_command', return_value=0)
    @patch('luksmount.check_sudo', return_value=True)
    @patch('luksmount.confirm', return_value=True)
    @patch('luksmount.get_serial', return_value='SERIAL123')
    @patch('luksmount.validate_paths', return_value=0)
    def test_success_opens_then_mounts(self, mock_validate, mock_serial,
                                       mock_confirm, mock_sudo, mock_run):
        self.assertEqual(luksmount.process_mount('sdb', 'disk3'), 0)
        executed = [call[0][0] for call in mock_run.call_args_list]
        self.assertEqual(executed, [
            ['sudo', 'cryptsetup', 'open', '/dev/sdb', 'disk3'],
            ['sudo', 'mount', '/dev/mapper/disk3', '/mnt/user/disk3'],
        ])

    @patch('luksmount.run_command')
    @patch('luksmount.check_sudo')
    @patch('luksmount.get_serial')
    @patch('luksmount.os.path.isdir', return_value=True)
    @patch('luksmount.os.path.exists', return_value=True)
    @patch('luksmount.is_block_device', return_value=True)
    def test_existing_mapper_is_rejected(self, mock_blk, mock_exists, mock_isdir,
                                         mock_serial, mock_sudo, mock_run):
        self.assertEqual(luksmount.process_mount('sdb', 'disk3'), 1)
        mock_serial.assert_not_called()
        mock_sudo.assert_not_called()
        mock_run.assert_not_called()

    @patch('luksmount.run_command')
    @patch('luksmount.check_sudo')
    @patch('luksmount.get_serial')
    @patch('luksmount.os.path.isdir', return_value=False)
    @patch('luksmount.is_block_device', return_value=True)
    def test_missing_mountpoint_is_rejected(self, mock_blk, mock_isdir,
                                            mock_serial, mock_sudo, mock_run):
        def exists(path):
            return path == '/dev/sdb'

        with patch('luksmount.os.path.exists', side_effect=exists):
            self.assertEqual(luksmount.process_mount('sdb', 'disk3'), 1)
        mock_serial.assert_not_called()
        mock_sudo.assert_not_called()
        mock_run.assert_not_called()

    def make_fake_path(self, commands, non_executable):
        """ Create a temporary PATH directory containing the given commands; return its path. """
        directory = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, directory)
        for command in commands:
            path = os.path.join(directory, command)
            with open(path, 'w', encoding='utf-8') as f:
                f.write('#!/bin/sh\n')
            if command not in non_executable:
                os.chmod(path, 0o755)
            else:
                os.chmod(path, 0o644)
        return directory

    @patch('luksmount.process_mount')
    def test_missing_required_command_returns_127(self, mock_process):
        directory = self.make_fake_path(['get-serial', 'mount', 'sudo'], [])
        with patch.dict(os.environ, {'PATH': directory}):
            with patch.object(sys, 'argv', ['luksmount.py', 'sdb', 'disk3']):
                self.assertEqual(luksmount.main(), 127)
        mock_process.assert_not_called()

    @patch('luksmount.process_mount')
    def test_non_executable_required_command_returns_126(self, mock_process):
        directory = self.make_fake_path(['get-serial', 'cryptsetup', 'mount', 'sudo'], ['cryptsetup'])
        with patch.dict(os.environ, {'PATH': directory}):
            with patch.object(sys, 'argv', ['luksmount.py', 'sdb', 'disk3']):
                self.assertEqual(luksmount.main(), 126)
        mock_process.assert_not_called()


if __name__ == '__main__':
    unittest.main()
