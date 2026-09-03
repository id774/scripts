#!/usr/bin/env python

########################################################################
# tcmount_test.py: Tests for tcmount.py
#
#  Description:
#  This test suite is designed to test the tcmount.py script, focusing on
#  the functionality of building argument lists for mounting and unmounting
#  TrueCrypt and VeraCrypt encrypted devices, and on the orchestration
#  functions that run them without a shell. It tests various combinations
#  of options, including TrueCrypt and VeraCrypt compatibility modes, and
#  verifies that command failures propagate to the script's exit status.
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
#    - Build a default TrueCrypt mount argument list for a block device with UTF-8 filesystem options.
#    - Build a mount argument list with read-only filesystem option.
#    - Build a mount argument list without UTF-8 filesystem option.
#    - Build a mount argument list with an explicit target mount directory.
#    - Build a mount argument list for VeraCrypt TC-compat mode with the correct tool tokens.
#    - Build a detach argument list for a resolved mountpoint.
#    - Build an external container mount argument list using the default mount target (device name).
#    - Build an external container mount argument list honoring an explicit target mount directory.
#    - Build an external container unmount argument list by its fixed file path.
#    - list_all_devices() includes the first (sdc) and last (sdz) devices.
#    - Detect TrueCrypt installation when the truecrypt command is available.
#    - Detect VeraCrypt installation when the veracrypt command is available.
#    - find_command() resolves an executable command found on PATH.
#    - find_command() returns None when the command is not found on PATH.
#    - find_command() resolves an empty PATH component to the current directory.
#    - command_exists() is consistent with find_command().
#    - os_exec() runs an argument list via subprocess.call without a shell and returns its status.
#    - os_exec() returns 1 and prints a diagnostic when the command cannot be executed.
#    - is_block_device() returns True only for a path whose mode is a block device.
#    - resolve_real_mountpoint() returns the resolved path when get-device and get-mountpoint both succeed.
#    - resolve_real_mountpoint() returns None and does not call get-mountpoint when get-device fails.
#    - resolve_real_mountpoint() returns None when get-mountpoint fails after get-device succeeds.
#    - run_single_mount() rejects a non-block-device source without executing a command.
#    - run_single_mount() returns the propagated status of a successful mount.
#    - run_single_mount() returns the propagated status of a failed mount.
#    - run_single_unmount() returns 1 and does not execute a detach command when resolution fails.
#    - run_single_unmount() returns the propagated status of the detach command on success.
#    - run_external_mount() rejects a missing container file without executing a command.
#    - run_external_unmount() always detaches by the container file path.
#    - run_mount_all() continues past a mid-sequence failure and aggregates status 1.
#    - run_mount_all() returns 0 only when every device mounts successfully.
#    - In process_mounting(), no shell metacharacter in a device/target value is interpreted as a second command.
#    - In process_mounting(), mount and unmount using mocked orchestration (TrueCrypt path).
#    - In process_mounting(), mount and unmount using mocked orchestration (VeraCrypt path).
#    - In process_mounting(), mount and unmount using mocked orchestration (VeraCrypt TC-compat path).
#    - In process_mounting(), pass an explicit target to run_single_mount and run_single_unmount.
#    - In process_mounting(), a failed single mount propagates status 1 to the caller.
#    - In process_mounting(), a failed single unmount propagates status 1 to the caller.
#    - In process_mounting(), custom exit code 11 when TrueCrypt is required but missing.
#    - In process_mounting(), custom exit code 12 when VeraCrypt is required but missing (-v).
#    - In process_mounting(), custom exit code 13 when VeraCrypt is required but missing (-t).
#    - In process_mounting(), generate expected mount argument lists across option combinations
#      (veracrypt/tc_compat/no_utf8/readonly/all/external).
#    - In process_mounting(), delegate external mount (-e) to run_external_mount with an explicit target.
#    - In process_mounting(), unmount an external container with default arguments.
#    - In process_mounting(), unmount an external container even when an explicit target is provided.
#    - In process_mounting(), --all continues through every device and aggregates a failure.
#
#  Version History:
#  v1.4 2026-09-03
#       Rewrote the suite for the argv-based mount/unmount execution: no
#       shell=True anywhere, command failures propagate to process_mounting()'s
#       return value, --all continues past failures and aggregates status, and
#       get-device/get-mountpoint failures block the detach command. Added
#       tests for the manual PATH search in find_command().
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
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import tcmount


def make_options(veracrypt=False, tc_compat=False, no_utf8=False, readonly=False,
                 all_devices=False, external=None):
    """ Build a minimal stand-in for optparse's Values object. """
    def options():
        return None
    options.veracrypt = veracrypt
    options.tc_compat = tc_compat
    options.no_utf8 = no_utf8
    options.readonly = readonly
    options.all = all_devices
    options.external = external
    return options


class TestTcMount(unittest.TestCase):
    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'tcmount.py')

        proc = subprocess.Popen(['python', script_path, '-h'],
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

    # -- argument-list builders -----------------------------------------

    def test_build_mount_argv(self):
        expected = ['sudo', 'truecrypt', '-t', '-k', '', '--protect-hidden=no',
                    '--fs-options=utf8', '/dev/sdb', os.path.expanduser('~/mnt/sdb')]
        result = tcmount.build_mount_argv(['truecrypt'], 'sdb', 'utf8')
        self.assertEqual(result, expected)

    def test_build_mount_argv_with_readonly(self):
        expected = ['sudo', 'truecrypt', '-t', '-k', '', '--protect-hidden=no',
                    '--fs-options=ro', '/dev/sdb', os.path.expanduser('~/mnt/sdb')]
        result = tcmount.build_mount_argv(['truecrypt'], 'sdb', 'ro')
        self.assertEqual(result, expected)

    def test_build_mount_argv_without_utf8(self):
        expected = ['sudo', 'truecrypt', '-t', '-k', '', '--protect-hidden=no',
                    '--fs-options=', '/dev/sdb', os.path.expanduser('~/mnt/sdb')]
        result = tcmount.build_mount_argv(['truecrypt'], 'sdb', '')
        self.assertEqual(result, expected)

    def test_build_mount_argv_with_explicit_target(self):
        expected = ['sudo', 'truecrypt', '-t', '-k', '', '--protect-hidden=no',
                    '--fs-options=utf8', '/dev/sdb', os.path.expanduser('~/mnt/disk1')]
        result = tcmount.build_mount_argv(['truecrypt'], 'sdb', 'utf8', 'disk1')
        self.assertEqual(result, expected)

    def test_build_mount_argv_tc_compat_tool_tokens(self):
        # tc-compat mounts as two argv words: 'veracrypt', '-tc'.
        expected = ['sudo', 'veracrypt', '-tc', '-t', '-k', '', '--protect-hidden=no',
                    '--fs-options=utf8', '/dev/sdb', os.path.expanduser('~/mnt/sdb')]
        result = tcmount.build_mount_argv(['veracrypt', '-tc'], 'sdb', 'utf8')
        self.assertEqual(result, expected)

    def test_build_detach_argv(self):
        expected = ['sudo', 'truecrypt', '-d', '/mnt/real']
        result = tcmount.build_detach_argv(['truecrypt'], '/mnt/real')
        self.assertEqual(result, expected)

    def test_build_detach_argv_tc_compat_uses_bare_veracrypt(self):
        # Unmount in TC-compat mode uses plain 'veracrypt', not '-tc'.
        expected = ['sudo', 'veracrypt', '-d', '/mnt/real']
        result = tcmount.build_detach_argv(['veracrypt'], '/mnt/real')
        self.assertEqual(result, expected)

    def test_build_mount_external_argv_default_target(self):
        expected = ['sudo', 'truecrypt', '-t', '-k', '', '--protect-hidden=no',
                    '--fs-options=utf8',
                    os.path.expanduser('~/mnt/external/container.tc'),
                    os.path.expanduser('~/mnt/sde')]
        result = tcmount.build_mount_external_argv(['truecrypt'], 'utf8', 'sde')
        self.assertEqual(result, expected)

    def test_build_mount_external_argv_explicit_target(self):
        expected = ['sudo', 'truecrypt', '-t', '-k', '', '--protect-hidden=no',
                    '--fs-options=utf8',
                    os.path.expanduser('~/mnt/external/container.tc'),
                    os.path.expanduser('~/mnt/disk3')]
        result = tcmount.build_mount_external_argv(['truecrypt'], 'utf8', 'disk3')
        self.assertEqual(result, expected)

    def test_build_unmount_external_argv(self):
        expected = ['sudo', 'truecrypt', '-d', os.path.expanduser('~/mnt/external/container.tc')]
        result = tcmount.build_unmount_external_argv(['truecrypt'])
        self.assertEqual(result, expected)

    def test_list_all_devices(self):
        result = tcmount.list_all_devices()
        self.assertEqual(result[0], 'sdc')
        self.assertEqual(result[-1], 'sdz')

    # -- detection and PATH lookup ---------------------------------------

    @patch('tcmount.find_command', return_value='/usr/bin/truecrypt')
    def test_is_truecrypt_installed(self, mock_find):
        self.assertTrue(tcmount.is_truecrypt_installed())
        mock_find.assert_called_with('truecrypt')

    @patch('tcmount.find_command', return_value='/usr/bin/veracrypt')
    def test_is_veracrypt_installed(self, mock_find):
        self.assertTrue(tcmount.is_veracrypt_installed())
        mock_find.assert_called_with('veracrypt')

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

    def test_find_command_resolves_executable_on_path(self):
        directory = self.make_fake_path(['truecrypt'], [])
        with patch.dict(os.environ, {'PATH': directory}):
            found = tcmount.find_command('truecrypt')
            self.assertTrue(tcmount.command_exists('truecrypt'))
        self.assertEqual(os.path.realpath(found),
                         os.path.realpath(os.path.join(directory, 'truecrypt')))

    def test_find_command_missing_returns_none(self):
        directory = self.make_fake_path([], [])
        with patch.dict(os.environ, {'PATH': directory}):
            self.assertIsNone(tcmount.find_command('nonexistent_command'))
            self.assertFalse(tcmount.command_exists('nonexistent_command'))

    def test_find_command_empty_path_component_is_cwd(self):
        directory = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, directory)
        command_name = 'fake_command_for_cwd_test'
        path = os.path.join(directory, command_name)
        with open(path, 'w', encoding='utf-8') as f:
            f.write('#!/bin/sh\n')
        os.chmod(path, 0o755)
        original_cwd = os.getcwd()
        self.addCleanup(os.chdir, original_cwd)
        os.chdir(directory)
        with patch.dict(os.environ, {'PATH': ''}):
            found = tcmount.find_command(command_name)
        self.assertEqual(os.path.realpath(found), os.path.realpath(path))

    # -- os_exec / is_block_device ----------------------------------------

    @patch('tcmount.subprocess.call')
    def test_os_exec_runs_argv_without_shell(self, mock_call):
        mock_call.return_value = 0
        argv = ['echo', 'Test Command']
        status = tcmount.os_exec(argv)
        mock_call.assert_called_with(argv)
        self.assertEqual(status, 0)

    @patch('tcmount.subprocess.call')
    def test_os_exec_propagates_nonzero_status(self, mock_call):
        mock_call.return_value = 7
        self.assertEqual(tcmount.os_exec(['false']), 7)

    @patch('tcmount.subprocess.call', side_effect=OSError('not found'))
    def test_os_exec_exec_failure_returns_1(self, mock_call):
        self.assertEqual(tcmount.os_exec(['nonexistent-binary']), 1)

    def test_is_block_device_true_for_block_device(self):
        fake_stat = type('FakeStat', (), {'st_mode': stat.S_IFBLK | 0o600})()
        with patch('tcmount.os.stat', return_value=fake_stat):
            self.assertTrue(tcmount.is_block_device('/dev/sdb'))

    def test_is_block_device_false_for_regular_file(self):
        fake_stat = type('FakeStat', (), {'st_mode': stat.S_IFREG | 0o600})()
        with patch('tcmount.os.stat', return_value=fake_stat):
            self.assertFalse(tcmount.is_block_device('/tmp/not-a-device'))

    def test_is_block_device_false_when_missing(self):
        with patch('tcmount.os.stat', side_effect=OSError()):
            self.assertFalse(tcmount.is_block_device('/dev/does-not-exist'))

    # -- resolve_real_mountpoint -------------------------------------------

    @patch('tcmount.subprocess.check_output')
    def test_resolve_real_mountpoint_success(self, mock_check_output):
        mock_check_output.side_effect = [b'/dev/sdb1\n', b'/mnt/real\n']
        result = tcmount.resolve_real_mountpoint('disk1')
        self.assertEqual(result, '/mnt/real')
        self.assertEqual(mock_check_output.call_args_list[0],
                         call(['get-device', os.path.expanduser('~/mnt/disk1')]))
        self.assertEqual(mock_check_output.call_args_list[1],
                         call(['get-mountpoint', '/dev/sdb1']))

    @patch('tcmount.subprocess.check_output')
    def test_resolve_real_mountpoint_get_device_failure_skips_get_mountpoint(self, mock_check_output):
        mock_check_output.side_effect = subprocess.CalledProcessError(1, 'get-device')
        result = tcmount.resolve_real_mountpoint('disk1')
        self.assertIsNone(result)
        self.assertEqual(mock_check_output.call_count, 1)

    @patch('tcmount.subprocess.check_output')
    def test_resolve_real_mountpoint_get_mountpoint_failure(self, mock_check_output):
        mock_check_output.side_effect = [b'/dev/sdb1\n', subprocess.CalledProcessError(1, 'get-mountpoint')]
        result = tcmount.resolve_real_mountpoint('disk1')
        self.assertIsNone(result)
        self.assertEqual(mock_check_output.call_count, 2)

    # -- run_single_mount / run_single_unmount ------------------------------

    @patch('tcmount.os_exec')
    @patch('tcmount.is_block_device', return_value=False)
    def test_run_single_mount_rejects_non_block_device(self, mock_is_blk, mock_os_exec):
        status = tcmount.run_single_mount('sdb', 'utf8', ['truecrypt'])
        self.assertEqual(status, 1)
        mock_os_exec.assert_not_called()

    @patch('tcmount.os_exec', return_value=0)
    @patch('tcmount.is_block_device', return_value=True)
    def test_run_single_mount_success_status(self, mock_is_blk, mock_os_exec):
        self.assertEqual(tcmount.run_single_mount('sdb', 'utf8', ['truecrypt']), 0)

    @patch('tcmount.os_exec', return_value=1)
    @patch('tcmount.is_block_device', return_value=True)
    def test_run_single_mount_failure_status(self, mock_is_blk, mock_os_exec):
        self.assertEqual(tcmount.run_single_mount('sdb', 'utf8', ['truecrypt']), 1)

    @patch('tcmount.os_exec')
    @patch('tcmount.resolve_real_mountpoint', return_value=None)
    def test_run_single_unmount_resolution_failure_skips_detach(self, mock_resolve, mock_os_exec):
        status = tcmount.run_single_unmount('disk1', ['truecrypt'])
        self.assertEqual(status, 1)
        mock_os_exec.assert_not_called()

    @patch('tcmount.os_exec', return_value=0)
    @patch('tcmount.resolve_real_mountpoint', return_value='/mnt/real')
    def test_run_single_unmount_success_status(self, mock_resolve, mock_os_exec):
        self.assertEqual(tcmount.run_single_unmount('disk1', ['truecrypt']), 0)
        mock_os_exec.assert_called_with(['sudo', 'truecrypt', '-d', '/mnt/real'])

    # -- run_external_mount / run_external_unmount ---------------------------

    @patch('tcmount.os_exec')
    @patch('tcmount.os.path.isfile', return_value=False)
    def test_run_external_mount_missing_container_rejected(self, mock_isfile, mock_os_exec):
        status = tcmount.run_external_mount('sde', 'utf8', ['truecrypt'])
        self.assertEqual(status, 1)
        mock_os_exec.assert_not_called()

    @patch('tcmount.os_exec', return_value=0)
    def test_run_external_unmount_detaches_by_container_path(self, mock_os_exec):
        tcmount.run_external_unmount(['truecrypt'])
        mock_os_exec.assert_called_with(
            ['sudo', 'truecrypt', '-d', os.path.expanduser('~/mnt/external/container.tc')])

    # -- run_mount_all -------------------------------------------------------

    @patch('tcmount.run_single_mount')
    def test_run_mount_all_continues_past_failure_and_aggregates(self, mock_run_single):
        devices = tcmount.list_all_devices()

        def side_effect(device, mount_options, tool_argv, target=None):
            return 1 if device == devices[1] else 0

        mock_run_single.side_effect = side_effect
        status = tcmount.run_mount_all('utf8', ['truecrypt'])
        self.assertEqual(status, 1)
        self.assertEqual(mock_run_single.call_count, len(devices))

    @patch('tcmount.run_single_mount', return_value=0)
    def test_run_mount_all_all_succeed_returns_0(self, mock_run_single):
        self.assertEqual(tcmount.run_mount_all('utf8', ['truecrypt']), 0)
        self.assertEqual(mock_run_single.call_count, len(tcmount.list_all_devices()))

    # -- process_mounting: no-shell / injection safety -----------------------

    @patch('tcmount.subprocess.call')
    @patch('tcmount.is_block_device', return_value=True)
    @patch('tcmount.is_truecrypt_installed', return_value=True)
    def test_process_mounting_does_not_interpret_shell_metacharacters(self, mock_tc, mock_is_blk, mock_call):
        mock_call.return_value = 0
        options = make_options()
        # A target containing shell metacharacters must reach subprocess.call
        # as a single literal argv element, never as shell text.
        malicious_target = 'disk1; touch /tmp/pwned'
        tcmount.process_mounting(options, ['sdb', malicious_target])
        argv = mock_call.call_args[0][0]
        self.assertIsInstance(argv, list)
        self.assertIn(os.path.expanduser('~/mnt/' + malicious_target), argv)
        for token in argv:
            self.assertNotIn(';', token.replace(malicious_target, ''))

    # -- process_mounting: orchestration dispatch ----------------------------

    def process_mounting_test_helper(self, veracrypt=False, tc_compat=False):
        with patch('tcmount.run_single_mount', return_value=0) as mock_run_mount, \
                patch('tcmount.run_single_unmount', return_value=0) as mock_run_unmount:
            options = make_options(veracrypt=veracrypt, tc_compat=tc_compat)

            status = tcmount.process_mounting(options, ['sdb'])
            self.assertEqual(status, 0)
            mock_run_mount.assert_called_with('sdb', 'utf8', mock_run_mount.call_args[0][2], None)

            status = tcmount.process_mounting(options, ['sdb', 'unmount'])
            self.assertEqual(status, 0)
            mock_run_unmount.assert_called_with('sdb', mock_run_unmount.call_args[0][1])

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
        with patch('tcmount.run_single_mount', return_value=0) as mock_run_mount, \
                patch('tcmount.run_single_unmount', return_value=0) as mock_run_unmount, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            options = make_options()

            tcmount.process_mounting(options, ['sdb', 'disk1'])
            mock_run_mount.assert_called_with('sdb', 'utf8', ['truecrypt'], 'disk1')

            tcmount.process_mounting(options, ['sdb', 'disk1', 'unmount'])
            mock_run_unmount.assert_called_with('disk1', ['truecrypt'])

    def test_process_mounting_mount_failure_propagates_status(self):
        with patch('tcmount.run_single_mount', return_value=1), \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            options = make_options()
            self.assertEqual(tcmount.process_mounting(options, ['sdb']), 1)

    def test_process_mounting_unmount_failure_propagates_status(self):
        with patch('tcmount.run_single_unmount', return_value=1), \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            options = make_options()
            self.assertEqual(tcmount.process_mounting(options, ['sdb', 'unmount']), 1)

    @patch('tcmount.is_truecrypt_installed', return_value=False)
    def test_process_mounting_exit_11_when_truecrypt_missing(self, mock_tc):
        options = make_options()
        with self.assertRaises(SystemExit) as cm:
            tcmount.process_mounting(options, ['sdb'])
        self.assertEqual(cm.exception.code, 11)

    @patch('tcmount.is_veracrypt_installed', return_value=False)
    def test_process_mounting_exit_12_when_veracrypt_missing(self, mock_vc):
        options = make_options(veracrypt=True)
        with self.assertRaises(SystemExit) as cm:
            tcmount.process_mounting(options, ['sdb'])
        self.assertEqual(cm.exception.code, 12)

    @patch('tcmount.is_veracrypt_installed', return_value=False)
    def test_process_mounting_exit_13_when_veracrypt_missing_for_tc_compat(self, mock_vc):
        options = make_options(tc_compat=True)
        with self.assertRaises(SystemExit) as cm:
            tcmount.process_mounting(options, ['sdb'])
        self.assertEqual(cm.exception.code, 13)

    @patch('tcmount.os_exec')
    def test_process_mounting_combinations(self, mock_os_exec):
        self.check_both_installed()
        mock_os_exec.return_value = 0

        test_cases = [
            (False, False, False, False, None, 'utf8'),
            (False, False, True, False, None, ''),
            (False, False, False, True, None, 'utf8,ro'),
            (False, False, True, True, None, 'ro'),
            (False, False, False, False, 'sdb', 'utf8'),
            (False, False, True, False, 'sdb', ''),
            (True, False, False, False, None, 'utf8'),
            (True, False, True, True, None, 'ro'),
            (False, True, False, False, None, 'utf8'),
            (False, True, True, True, None, 'ro'),
        ]

        for veracrypt, tc_compat, no_utf8, readonly, external, expected_options in test_cases:
            with self.subTest(veracrypt=veracrypt, tc_compat=tc_compat, no_utf8=no_utf8,
                              readonly=readonly, external=external):
                options = make_options(veracrypt=veracrypt, tc_compat=tc_compat,
                                       no_utf8=no_utf8, readonly=readonly, external=external)
                tcmount.process_mounting(options, ['sdb'])

                if tc_compat:
                    tool_argv = ['veracrypt', '-tc']
                elif veracrypt:
                    tool_argv = ['veracrypt']
                else:
                    tool_argv = ['truecrypt']

                if external:
                    expected_argv = (['sudo'] + tool_argv +
                                     ['-t', '-k', '', '--protect-hidden=no',
                                      '--fs-options=%s' % expected_options,
                                      os.path.expanduser('~/mnt/external/container.tc'),
                                      os.path.expanduser('~/mnt/' + external)])
                else:
                    expected_argv = (['sudo'] + tool_argv +
                                     ['-t', '-k', '', '--protect-hidden=no',
                                      '--fs-options=%s' % expected_options,
                                      '/dev/sdb', os.path.expanduser('~/mnt/sdb')])

                mock_os_exec.assert_called_with(expected_argv)
                mock_os_exec.reset_mock()

    def test_process_mounting_external_with_explicit_target(self):
        with patch('tcmount.run_external_mount', return_value=0) as mock_run_ext, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            options = make_options(external='sde')

            # args includes an unrelated device ('sdb') and an explicit target ('disk3')
            tcmount.process_mounting(options, ['sdb', 'disk3'])

            mock_run_ext.assert_called_with('sde', 'utf8', ['truecrypt'], 'disk3')

    def test_process_mounting_external_unmount_default(self):
        with patch('tcmount.run_external_unmount', return_value=0) as mock_run_ext_unmount, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            options = make_options(external='sde')

            tcmount.process_mounting(options, ['unmount'])

            mock_run_ext_unmount.assert_called_once_with(['truecrypt'])

    def test_process_mounting_external_unmount_explicit_target(self):
        # -e sde disk3 unmount -> detach by container path (target ignored)
        with patch('tcmount.run_external_unmount', return_value=0) as mock_run_ext_unmount, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            options = make_options(external='sde')

            tcmount.process_mounting(options, ['disk3', 'unmount'])

            mock_run_ext_unmount.assert_called_once_with(['truecrypt'])

    def test_process_mounting_all_continues_and_aggregates(self):
        with patch('tcmount.run_mount_all', return_value=1) as mock_run_all, \
                patch('tcmount.is_truecrypt_installed', return_value=True), \
                patch('tcmount.is_veracrypt_installed', return_value=False):
            options = make_options(all_devices=True)
            self.assertEqual(tcmount.process_mounting(options, []), 1)
            mock_run_all.assert_called_once_with('utf8', ['truecrypt'])


if __name__ == '__main__':
    unittest.main()
