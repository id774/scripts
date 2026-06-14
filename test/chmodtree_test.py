#!/usr/bin/env python

########################################################################
# chmodtree_test.py: Tests for chmodtree.py
#
#  Description:
#  This test suite is designed to test the chmodtree.py script, focusing on
#  recursive normalization of file permissions, directory permissions, owner,
#  and group within a specified directory tree.
#
#  The tests cover command construction, default mismatch-only filtering,
#  forced all-entry reapplication, batched find -exec ... {} + execution,
#  owner/group normalization with chown, command availability checks, and
#  exit status propagation.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Running the tests:
#  Execute the test script from the command line:
#      python test/chmodtree_test.py
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Verify check_command does nothing (no output, no exit) when the command exists and is executable.
#    - Verify check_command prints an error and exits with code 127 when the command does not exist.
#    - Verify check_command prints an error and exits with code 126 when the command exists but is not executable.
#    - Verify octal permission mode detection for numeric and symbolic modes.
#    - Verify base find command construction with and without name patterns.
#    - Apply chmod to files only with numeric mode and mismatch-only filtering.
#    - Apply chmod to directories only with sudo and Linux chmod -c behavior.
#    - Apply chmod to both files and directories with a name pattern.
#    - Apply chmod with symbolic mode without mismatch-only permission filtering.
#    - Apply chmod with --force without mismatch-only permission filtering.
#    - Build chown specs for user, group, and user:group normalization.
#    - Build owner/group mismatch filters for chown.
#    - Apply chown to user and group only when entries differ.
#    - Apply chown to all matched entries with --force.
#    - Apply chmod and chown together and verify execution order.
#    - Verify chmodtree returns the first non-zero exit status while continuing remaining operations.
#    - Verify main checks chmod only when chmod options are used.
#    - Verify main checks chown only when owner/group options are used.
#
#  Version History:
#  v1.4 2026-06-14
#       Expanded tests for chmodtree.py v3.1 owner/group normalization,
#       default mismatch-only behavior, --force, symbolic chmod modes, octal
#       mode detection, chown command construction, conditional command checks,
#       and combined chmod/chown execution.
#  v1.3 2026-06-13
#       Updated expected chmodtree command construction for argument-list execution,
#       batched find -exec ... {} + usage, chmod -- mode separation, and command
#       failure propagation.
#  v1.2 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.1 2024-01-28
#       Added individual test cases for check_command function to verify
#       behavior with existing, nonexistent, and non-executable commands.
#  v1.0 2023-12-15
#       First release of the test suite for chmodtree.py.
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import MagicMock, call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import chmodtree


class TestChmodTree(unittest.TestCase):
    """ Unit tests for the chmodtree.py script. """

    def make_options(self, sudo=False, quiet=True, files=None, dirs=None,
                     name=None, user=None, group=None, force=False):
        """ Create a simple options object for chmodtree tests. """
        options = MagicMock()
        options.sudo = sudo
        options.quiet = quiet
        options.files = files
        options.dirs = dirs
        options.name = name
        options.user = user
        options.group = group
        options.force = force
        return options

    def test_usage_shows_help(self):
        """ Test that -h prints the script header and exits successfully. """
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'chmodtree.py')

        proc = subprocess.Popen(['python', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))
        self.assertIn('--user=USER', out.decode('utf-8'))
        self.assertIn('--group=GROUP', out.decode('utf-8'))
        self.assertIn('--force', out.decode('utf-8'))

    @patch('chmodtree.os.access')
    @patch('chmodtree.find_command')
    @patch('chmodtree.sys.exit')
    @patch('chmodtree.print')
    def test_check_command_with_existing_executable_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that exists and is executable. """
        mock_find_command.return_value = '/usr/bin/command'
        mock_access.return_value = True
        chmodtree.check_command('command')
        mock_exit.assert_not_called()
        mock_print.assert_not_called()

    @patch('chmodtree.os.access')
    @patch('chmodtree.find_command')
    @patch('chmodtree.sys.exit')
    @patch('chmodtree.print')
    def test_check_command_with_nonexistent_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that does not exist. """
        mock_find_command.return_value = None
        chmodtree.check_command('nonexistent')
        mock_print.assert_called_with("[ERROR] Command 'nonexistent' is not installed. Please install nonexistent and try again.", file=sys.stderr)
        mock_exit.assert_called_with(127)

    @patch('chmodtree.os.access')
    @patch('chmodtree.find_command')
    @patch('chmodtree.sys.exit')
    @patch('chmodtree.print')
    def test_check_command_with_nonexecutable_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that exists but is not executable. """
        mock_find_command.return_value = '/usr/bin/nonexecutable'
        mock_access.return_value = False
        chmodtree.check_command('nonexecutable')
        mock_print.assert_called_with("[ERROR] Command 'nonexecutable' is not executable. Please check the permissions.", file=sys.stderr)
        mock_exit.assert_called_with(126)

    def test_is_octal_mode_accepts_numeric_modes(self):
        """ Test octal mode detection for supported numeric permission forms. """
        self.assertTrue(chmodtree.is_octal_mode('644'))
        self.assertTrue(chmodtree.is_octal_mode('0644'))
        self.assertTrue(chmodtree.is_octal_mode('1777'))
        self.assertTrue(chmodtree.is_octal_mode('02755'))

    def test_is_octal_mode_rejects_symbolic_and_invalid_modes(self):
        """ Test octal mode detection rejects symbolic and invalid modes. """
        self.assertFalse(chmodtree.is_octal_mode(None))
        self.assertFalse(chmodtree.is_octal_mode(''))
        self.assertFalse(chmodtree.is_octal_mode('u+rw'))
        self.assertFalse(chmodtree.is_octal_mode('a+rX'))
        self.assertFalse(chmodtree.is_octal_mode('0888'))
        self.assertFalse(chmodtree.is_octal_mode('64'))

    def test_build_find_command_without_name(self):
        """ Test base find command construction without a name pattern. """
        options = self.make_options()
        self.assertEqual(chmodtree.build_find_command(options, 'test dir'),
                         ['find', 'test dir'])

    def test_build_find_command_with_name(self):
        """ Test base find command construction with a name pattern. """
        options = self.make_options(name='*.py')
        self.assertEqual(chmodtree.build_find_command(options, 'test dir'),
                         ['find', 'test dir', '-name', '*.py'])

    @patch('chmodtree.os_exec')
    def test_files_chmod_uses_mismatch_filter_by_default(self, mock_os_exec):
        """ Test chmod application to files only uses ! -perm for numeric mode. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=False,
            quiet=True,
            files='0644',
            dirs=None,
            name='*.py'
        )

        status = chmodtree.chmodtree(options, 'testdir1')

        self.assertEqual(status, 0)
        mock_os_exec.assert_called_once_with([
            'find', 'testdir1',
            '-name', '*.py',
            '-type', 'f',
            '!', '-perm', '0644',
            '-exec', 'chmod',
            '--', '0644', '{}', '+'
        ])

    @patch('chmodtree.sys.platform', 'linux')
    @patch('chmodtree.os_exec')
    def test_dirs_chmod_with_sudo_uses_linux_changes_option(self, mock_os_exec):
        """ Test chmod application to directories only, using sudo and chmod -c. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=True,
            quiet=False,
            files=None,
            dirs='0755',
            name=None
        )

        status = chmodtree.chmodtree(options, 'testdir2')

        self.assertEqual(status, 0)
        mock_os_exec.assert_called_once_with([
            'sudo',
            'find', 'testdir2',
            '-type', 'd',
            '!', '-perm', '0755',
            '-exec', 'chmod',
            '-c',
            '--', '0755', '{}', '+'
        ])

    @patch('chmodtree.os_exec')
    def test_files_and_dirs_chmod_with_pattern(self, mock_os_exec):
        """ Test chmod application to both files and directories with a name pattern. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=True,
            quiet=True,
            files='0600',
            dirs='0700',
            name='*.rb'
        )

        status = chmodtree.chmodtree(options, 'testdir3')

        self.assertEqual(status, 0)
        expected_calls = [
            call([
                'sudo',
                'find', 'testdir3',
                '-name', '*.rb',
                '-type', 'f',
                '!', '-perm', '0600',
                '-exec', 'chmod',
                '--', '0600', '{}', '+'
            ]),
            call([
                'sudo',
                'find', 'testdir3',
                '-name', '*.rb',
                '-type', 'd',
                '!', '-perm', '0700',
                '-exec', 'chmod',
                '--', '0700', '{}', '+'
            ])
        ]
        self.assertEqual(mock_os_exec.call_args_list, expected_calls)

    @patch('chmodtree.sys.platform', 'linux')
    @patch('chmodtree.os_exec')
    def test_files_and_dirs_sudo_verbose_chmod(self, mock_os_exec):
        """ Test chmod application to files and directories with sudo and verbose output. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=True,
            quiet=False,
            files='0640',
            dirs='0750',
            name='*.txt'
        )

        status = chmodtree.chmodtree(options, 'testdir4')

        self.assertEqual(status, 0)
        expected_calls = [
            call([
                'sudo',
                'find', 'testdir4',
                '-name', '*.txt',
                '-type', 'f',
                '!', '-perm', '0640',
                '-exec', 'chmod',
                '-c',
                '--', '0640', '{}', '+'
            ]),
            call([
                'sudo',
                'find', 'testdir4',
                '-name', '*.txt',
                '-type', 'd',
                '!', '-perm', '0750',
                '-exec', 'chmod',
                '-c',
                '--', '0750', '{}', '+'
            ])
        ]
        self.assertEqual(mock_os_exec.call_args_list, expected_calls)

    @patch('chmodtree.os_exec')
    def test_symbolic_chmod_does_not_use_permission_filter(self, mock_os_exec):
        """ Test symbolic chmod modes are not pre-filtered with ! -perm. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=False,
            quiet=True,
            files='u+rw',
            dirs=None,
            name='*.conf'
        )

        status = chmodtree.chmodtree(options, 'testdir5')

        self.assertEqual(status, 0)
        mock_os_exec.assert_called_once_with([
            'find', 'testdir5',
            '-name', '*.conf',
            '-type', 'f',
            '-exec', 'chmod',
            '--', 'u+rw', '{}', '+'
        ])

    @patch('chmodtree.os_exec')
    def test_force_chmod_does_not_use_permission_filter(self, mock_os_exec):
        """ Test --force disables mismatch-only permission filtering. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=False,
            quiet=True,
            files='0644',
            dirs='0755',
            force=True
        )

        status = chmodtree.chmodtree(options, 'testdir6')

        self.assertEqual(status, 0)
        expected_calls = [
            call([
                'find', 'testdir6',
                '-type', 'f',
                '-exec', 'chmod',
                '--', '0644', '{}', '+'
            ]),
            call([
                'find', 'testdir6',
                '-type', 'd',
                '-exec', 'chmod',
                '--', '0755', '{}', '+'
            ])
        ]
        self.assertEqual(mock_os_exec.call_args_list, expected_calls)

    def test_build_chown_spec_with_user_and_group(self):
        """ Test chown spec construction for user:group. """
        options = self.make_options(user='root', group='adm')
        self.assertEqual(chmodtree.build_chown_spec(options), 'root:adm')

    def test_build_chown_spec_with_user_only(self):
        """ Test chown spec construction for user only. """
        options = self.make_options(user='root')
        self.assertEqual(chmodtree.build_chown_spec(options), 'root')

    def test_build_chown_spec_with_group_only(self):
        """ Test chown spec construction for group only. """
        options = self.make_options(group='adm')
        self.assertEqual(chmodtree.build_chown_spec(options), ':adm')

    def test_build_chown_spec_without_user_or_group(self):
        """ Test chown spec construction without owner/group returns None. """
        options = self.make_options()
        self.assertIsNone(chmodtree.build_chown_spec(options))

    def test_build_chown_filter_with_user_and_group(self):
        """ Test owner/group mismatch filter construction. """
        options = self.make_options(user='root', group='adm')
        self.assertEqual(
            chmodtree.build_chown_filter(options),
            ['(', '!', '-user', 'root', '-o', '!', '-group', 'adm', ')']
        )

    def test_build_chown_filter_with_user_only(self):
        """ Test owner mismatch filter construction. """
        options = self.make_options(user='root')
        self.assertEqual(
            chmodtree.build_chown_filter(options),
            ['!', '-user', 'root']
        )

    def test_build_chown_filter_with_group_only(self):
        """ Test group mismatch filter construction. """
        options = self.make_options(group='adm')
        self.assertEqual(
            chmodtree.build_chown_filter(options),
            ['!', '-group', 'adm']
        )

    def test_build_chown_filter_with_force(self):
        """ Test --force disables owner/group mismatch filtering. """
        options = self.make_options(user='root', group='adm', force=True)
        self.assertEqual(chmodtree.build_chown_filter(options), [])

    @patch('chmodtree.sys.platform', 'linux')
    @patch('chmodtree.os_exec')
    def test_chown_user_and_group_command(self, mock_os_exec):
        """ Test user/group normalization uses chown with mismatch filters. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=False,
            quiet=False,
            user='root',
            group='root'
        )

        status = chmodtree.chmodtree(options, 'testdir7')

        self.assertEqual(status, 0)
        mock_os_exec.assert_called_once_with([
            'find', 'testdir7',
            '(', '!', '-user', 'root', '-o', '!', '-group', 'root', ')',
            '-exec', 'chown',
            '-c',
            '--', 'root:root', '{}', '+'
        ])

    @patch('chmodtree.os_exec')
    def test_chown_group_only_command(self, mock_os_exec):
        """ Test group-only normalization uses chown :group. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=True,
            quiet=True,
            group='adm'
        )

        status = chmodtree.chmodtree(options, 'testdir8')

        self.assertEqual(status, 0)
        mock_os_exec.assert_called_once_with([
            'sudo',
            'find', 'testdir8',
            '!', '-group', 'adm',
            '-exec', 'chown',
            '--', ':adm', '{}', '+'
        ])

    @patch('chmodtree.os_exec')
    def test_force_chown_command(self, mock_os_exec):
        """ Test --force applies chown to all matched entries. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=False,
            quiet=True,
            user='root',
            group='root',
            force=True
        )

        status = chmodtree.chmodtree(options, 'testdir9')

        self.assertEqual(status, 0)
        mock_os_exec.assert_called_once_with([
            'find', 'testdir9',
            '-exec', 'chown',
            '--', 'root:root', '{}', '+'
        ])

    @patch('chmodtree.os_exec')
    def test_combined_chmod_and_chown_execution_order(self, mock_os_exec):
        """ Test chmod files, chmod dirs, and chown run in a stable order. """
        mock_os_exec.return_value = 0
        options = self.make_options(
            sudo=True,
            quiet=True,
            files='0644',
            dirs='0755',
            user='root',
            group='root',
            name='*.sh'
        )

        status = chmodtree.chmodtree(options, 'testdir10')

        self.assertEqual(status, 0)
        expected_calls = [
            call([
                'sudo',
                'find', 'testdir10',
                '-name', '*.sh',
                '-type', 'f',
                '!', '-perm', '0644',
                '-exec', 'chmod',
                '--', '0644', '{}', '+'
            ]),
            call([
                'sudo',
                'find', 'testdir10',
                '-name', '*.sh',
                '-type', 'd',
                '!', '-perm', '0755',
                '-exec', 'chmod',
                '--', '0755', '{}', '+'
            ]),
            call([
                'sudo',
                'find', 'testdir10',
                '-name', '*.sh',
                '(', '!', '-user', 'root', '-o', '!', '-group', 'root', ')',
                '-exec', 'chown',
                '--', 'root:root', '{}', '+'
            ])
        ]
        self.assertEqual(mock_os_exec.call_args_list, expected_calls)

    @patch('chmodtree.os_exec')
    def test_chmodtree_returns_first_nonzero_status(self, mock_os_exec):
        """ Test chmodtree returns the first non-zero status while continuing later operations. """
        mock_os_exec.side_effect = [5, 7, 9]
        options = self.make_options(
            sudo=False,
            quiet=True,
            files='0644',
            dirs='0755',
            user='root',
            group='root'
        )

        status = chmodtree.chmodtree(options, 'testdir11')

        self.assertEqual(status, 5)
        self.assertEqual(mock_os_exec.call_count, 3)

    @patch('chmodtree.os_exec')
    def test_chmodtree_returns_later_nonzero_status_when_first_succeeds(self, mock_os_exec):
        """ Test chmodtree returns a later non-zero status when earlier operations succeed. """
        mock_os_exec.side_effect = [0, 7, 9]
        options = self.make_options(
            sudo=False,
            quiet=True,
            files='0644',
            dirs='0755',
            user='root',
            group='root'
        )

        status = chmodtree.chmodtree(options, 'testdir12')

        self.assertEqual(status, 7)
        self.assertEqual(mock_os_exec.call_count, 3)

    @patch('chmodtree.check_sudo')
    @patch('chmodtree.chmodtree')
    @patch('chmodtree.check_command')
    @patch('chmodtree.setup_option_parser')
    def test_main_checks_chmod_only_when_chmod_options_are_used(self, mock_parser_setup, mock_check_command, mock_chmodtree, mock_check_sudo):
        """ Test main checks chmod when file or directory modes are requested. """
        parser = MagicMock()
        options = self.make_options(files='0644', dirs=None, user=None, group=None, sudo=False)
        parser.parse_args.return_value = (options, ['target'])
        mock_parser_setup.return_value = parser
        mock_chmodtree.return_value = 0

        status = chmodtree.main()

        self.assertEqual(status, 0)
        mock_check_command.assert_has_calls([call('find'), call('chmod')])
        self.assertNotIn(call('chown'), mock_check_command.call_args_list)
        mock_check_sudo.assert_not_called()
        mock_chmodtree.assert_called_once_with(options, 'target')

    @patch('chmodtree.check_sudo')
    @patch('chmodtree.chmodtree')
    @patch('chmodtree.check_command')
    @patch('chmodtree.setup_option_parser')
    def test_main_checks_chown_only_when_owner_or_group_options_are_used(self, mock_parser_setup, mock_check_command, mock_chmodtree, mock_check_sudo):
        """ Test main checks chown when owner or group normalization is requested. """
        parser = MagicMock()
        options = self.make_options(files=None, dirs=None, user='root', group='root', sudo=True)
        parser.parse_args.return_value = (options, ['target'])
        mock_parser_setup.return_value = parser
        mock_chmodtree.return_value = 0

        status = chmodtree.main()

        self.assertEqual(status, 0)
        mock_check_command.assert_has_calls([call('find'), call('chown')])
        self.assertNotIn(call('chmod'), mock_check_command.call_args_list)
        mock_check_sudo.assert_called_once()
        mock_chmodtree.assert_called_once_with(options, 'target')

    @patch('chmodtree.chmodtree')
    @patch('chmodtree.check_command')
    @patch('chmodtree.setup_option_parser')
    def test_main_returns_one_when_argument_count_is_invalid(self, mock_parser_setup, mock_check_command, mock_chmodtree):
        """ Test main returns 1 and prints help when the directory argument is missing. """
        parser = MagicMock()
        options = self.make_options()
        parser.parse_args.return_value = (options, [])
        mock_parser_setup.return_value = parser

        status = chmodtree.main()

        self.assertEqual(status, 1)
        parser.print_help.assert_called_once()
        mock_check_command.assert_not_called()
        mock_chmodtree.assert_not_called()


if __name__ == '__main__':
    unittest.main()
