#!/usr/bin/env python

########################################################################
# pyck_test.py: Unit tests for pyck.py
#
#  Description:
#  This script contains unit tests for the pyck.py script. It tests
#  the functionality of code style checks, auto-formatting, and removal
#  of unused imports for Python files. The script tests both dry-run
#  and auto-fix modes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script from the command line to perform the tests.
#      python test/pyck_test.py
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Verify check_command does nothing (no output, no exit) when the command exists and is executable.
#    - Verify check_command prints an error and exits with code 127 when the command does not exist.
#    - Verify check_command prints an error and exits with code 126 when the command exists but is not executable.
#    - Run format_file() to invoke autoflake, autopep8, and isort with expected arguments.
#    - Verify format_file() quotes a file path containing spaces before passing it to the shell.
#    - Suppress output on successful run_command() execution.
#    - Print the provided error prefix and command output when run_command() returns a non-zero status.
#    - Print a single literal message when run_command() is called with literal_message=True.
#    - In dry-run mode, run flake8/autoflake/autopep8/isort checks for a single Python file.
#    - In dry-run mode, verify a path containing spaces is quoted before being passed to flake8,
#      autoflake, autopep8, and isort, including the new autopep8 --diff --exit-code command.
#    - In dry-run mode, run flake8/autoflake/autopep8/isort checks for multiple Python files.
#    - In dry-run mode, run flake8/autoflake/autopep8/isort checks for each .py file under a
#      single directory.
#    - In dry-run mode, run flake8/autoflake/autopep8/isort checks for each .py file under
#      multiple directories.
#    - Regression: a flake8-only lint issue (autoflake/autopep8/isort all report no changes)
#      must not produce "Would format:".
#    - Report dry-run flake8 findings as "Lint issue (manual review candidate):".
#    - Run flake8 after autoflake, autopep8, and isort in auto-fix mode.
#    - Report flake8 findings remaining after auto-fix as "Manual fix required:".
#    - Keep exit status 0 when lint findings remain after a successful auto-fix run.
#    - Quote paths containing spaces in the post-fix flake8 command.
#    - Regression: an autopep8-only diff (autoflake/isort report no changes) must produce
#      "Would format:" for the target file.
#    - "Would clean:" is shown only when autoflake reports a change is needed.
#    - "Would sort imports in:" is shown only when isort reports a change is needed.
#    - When autoflake, autopep8, and isort all report no changes, none of "Would clean:",
#      "Would format:", or "Would sort imports in:" is shown.
#    - Dry-run never invokes autoflake -i, autopep8 -i, or a plain (non-check) isort command,
#      and only runs check/diff commands against the target file.
#    - resolve_target_files() resolves a single file, a directory (recursively, .py files
#      only), and reports the existing invalid-path error.
#    - dry_run_formatting() and execute_formatting() resolve and operate on the same set of
#      .py files for the same input paths.
#    - In execute (auto-fix) mode, format a single Python file via format_file().
#    - In execute (auto-fix) mode, format a mix of directory and file paths and report an error for invalid paths.
#    - In execute (auto-fix) mode, format a single directory by formatting each .py file under it.
#    - In execute (auto-fix) mode, format multiple directories by formatting each .py file under them.
#    - Detect an existing command path with find_command() when the command is present in PATH.
#    - Return None from find_command() when the command is not present in PATH.
#    - Verify check_command behavior via alternate patching for existing executable commands.
#    - Verify create_isolated_config() writes the formatter/linter configuration used by pyck.
#    - Verify format_imports() passes the isolated configuration to isort.
#    - Verify main() creates one isolated temporary configuration and passes it to dry-run and auto-fix processing.
#
#  Version History:
#  v1.4 2026-08-22
#       Cover auto-fix change detection, lint candidate reporting, and
#       unresolved lint diagnostics after auto-fix.
#       Remove duplicate check_command tests so strict error-message assertions run,
#       and cover isolated formatter and linter configuration.
#  v1.3 2026-07-15
#       Add test cases verifying that paths with spaces are quoted
#       before being passed to format_file and dry_run_formatting.
#  v1.2 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.1 2024-01-28
#       Added individual test cases for check_command function to verify
#       behavior with existing, nonexistent, and non-executable commands.
#  v1.0 2024-01-13
#       Initial test script for pyck.py
#
########################################################################

import os
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import MagicMock, call, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import pyck

CONFIG_PATH = '/tmp/pyck.cfg'
CONFIG_PATH_WITH_SPACES = '/tmp/pyck config/pyck.cfg'


def _popen_side_effect(rules):
    """ Build a subprocess.Popen side_effect returning canned results keyed by command substring. """
    def side_effect(command, shell=True, stdout=None):
        mock_process = MagicMock()
        for key, (returncode, out) in rules.items():
            if key in command:
                mock_process.communicate.return_value = (out, '')
                mock_process.returncode = returncode
                return mock_process
        mock_process.communicate.return_value = ('', '')
        mock_process.returncode = 0
        return mock_process
    return side_effect


class TestPyck(unittest.TestCase):
    """ Unit tests for the pyck.py script. """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'pyck.py')

        proc = subprocess.Popen(['python', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @patch('pyck.os.access')
    @patch('pyck.find_command')
    @patch('pyck.sys.exit')
    @patch('pyck.print')
    def test_check_command_with_existing_executable_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that exists and is executable. """
        mock_find_command.return_value = '/usr/bin/command'
        mock_access.return_value = True
        pyck.check_command('command')
        mock_exit.assert_not_called()
        mock_print.assert_not_called()

    @patch('pyck.os.access')
    @patch('pyck.find_command')
    @patch('pyck.sys.exit')
    @patch('pyck.print')
    def test_check_command_with_nonexistent_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that does not exist. """
        mock_find_command.return_value = None
        pyck.check_command('nonexistent')
        mock_print.assert_called_with("[ERROR] Command 'nonexistent' is not installed. Please install nonexistent and try again.", file=sys.stderr)
        mock_exit.assert_called_with(127)

    @patch('pyck.os.access')
    @patch('pyck.find_command')
    @patch('pyck.sys.exit')
    @patch('pyck.print')
    def test_check_command_with_nonexecutable_command(self, mock_print, mock_exit, mock_find_command, mock_access):
        """ Test check_command with a command that exists but is not executable. """
        mock_find_command.return_value = '/usr/bin/nonexecutable'
        mock_access.return_value = False
        pyck.check_command('nonexecutable')
        mock_print.assert_called_with("[ERROR] Command 'nonexecutable' is not executable. Please check the permissions.", file=sys.stderr)
        mock_exit.assert_called_with(126)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_format_file(self, mock_print, mock_popen):
        pyck.format_file(
            'path/to/file.py', 'E302,E402,E501', CONFIG_PATH)

        expected_calls = [
            call(
                "autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 -i path/to/file.py", shell=True),
            call().wait(),
            call("autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 -v -i path/to/file.py", shell=True),
            call().wait(),
            call("isort --settings-path=/tmp/pyck.cfg path/to/file.py", shell=True),
            call().wait()
        ]
        mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_format_file_quotes_path_with_spaces(self, mock_print, mock_popen):
        pyck.format_file(
            'path/to/my file.py',
            'E302,E402,E501',
            CONFIG_PATH_WITH_SPACES)

        expected_calls = [
            call(
                "autoflake --config='/tmp/pyck config/pyck.cfg' --imports=django,requests,urllib3 -i 'path/to/my file.py'", shell=True),
            call().wait(),
            call("autopep8 --global-config='/tmp/pyck config/pyck.cfg' --ignore-local-config --ignore=E302,E402,E501 -v -i 'path/to/my file.py'", shell=True),
            call().wait(),
            call("isort --settings-path='/tmp/pyck config/pyck.cfg' 'path/to/my file.py'", shell=True),
            call().wait()
        ]
        mock_popen.assert_has_calls(expected_calls, any_order=True)

    def test_create_isolated_config(self):
        with tempfile.TemporaryDirectory() as directory:
            config_path = pyck.create_isolated_config(directory)

            self.assertEqual(
                config_path,
                os.path.join(directory, 'pyck.cfg'))

            with open(config_path, 'r', encoding='utf-8') as f:
                self.assertEqual(
                    f.read(),
                    "[autoflake]\n"
                    "quiet = false\n\n"
                    "[pycodestyle]\n\n"
                    "[isort]\n"
                    "lines_between_sections = 1\n")

    @patch('pyck.subprocess.Popen')
    def test_format_imports_uses_isolated_config(self, mock_popen):
        pyck.format_imports('path/to/file.py', CONFIG_PATH)

        mock_popen.assert_has_calls([
            call(
                "isort --settings-path=/tmp/pyck.cfg path/to/file.py",
                shell=True),
            call().wait(),
        ])

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_run_command_success(self, mock_print, mock_popen):
        # Test scenario for successful command execution
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', '')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.run_command('echo test', show_files=None)
        mock_print.assert_not_called()

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_run_command_error(self, mock_print, mock_popen):
        # Test scenario for command execution with an error
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('error output', '')
        mock_process.returncode = 1
        mock_popen.return_value = mock_process

        pyck.run_command('echo test', show_files="Error occurred")
        mock_print.assert_called_with("Error occurred error output")

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_run_command_literal_message(self, mock_print, mock_popen):
        # literal_message=True prints show_files verbatim, ignoring command output
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('--- a\n+++ b\n', '')
        mock_process.returncode = 2
        mock_popen.return_value = mock_process

        pyck.run_command('autopep8 --diff --exit-code test.py',
                         show_files="Would format: test.py", literal_message=True)
        mock_print.assert_called_once_with("Would format: test.py")

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_single_file(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        # Testing dry-run for a single file
        pyck.dry_run_formatting(
            ['path/to/single_file.py'], 'E302,E402,E501', CONFIG_PATH)

        # Verify subprocess.Popen calls for the single file
        mock_popen.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/single_file.py", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 --check path/to/single_file.py", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 --diff --exit-code path/to/single_file.py", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "isort --settings-path=/tmp/pyck.cfg --check-only path/to/single_file.py", shell=True, stdout=-1)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_with_multiple_files(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.dry_run_formatting(
            ['path/to/file1.py', 'path/to/file2.py'], 'E302,E402,E501', CONFIG_PATH)

        expected_calls = [
            call("flake8 --isolated --ignore=E302,E402,E501 path/to/file1.py",
                 shell=True, stdout=-1),
            call("autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 --check path/to/file1.py",
                 shell=True, stdout=-1),
            call("autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 --diff --exit-code path/to/file1.py",
                 shell=True, stdout=-1),
            call("isort --settings-path=/tmp/pyck.cfg --check-only path/to/file1.py",
                 shell=True, stdout=-1),
            call("flake8 --isolated --ignore=E302,E402,E501 path/to/file2.py",
                 shell=True, stdout=-1),
            call("autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 --check path/to/file2.py",
                 shell=True, stdout=-1),
            call("autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 --diff --exit-code path/to/file2.py",
                 shell=True, stdout=-1),
            call("isort --settings-path=/tmp/pyck.cfg --check-only path/to/file2.py",
                 shell=True, stdout=-1)
        ]
        mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_quotes_path_with_spaces(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        # Testing dry-run for a path containing spaces
        pyck.dry_run_formatting(
            ['path/to/my file.py'], 'E302,E402,E501', CONFIG_PATH_WITH_SPACES)

        mock_popen.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 'path/to/my file.py'", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "autoflake --config='/tmp/pyck config/pyck.cfg' --imports=django,requests,urllib3 --check 'path/to/my file.py'", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "autopep8 --global-config='/tmp/pyck config/pyck.cfg' --ignore-local-config --ignore=E302,E402,E501 --diff --exit-code 'path/to/my file.py'", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "isort --settings-path='/tmp/pyck config/pyck.cfg' --check-only 'path/to/my file.py'", shell=True, stdout=-1)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    @patch('pyck.os.walk')
    def test_dry_run_formatting_single_directory(self, mock_walk, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = True
        mock_isfile.return_value = False
        mock_walk.return_value = [
            ('path/to/directory', [], ['file1.py', 'file2.py'])]
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.dry_run_formatting(
            ['path/to/directory'], 'E302,E402,E501', CONFIG_PATH)

        expected_calls = []
        for name in ('file1.py', 'file2.py'):
            target = os.path.join('path/to/directory', name)
            expected_calls.extend([
                call("flake8 --isolated --ignore=E302,E402,E501 {}".format(target),
                     shell=True, stdout=-1),
                call("autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 --check {}".format(target),
                     shell=True, stdout=-1),
                call("autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 --diff --exit-code {}".format(target),
                     shell=True, stdout=-1),
                call("isort --settings-path=/tmp/pyck.cfg --check-only {}".format(target),
                     shell=True, stdout=-1),
            ])
        mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    @patch('pyck.os.walk')
    def test_dry_run_formatting_multiple_directories(self, mock_walk, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = True
        mock_isfile.return_value = False
        mock_walk.side_effect = [
            [('path/to/dir1', [], ['file1.py', 'file2.py'])],
            [('path/to/dir2', [], ['file3.py', 'file4.py'])]
        ]
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('output', 'error')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.dry_run_formatting(
            ['path/to/dir1', 'path/to/dir2'], 'E302,E402,E501', CONFIG_PATH)

        expected_calls = []
        for root, name in (('path/to/dir1', 'file1.py'), ('path/to/dir1', 'file2.py'),
                           ('path/to/dir2', 'file3.py'), ('path/to/dir2', 'file4.py')):
            target = os.path.join(root, name)
            expected_calls.extend([
                call("flake8 --isolated --ignore=E302,E402,E501 {}".format(target),
                     shell=True, stdout=-1),
                call("autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 --check {}".format(target),
                     shell=True, stdout=-1),
                call("autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 --diff --exit-code {}".format(target),
                     shell=True, stdout=-1),
                call("isort --settings-path=/tmp/pyck.cfg --check-only {}".format(target),
                     shell=True, stdout=-1),
            ])
        mock_popen.assert_has_calls(expected_calls, any_order=True)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_flake8_only_does_not_report_would_format(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        # Regression: a flake8-only lint issue must never surface as "Would format:".
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_popen.side_effect = _popen_side_effect({
            'flake8': (1, "path/to/file.py:1:1: F401 'os' imported but unused"),
            'autoflake': (0, ''),
            'autopep8': (0, ''),
            'isort': (0, ''),
        })

        pyck.dry_run_formatting(
            ['path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        would_format_calls = [c for c in mock_print.call_args_list
                              if 'Would format:' in str(c)]
        self.assertEqual(would_format_calls, [])
        mock_print.assert_any_call(
            "Lint issue (manual review candidate): "
            "path/to/file.py:1:1: F401 'os' imported but unused")

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_autopep8_only_reports_would_format(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        # Regression: autopep8 alone reporting a diff must produce "Would format:".
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_popen.side_effect = _popen_side_effect({
            'flake8': (0, ''),
            'autoflake': (0, ''),
            'autopep8': (2, '--- original\n+++ fixed\n'),
            'isort': (0, ''),
        })

        pyck.dry_run_formatting(
            ['path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        mock_print.assert_any_call("Would format: path/to/file.py")

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_reports_would_clean_only_when_autoflake_changes(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_popen.side_effect = _popen_side_effect({
            'flake8': (0, ''),
            'autoflake': (1, ''),
            'autopep8': (0, ''),
            'isort': (0, ''),
        })

        pyck.dry_run_formatting(
            ['path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        mock_print.assert_any_call("Would clean: path/to/file.py")

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_reports_would_sort_only_when_isort_changes(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_popen.side_effect = _popen_side_effect({
            'flake8': (0, ''),
            'autoflake': (0, ''),
            'autopep8': (0, ''),
            'isort': (1, ''),
        })

        pyck.dry_run_formatting(
            ['path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        mock_print.assert_any_call("Would sort imports in: path/to/file.py")

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_no_change_reports_nothing(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('', '')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.dry_run_formatting(
            ['path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        for label in ('Would clean:', 'Would format:', 'Would sort imports in:'):
            matching_calls = [c for c in mock_print.call_args_list if label in str(c)]
            self.assertEqual(matching_calls, [])

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_dry_run_formatting_never_uses_write_mode_commands(self, mock_isfile, mock_isdir, mock_print, mock_popen):
        # Safety: dry-run must only invoke check/diff commands, never the mutating ones.
        mock_isdir.return_value = False
        mock_isfile.return_value = True
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('', '')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.dry_run_formatting(
            ['path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        for command_call in mock_popen.call_args_list:
            command = command_call.args[0]
            self.assertNotIn('autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 -i', command)
            self.assertNotIn('autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 -v -i', command)
            self.assertNotRegex(command, r'^isort --settings-path=/tmp/pyck\.cfg (?!--check-only)')
        mock_popen.assert_any_call(
            "autoflake --config=/tmp/pyck.cfg --imports=django,requests,urllib3 --check path/to/file.py", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "autopep8 --global-config=/tmp/pyck.cfg --ignore-local-config --ignore=E302,E402,E501 --diff --exit-code path/to/file.py", shell=True, stdout=-1)
        mock_popen.assert_any_call(
            "isort --settings-path=/tmp/pyck.cfg --check-only path/to/file.py", shell=True, stdout=-1)

    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_resolve_target_files_single_file(self, mock_isfile, mock_isdir):
        mock_isdir.return_value = False
        mock_isfile.return_value = True

        result = pyck.resolve_target_files(['path/to/file.py'])

        self.assertEqual(result, ['path/to/file.py'])

    @patch('pyck.os.walk')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_resolve_target_files_directory(self, mock_isfile, mock_isdir, mock_walk):
        mock_isdir.return_value = True
        mock_isfile.return_value = False
        mock_walk.return_value = [
            ('path/to/directory', [], ['file1.py', 'file2.py', 'notes.txt'])]

        result = pyck.resolve_target_files(['path/to/directory'])

        self.assertEqual(result, [
            os.path.join('path/to/directory', 'file1.py'),
            os.path.join('path/to/directory', 'file2.py'),
        ])

    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    def test_resolve_target_files_invalid_path(self, mock_isfile, mock_isdir, mock_print):
        mock_isdir.return_value = False
        mock_isfile.return_value = False

        result = pyck.resolve_target_files(['invalid/path'])

        self.assertEqual(result, [])
        mock_print.assert_called_with(
            "[ERROR] The specified path 'invalid/path' is neither a file nor a directory.", file=sys.stderr)

    @patch('pyck.subprocess.Popen')
    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path.isdir')
    @patch('pyck.os.path.isfile')
    @patch('pyck.os.walk')
    def test_dry_run_and_execute_formatting_target_same_files(self, mock_walk, mock_isfile, mock_isdir, mock_print, mock_format_file, mock_popen):
        # dry-run and auto-fix must resolve and act on the exact same set of .py files.
        mock_isdir.return_value = True
        mock_isfile.return_value = False
        mock_walk.return_value = [
            ('path/to/directory', [], ['file1.py', 'file2.py'])]
        mock_process = MagicMock()
        mock_process.communicate.return_value = ('', '')
        mock_process.returncode = 0
        mock_popen.return_value = mock_process

        pyck.execute_formatting(
            ['path/to/directory'], 'E302,E402,E501', CONFIG_PATH)
        auto_fix_files = sorted(
            c.args[0] for c in mock_format_file.call_args_list)

        mock_popen.reset_mock()

        pyck.dry_run_formatting(
            ['path/to/directory'], 'E302,E402,E501', CONFIG_PATH)
        dry_run_files = sorted(set(
            c.args[0].split()[-1] for c in mock_popen.call_args_list))

        expected_files = sorted([
            os.path.join('path/to/directory', 'file1.py'),
            os.path.join('path/to/directory', 'file2.py'),
        ])
        self.assertEqual(auto_fix_files, expected_files)
        self.assertEqual(dry_run_files, expected_files)

    @patch('pyck.run_command')
    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_single_file(
            self, mock_walk, mock_path, mock_print,
            mock_format_file, mock_run_command):
        # Mocking file existence
        mock_path.isfile.return_value = True
        mock_path.isdir.return_value = False

        # Testing path for a single file
        pyck.execute_formatting(
            ['path/to/single_file.py'], 'E302,E402,E501', CONFIG_PATH)

        # Verify format_file call for the single file
        mock_format_file.assert_called_once_with(
            'path/to/single_file.py', 'E302,E402,E501', CONFIG_PATH)

        mock_run_command.assert_called_once_with(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/single_file.py",
            show_files="Manual fix required:")

    @patch('pyck.run_command')
    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_with_multiple_files(
            self, mock_walk, mock_path, mock_print,
            mock_format_file, mock_run_command):
        # Mocking file and directory existence
        mock_path.isfile.side_effect = lambda p: p == 'path/to/file.py'
        mock_path.isdir.side_effect = lambda p: p == 'path/to/directory'
        mock_path.join.side_effect = lambda *parts: '/'.join(parts)
        mock_walk.return_value = [
            ('path/to/directory', [], ['file1.py', 'file2.py'])]

        # Testing paths for a directory and a file
        pyck.execute_formatting(
            ['path/to/directory', 'path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        # Verify format_file calls for files in the directory
        expected_file1_path = os.path.join('path/to/directory', 'file1.py')
        expected_file2_path = os.path.join('path/to/directory', 'file2.py')
        mock_format_file.assert_any_call(expected_file1_path, 'E302,E402,E501', CONFIG_PATH)
        mock_format_file.assert_any_call(expected_file2_path, 'E302,E402,E501', CONFIG_PATH)

        # Verify format_file call for a single file
        mock_format_file.assert_any_call('path/to/file.py', 'E302,E402,E501', CONFIG_PATH)

        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/directory/file1.py",
            show_files="Manual fix required:")
        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/directory/file2.py",
            show_files="Manual fix required:")
        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/file.py",
            show_files="Manual fix required:")

        # Test behavior when path is neither a file nor a directory
        pyck.execute_formatting(['invalid/path'], 'E302,E402,E501', CONFIG_PATH)
        mock_print.assert_called_with(
            "[ERROR] The specified path 'invalid/path' is neither a file nor a directory.", file=sys.stderr)

    @patch('pyck.run_command')
    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_single_directory(
            self, mock_walk, mock_path, mock_print,
            mock_format_file, mock_run_command):
        # Mocking directory existence
        mock_path.isfile.return_value = False
        mock_path.isdir.return_value = True
        mock_path.join.side_effect = lambda *parts: '/'.join(parts)
        mock_walk.return_value = [
            ('path/to/directory', [], ['file1.py', 'file2.py'])]

        # Testing path for a directory
        pyck.execute_formatting(
            ['path/to/directory'], 'E302,E402,E501', CONFIG_PATH)

        # Verify format_file calls for files in the directory
        expected_file1_path = os.path.join('path/to/directory', 'file1.py')
        expected_file2_path = os.path.join('path/to/directory', 'file2.py')
        mock_format_file.assert_any_call(expected_file1_path, 'E302,E402,E501', CONFIG_PATH)
        mock_format_file.assert_any_call(expected_file2_path, 'E302,E402,E501', CONFIG_PATH)

        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/directory/file1.py",
            show_files="Manual fix required:")
        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/directory/file2.py",
            show_files="Manual fix required:")

    @patch('pyck.run_command')
    @patch('pyck.format_file')
    @patch('pyck.print')
    @patch('pyck.os.path')
    @patch('pyck.os.walk')
    def test_execute_formatting_multiple_directories(
            self, mock_walk, mock_path, mock_print,
            mock_format_file, mock_run_command):
        # Mocking multiple directories
        mock_path.isfile.return_value = False
        mock_path.isdir.side_effect = lambda p: p in [
            'path/to/dir1', 'path/to/dir2']
        mock_path.join.side_effect = lambda *parts: '/'.join(parts)
        mock_walk.side_effect = [
            [('path/to/dir1', [], ['file1.py', 'file2.py'])],
            [('path/to/dir2', [], ['file3.py', 'file4.py'])]
        ]

        # Testing paths for multiple directories
        pyck.execute_formatting(
            ['path/to/dir1', 'path/to/dir2'], 'E302,E402,E501', CONFIG_PATH)

        # Verify format_file calls for files in each directory
        expected_calls = [
            os.path.join('path/to/dir1', 'file1.py'),
            os.path.join('path/to/dir1', 'file2.py'),
            os.path.join('path/to/dir2', 'file3.py'),
            os.path.join('path/to/dir2', 'file4.py')
        ]
        for file_path in expected_calls:
            mock_format_file.assert_any_call(file_path, 'E302,E402,E501', CONFIG_PATH)

        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/dir1/file1.py",
            show_files="Manual fix required:")
        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/dir1/file2.py",
            show_files="Manual fix required:")
        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/dir2/file3.py",
            show_files="Manual fix required:")
        mock_run_command.assert_any_call(
            "flake8 --isolated --ignore=E302,E402,E501 path/to/dir2/file4.py",
            show_files="Manual fix required:")

    @patch('pyck.run_command')
    @patch('pyck.format_file')
    @patch('pyck.resolve_target_files')
    def test_execute_formatting_runs_flake8_after_format_file(
            self, mock_resolve_target_files,
            mock_format_file, mock_run_command):
        mock_resolve_target_files.return_value = ['path/to/file.py']

        calls = MagicMock()
        calls.attach_mock(mock_format_file, 'format_file')
        calls.attach_mock(mock_run_command, 'run_command')

        pyck.execute_formatting(
            ['path/to/file.py'], 'E302,E402,E501', CONFIG_PATH)

        self.assertEqual(calls.mock_calls, [
            call.format_file(
                'path/to/file.py', 'E302,E402,E501', CONFIG_PATH),
            call.run_command(
                'flake8 --isolated --ignore=E302,E402,E501 path/to/file.py',
                show_files='Manual fix required:'),
        ])

    @patch('pyck.subprocess.Popen')
    @patch('pyck.print')
    def test_run_command_reports_manual_fix_required(
            self, mock_print, mock_popen):
        mock_process = MagicMock()
        mock_process.communicate.return_value = (
            "path/to/file.py:10:5: F811 redefinition of unused 'test_case'",
            '')
        mock_process.returncode = 1
        mock_popen.return_value = mock_process

        pyck.run_command(
            'flake8 --ignore=E302,E402,E501 path/to/file.py',
            show_files='Manual fix required:')

        mock_print.assert_called_once_with(
            "Manual fix required: "
            "path/to/file.py:10:5: F811 "
            "redefinition of unused 'test_case'")

    @patch('pyck.run_command')
    @patch('pyck.format_file')
    @patch('pyck.resolve_target_files')
    def test_execute_formatting_quotes_path_for_post_fix_flake8(
            self, mock_resolve_target_files,
            mock_format_file, mock_run_command):
        mock_resolve_target_files.return_value = [
            'path/to/my file.py']

        pyck.execute_formatting(
            ['path/to/my file.py'], 'E302,E402,E501', CONFIG_PATH_WITH_SPACES)

        mock_run_command.assert_called_once_with(
            "flake8 --isolated --ignore=E302,E402,E501 "
            "'path/to/my file.py'",
            show_files='Manual fix required:')

    @patch('pyck.create_isolated_config')
    @patch('pyck.tempfile.TemporaryDirectory')
    @patch('pyck.print')
    @patch('pyck.subprocess.Popen')
    @patch('pyck.format_file')
    @patch('pyck.resolve_target_files')
    @patch('pyck.check_command')
    @patch('pyck.setup_argument_parser')
    def test_main_returns_zero_when_lint_remains_after_auto_fix(
            self, mock_setup_argument_parser, mock_check_command,
            mock_resolve_target_files, mock_format_file,
            mock_popen, mock_print, mock_temporary_directory,
            mock_create_isolated_config):
        mock_parser = MagicMock()
        mock_args = MagicMock()
        mock_args.paths = ['path/to/file.py']
        mock_args.auto_fix = True
        mock_parser.parse_args.return_value = mock_args
        mock_setup_argument_parser.return_value = mock_parser

        mock_resolve_target_files.return_value = ['path/to/file.py']

        mock_temporary_directory.return_value.__enter__.return_value = '/tmp/pyck-test'
        mock_create_isolated_config.return_value = CONFIG_PATH

        mock_process = MagicMock()
        mock_process.communicate.return_value = (
            "path/to/file.py:10:5: F811 redefinition of unused 'test_case'",
            '')
        mock_process.returncode = 1
        mock_popen.return_value = mock_process

        result = pyck.main()

        self.assertEqual(result, 0)
        mock_print.assert_any_call(
            "Manual fix required: "
            "path/to/file.py:10:5: F811 "
            "redefinition of unused 'test_case'")

        mock_check_command.assert_has_calls([
            call('autopep8'),
            call('flake8'),
            call('autoflake'),
            call('isort'),
        ])

        mock_temporary_directory.assert_called_once_with()
        mock_create_isolated_config.assert_called_once_with(
            '/tmp/pyck-test')
        mock_format_file.assert_called_once_with(
            'path/to/file.py',
            'E302,E402,E501',
            CONFIG_PATH)

    @patch('pyck.dry_run_formatting')
    @patch('pyck.create_isolated_config')
    @patch('pyck.tempfile.TemporaryDirectory')
    @patch('pyck.check_command')
    @patch('pyck.setup_argument_parser')
    def test_main_passes_isolated_config_to_dry_run(
            self, mock_setup_argument_parser, mock_check_command,
            mock_temporary_directory, mock_create_isolated_config,
            mock_dry_run_formatting):
        mock_parser = MagicMock()
        mock_args = MagicMock()
        mock_args.paths = ['path/to/file.py']
        mock_args.auto_fix = False
        mock_parser.parse_args.return_value = mock_args
        mock_setup_argument_parser.return_value = mock_parser

        mock_temporary_directory.return_value.__enter__.return_value = '/tmp/pyck-test'
        mock_create_isolated_config.return_value = CONFIG_PATH

        result = pyck.main()

        self.assertEqual(result, 0)
        mock_create_isolated_config.assert_called_once_with(
            '/tmp/pyck-test')
        mock_dry_run_formatting.assert_called_once_with(
            ['path/to/file.py'],
            'E302,E402,E501',
            CONFIG_PATH)

    @patch('pyck.os.path.isfile')
    @patch.dict('pyck.os.environ', {'PATH': '/usr/bin:/bin'})
    def test_find_command_with_existing_command(self, mock_isfile):
        # Test the case where the command exists
        mock_isfile.return_value = True
        result = pyck.find_command('python')
        self.assertTrue(result.endswith('/python'))

    @patch('pyck.os.path.isfile')
    @patch.dict('pyck.os.environ', {'PATH': '/usr/bin:/bin'})
    def test_find_command_with_nonexistent_command(self, mock_isfile):
        # Test the case where the command does not exist
        mock_isfile.return_value = False
        result = pyck.find_command('nonexistent')
        self.assertIsNone(result)

    @patch('pyck.sys.exit')
    @patch('pyck.print')
    @patch('pyck.find_command')
    @patch('pyck.os.access')
    def test_check_command_with_existing_executable(self, mock_access, mock_find_command, mock_print, mock_exit):
        # Test with an existing and executable command
        mock_find_command.return_value = '/usr/bin/python'
        mock_access.return_value = True
        pyck.check_command('python')
        mock_exit.assert_not_called()
        mock_print.assert_not_called()


if __name__ == '__main__':
    unittest.main()
