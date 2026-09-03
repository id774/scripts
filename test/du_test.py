#!/usr/bin/env python

########################################################################
# du_test.py: Unit tests for du.py
#
#  Description:
#  This test suite validates the disk usage reporting functionality of du.py,
#  including directory checks, output parsing, and hidden directory filtering.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Accept an existing directory without exiting (valid directory).
#    - Exit with code 1 when the target path does not exist.
#    - Exit with code 1 when the target path exists but is not a directory.
#    - Parse du output and return the size for the requested directory path.
#    - run_custom_du includes hidden directories when include_hidden is True.
#    - run_custom_du excludes hidden directories when include_hidden is False.
#    - locate_command() resolves an executable command found on PATH.
#    - locate_command() returns None when the command is not found on PATH.
#    - locate_command() resolves an empty PATH component to the current directory.
#    - command_exists() is consistent with locate_command().
#
#  Version History:
#  v1.1 2026-09-03
#       Add tests for the manual PATH search in locate_command(), replacing
#       the previous 'command -v' based lookup. These run on any platform,
#       unlike the macOS-only disk usage tests above.
#  v1.0 2025-06-24
#      Initial release.
#
########################################################################

import os
import platform
import shutil
import subprocess
import sys
import tempfile
import unittest
from io import StringIO
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from du import check_directory, command_exists, locate_command, parse_du_output, run_custom_du


class TestDuScript(unittest.TestCase):
    def setUp(self):
        if platform.system() != 'Darwin':
            self.skipTest("du.py is intended for macOS only")
        self.test_dir = tempfile.mkdtemp()
        self.hidden_dir = os.path.join(self.test_dir, '.hidden')
        self.visible_dir = os.path.join(self.test_dir, 'visible')
        os.mkdir(self.hidden_dir)
        os.mkdir(self.visible_dir)

    def tearDown(self):
        subprocess.call(['rm', '-rf', self.test_dir])

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'du.py')

        proc = subprocess.Popen(['python', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_check_directory_valid(self):
        check_directory(self.test_dir)  # Should not raise

    def test_check_directory_nonexistent(self):
        stderr = sys.stderr
        sys.stderr = StringIO()
        try:
            with self.assertRaises(SystemExit) as cm:
                check_directory('/nonexistent/path')
            self.assertEqual(cm.exception.code, 1)
        finally:
            sys.stderr = stderr

    def test_check_directory_not_directory(self):
        dummy_file = os.path.join(self.test_dir, 'file.txt')
        with open(dummy_file, 'w') as f:
            f.write("data")
        stderr = sys.stderr
        sys.stderr = StringIO()
        try:
            with self.assertRaises(SystemExit) as cm:
                check_directory(dummy_file)
            self.assertEqual(cm.exception.code, 1)
        finally:
            sys.stderr = stderr

    def test_parse_du_output(self):
        fake_output = "{}\t{}\n{}\t{}".format("4.0K", self.test_dir, "8.0K", os.path.join(self.test_dir, "visible"))
        size = parse_du_output(fake_output, self.test_dir)
        self.assertEqual(size, "4.0K")

    def test_run_custom_du_includes_hidden(self):
        with patch('builtins.print') as mock_print:
            run_custom_du("1", self.test_dir, include_hidden=True)
            output = "".join(call.args[0] for call in mock_print.call_args_list)
            self.assertIn('.hidden', output)
            self.assertIn('visible', output)

    def test_run_custom_du_excludes_hidden(self):
        with patch('builtins.print') as mock_print:
            run_custom_du("1", self.test_dir, include_hidden=False)
            output = "".join(call.args[0] for call in mock_print.call_args_list)
            self.assertNotIn('.hidden', output)
            self.assertIn('visible', output)


class TestDuCommandLookup(unittest.TestCase):
    """ Tests for the manual PATH search; these are not macOS-specific. """

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

    def test_locate_command_resolves_executable_on_path(self):
        directory = self.make_fake_path(['fake_du'], [])
        with patch.dict(os.environ, {'PATH': directory}):
            found = locate_command('fake_du')
            self.assertTrue(command_exists('fake_du'))
        self.assertEqual(os.path.realpath(found),
                         os.path.realpath(os.path.join(directory, 'fake_du')))

    def test_locate_command_missing_returns_none(self):
        directory = self.make_fake_path([], [])
        with patch.dict(os.environ, {'PATH': directory}):
            self.assertIsNone(locate_command('nonexistent_command'))
            self.assertFalse(command_exists('nonexistent_command'))

    def test_locate_command_empty_path_component_is_cwd(self):
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
            found = locate_command(command_name)
        self.assertEqual(os.path.realpath(found), os.path.realpath(path))


if __name__ == '__main__':
    unittest.main()
