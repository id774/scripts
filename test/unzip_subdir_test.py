#!/usr/bin/env python3

########################################################################
# unzip_subdir_test.py: Test for unzip_subdir.py
#
#  Description:
#  This script tests unzip_subdir.py, which extracts each .zip file in a
#  given directory into a separate subdirectory. It verifies dry-run behavior,
#  skipping existing directories, and proper help message output.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-07-08
#       Fixed compatibility issues with Python 3.4.
#  v1.0 2025-07-07
#       Initial release.
#
#  Test Cases:
#  - Shows usage and exits with code 0 when invoked with -h option
#  - Displays expected output in dry-run mode without creating directories
#  - Skips extraction if target directory already exists
#
########################################################################

import io
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import unzip_subdir


class TestUnzipSubdir(unittest.TestCase):
    def test_help_option(self):
        script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../unzip_subdir.py'))
        from subprocess import PIPE, Popen
        process = Popen(['python3', script_path, '-h'], stdout=PIPE, stderr=PIPE)
        stdout, _ = process.communicate()
        self.assertEqual(process.returncode, 0)
        self.assertIn("Usage", stdout.decode('utf-8'))

    def test_dry_run_lists_zip_files(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            zip_path = os.path.join(tmpdir, 'testfile.zip')
            with open(zip_path, 'w') as f:
                f.write('dummy content')

            f = io.StringIO()
            with redirect_stdout(f):
                unzip_subdir.unzip_files([tmpdir], dry_run=True)
            output = f.getvalue()
            self.assertIn("DRY RUN", output)
            self.assertIn("testfile.zip", output)

    def test_skips_existing_directory(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            zip_path = os.path.join(tmpdir, 'data.zip')
            extract_dir = os.path.join(tmpdir, 'data')
            os.mkdir(extract_dir)
            with open(zip_path, 'w') as f:
                f.write('dummy zip')

            f = io.StringIO()
            with redirect_stdout(f):
                unzip_subdir.unzip_files([tmpdir], dry_run=True)
            output = f.getvalue()
            # Should not list it because dir already exists
            self.assertNotIn("data.zip", output)

    @patch('os.system')
    def test_actual_unzip_command_called(self, mock_system):
        with tempfile.TemporaryDirectory() as tmpdir:
            zip_path = os.path.join(tmpdir, 'foo.zip')
            with open(zip_path, 'w') as f:
                f.write('dummy zip')

            # no target dir should exist
            f = io.StringIO()
            with redirect_stdout(f):
                unzip_subdir.unzip_files([tmpdir], dry_run=False)
            output = f.getvalue()

            # Ensure directory was created
            self.assertTrue(os.path.isdir(os.path.join(tmpdir, 'foo')))
            # Replace assert_called_once (Python 3.6+ only)
            self.assertEqual(mock_system.call_count, 1)
            self.assertIn('foo', os.listdir(tmpdir))


if __name__ == '__main__':
    unittest.main()
