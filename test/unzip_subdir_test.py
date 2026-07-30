#!/usr/bin/env python

########################################################################
# unzip_subdir_test.py: Test for unzip_subdir.py
#
#  Description:
#  This script tests unzip_subdir.py, which extracts each .zip file in a
#  given directory into a separate subdirectory. It verifies dry-run behavior,
#  skipping existing directories, safe zipfile extraction, nested directory
#  traversal, unsafe archive member rejection, and proper help message output.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - In dry-run mode, lists .zip files found in the target directory without creating directories or executing unzip.
#    - Skips extraction (and listing in dry-run) when the target subdirectory already exists.
#    - In execution mode (non–dry-run), creates a subdirectory per .zip file and extracts archive contents with zipfile.
#    - Rejects archive entries that would be written outside the target directory.
#    - Extracts .zip files discovered in nested subdirectories using the discovered archive path.
#    - Continues after a failed archive and reports the failure count.
#    - Exits with status 1 from the CLI when an extraction fails.
#
#  Version History:
#  v1.4 2026-07-26
#       Add coverage for failure counting and non-zero CLI exit status,
#       and switch the help test to sys.executable.
#  v1.3 2026-07-23
#       Add coverage for cleanup after extraction failures.
#  v1.2 2026-07-14
#       Document and test zipfile extraction, nested paths, and unsafe member rejection.
#  v1.1 2025-07-08
#       Fixed compatibility issues with Python 3.4.
#  v1.0 2025-07-07
#       Initial release.
#
########################################################################

import io
import os
import sys
import tempfile
import unittest
import zipfile
from contextlib import redirect_stderr, redirect_stdout
from unittest import mock

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import unzip_subdir


class TestUnzipSubdir(unittest.TestCase):
    def test_help_option(self):
        script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../unzip_subdir.py'))
        from subprocess import PIPE, Popen
        process = Popen([sys.executable, script_path, '-h'], stdout=PIPE, stderr=PIPE)
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

    def test_actual_unzip_extracts_archive_contents(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            zip_path = os.path.join(tmpdir, 'foo.zip')
            with zipfile.ZipFile(zip_path, 'w') as archive:
                archive.writestr('bar.txt', 'extracted content')

            f = io.StringIO()
            with redirect_stdout(f):
                unzip_subdir.unzip_files([tmpdir], dry_run=False)

            extracted_path = os.path.join(tmpdir, 'foo', 'bar.txt')
            self.assertTrue(os.path.isdir(os.path.join(tmpdir, 'foo')))
            self.assertTrue(os.path.isfile(extracted_path))
            with open(extracted_path) as extracted:
                self.assertEqual(extracted.read(), 'extracted content')

    def test_rejects_zip_entries_outside_target_directory(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            zip_path = os.path.join(tmpdir, 'evil.zip')
            with zipfile.ZipFile(zip_path, 'w') as archive:
                archive.writestr('../outside.txt', 'should not escape')

            with redirect_stderr(io.StringIO()):
                unzip_subdir.unzip_files([tmpdir], dry_run=False)

            self.assertFalse(os.path.exists(os.path.join(tmpdir, 'outside.txt')))
            self.assertFalse(os.path.exists(os.path.join(tmpdir, 'evil')))

    def test_removes_target_directory_when_extraction_fails(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            zip_path = os.path.join(tmpdir, 'broken.zip')
            with zipfile.ZipFile(zip_path, 'w') as archive:
                archive.writestr('partial.txt', 'partial content')

            with mock.patch.object(zipfile.ZipFile, 'extractall',
                                   side_effect=OSError('extraction failed')):
                with redirect_stderr(io.StringIO()):
                    unzip_subdir.unzip_files([tmpdir], dry_run=False)

            self.assertFalse(os.path.exists(os.path.join(tmpdir, 'broken')))

    def test_mixed_archives_extract_valid_and_report_failure(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            good_zip = os.path.join(tmpdir, 'good.zip')
            with zipfile.ZipFile(good_zip, 'w') as archive:
                archive.writestr('good.txt', 'good content')

            broken_zip = os.path.join(tmpdir, 'broken.zip')
            with open(broken_zip, 'w') as f:
                f.write('not a zip archive')

            with redirect_stderr(io.StringIO()):
                failures = unzip_subdir.unzip_files([tmpdir], dry_run=False)

            self.assertEqual(failures, 1)
            self.assertTrue(os.path.isfile(os.path.join(tmpdir, 'good', 'good.txt')))
            self.assertFalse(os.path.exists(os.path.join(tmpdir, 'broken')))

    def test_cli_exits_non_zero_when_extraction_fails(self):
        script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../unzip_subdir.py'))
        from subprocess import PIPE, Popen
        with tempfile.TemporaryDirectory() as tmpdir:
            with open(os.path.join(tmpdir, 'broken.zip'), 'w') as f:
                f.write('not a zip archive')

            process = Popen([sys.executable, script_path, tmpdir], stdout=PIPE, stderr=PIPE)
            _, stderr = process.communicate()

            self.assertEqual(process.returncode, 1)
            self.assertIn('broken.zip', stderr.decode('utf-8'))
            self.assertFalse(os.path.exists(os.path.join(tmpdir, 'broken')))

    def test_cli_exits_zero_when_all_archives_extract(self):
        script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../unzip_subdir.py'))
        from subprocess import PIPE, Popen
        with tempfile.TemporaryDirectory() as tmpdir:
            with zipfile.ZipFile(os.path.join(tmpdir, 'good.zip'), 'w') as archive:
                archive.writestr('good.txt', 'good content')

            process = Popen([sys.executable, script_path, tmpdir], stdout=PIPE, stderr=PIPE)
            process.communicate()

            self.assertEqual(process.returncode, 0)
            self.assertTrue(os.path.isfile(os.path.join(tmpdir, 'good', 'good.txt')))

    def test_extracts_zip_files_from_nested_directories(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            nested_dir = os.path.join(tmpdir, 'nested')
            os.mkdir(nested_dir)
            zip_path = os.path.join(nested_dir, 'child.zip')
            with zipfile.ZipFile(zip_path, 'w') as archive:
                archive.writestr('child.txt', 'nested content')

            unzip_subdir.unzip_files([tmpdir], dry_run=False)

            extracted_path = os.path.join(nested_dir, 'child', 'child.txt')
            self.assertTrue(os.path.isfile(extracted_path))
            with open(extracted_path) as extracted:
                self.assertEqual(extracted.read(), 'nested content')


if __name__ == '__main__':
    unittest.main()
