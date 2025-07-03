#!/usr/bin/env python

########################################################################
# list_file_counts_test.py: Unit tests for list_file_counts.py
#
#  Description:
#  This script provides unit tests for the list_file_counts.py script.
#  It validates the functionality of various modules including file
#  counting, subdirectory listing, sorting, and error handling. The
#  tests are designed to ensure robust performance and correctness.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-05
#       Initial release.
#
########################################################################

import os
import subprocess
import sys
import unittest

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from list_file_counts import (count_files_in_directory,
                              count_files_in_subdirectories,
                              get_subdirectories, sort_file_counts)


class TestListFileCounts(unittest.TestCase):
    """ Unit tests for list_file_counts.py. """

    @classmethod
    def setUpClass(cls):
        """ Set up a temporary test environment. """
        cls.test_dir = "test_env"
        os.mkdir(cls.test_dir)

        # Create subdirectories and files
        os.mkdir(os.path.join(cls.test_dir, "subdir1"))
        os.mkdir(os.path.join(cls.test_dir, "subdir2"))
        os.mkdir(os.path.join(cls.test_dir, "subdir_empty"))

        for i in range(5):
            with open(os.path.join(cls.test_dir, "subdir1", "file{}.txt".format(i)), "w") as f:
                f.write("Test content")

        for i in range(3):
            with open(os.path.join(cls.test_dir, "subdir2", "file{}.txt".format(i)), "w") as f:
                f.write("Test content")

    @classmethod
    def tearDownClass(cls):
        """ Clean up the temporary test environment. """
        for root, dirs, files in os.walk(cls.test_dir, topdown=False):
            for file in files:
                os.remove(os.path.join(root, file))
            for dir in dirs:
                os.rmdir(os.path.join(root, dir))
        os.rmdir(cls.test_dir)

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'list_file_counts.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_get_subdirectories(self):
        """ Test retrieving subdirectories. """
        subdirs = get_subdirectories(self.test_dir)
        self.assertCountEqual(subdirs, ["subdir1", "subdir2", "subdir_empty"])

    def test_count_files_in_directory(self):
        """ Test counting files in a single directory. """
        count = count_files_in_directory(os.path.join(self.test_dir, "subdir1"))
        self.assertEqual(count, 5)

        count = count_files_in_directory(os.path.join(self.test_dir, "subdir2"))
        self.assertEqual(count, 3)

        count = count_files_in_directory(os.path.join(self.test_dir, "subdir_empty"))
        self.assertEqual(count, 0)

    def test_count_files_in_subdirectories(self):
        """ Test counting files in all subdirectories. """
        counts = count_files_in_subdirectories(self.test_dir)
        self.assertEqual(counts, {
            "subdir1": 5,
            "subdir2": 3,
            "subdir_empty": 0
        })

    def test_sort_file_counts(self):
        """ Test sorting file counts. """
        counts = {
            "subdir1": 5,
            "subdir2": 3,
            "subdir_empty": 0
        }
        sorted_counts = sort_file_counts(counts)
        self.assertEqual(sorted_counts, [
            ("subdir1", 5),
            ("subdir2", 3),
            ("subdir_empty", 0)
        ])

    def test_invalid_directory_get_subdirectories(self):
        """ Test handling invalid directory in get_subdirectories. """
        with self.assertRaises(ValueError) as context:
            get_subdirectories("invalid_dir")
        self.assertIn("Error accessing directory", str(context.exception))

    def test_invalid_directory_count_files_in_directory(self):
        """ Test handling invalid directory in count_files_in_directory. """
        with self.assertRaises(ValueError) as context:
            count_files_in_directory("invalid_dir")
        self.assertIn("Error accessing directory", str(context.exception))

    def test_invalid_directory_count_files_in_subdirectories(self):
        """ Test handling invalid directory in count_files_in_subdirectories. """
        with self.assertRaises(ValueError) as context:
            count_files_in_subdirectories("invalid_dir")
        self.assertIn("Error accessing directory", str(context.exception))


if __name__ == "__main__":
    unittest.main()
