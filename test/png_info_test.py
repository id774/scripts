#!/usr/bin/env python

########################################################################
# png_info_test.py: Test script for png_info.py
#
#  Description:
#  This test script contains unit tests for the png_info.py script.
#  It verifies the functionality of reading basic information from PNG files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Read PNG IHDR metadata from a valid PNG signature and return (width, height, bit depth, color format).
#    - Raise ValueError when the input is not a valid PNG file (invalid signature / data).
#    - Raise struct.error when the PNG header/IHDR data is incomplete and cannot be unpacked.
#    - Process every file matched across multiple command-line arguments, not just the first one.
#    - Continue past a failing file, report it on stderr, and exit with status 1.
#
#  Version History:
#  v1.3 2026-07-12
#       Use sys.executable instead of a hardcoded 'python' when launching
#       png_info.py as a subprocess, for portability to systems without a
#       'python' executable on PATH.
#  v1.2 2026-07-11
#       Added tests covering multi-file processing and partial-failure exit status.
#  v1.1 2024-01-30
#       Updated test cases to handle multiple reads with mock_open.
#  v1.0 2024-01-11
#       Initial test script for png_info.py
#
########################################################################

import os
import struct
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import mock_open, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from png_info import read_png_info


def write_minimal_png(path, width, height, bit_depth=8, color_type=2):
    """ Write a minimal PNG file containing only a signature and an IHDR chunk. """
    ihdr_data = struct.pack(">IIBBBBB", width, height, bit_depth, color_type, 0, 0, 0)
    with open(path, 'wb') as f:
        f.write(b'\x89PNG\r\n\x1a\n')
        f.write(struct.pack(">I4s", len(ihdr_data), b'IHDR'))
        f.write(ihdr_data)


class TestPngInfo(unittest.TestCase):
    """ Unit tests for the png_info.py script. """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'png_info.py')

        proc = subprocess.Popen([sys.executable, script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @patch('builtins.open', new_callable=mock_open)
    def test_valid_png(self, mock_file):
        """ Test reading information from a valid PNG file. """
        # Define the behavior of the fake read method to simulate file content reading
        mock_file.return_value.read.side_effect = [
            b'\x89PNG\r\n\x1a\n',  # PNG signature
            b'\x00\x00\x00\rIHDR',  # IHDR chunk length and type
            b'\x00\x00\x03\x20\x00\x00\x02\x58\x08\x06\x00\x00\x00',  # IHDR chunk data (width, height, bit depth, color type)
            b'',  # Indicate no more data
        ]

        expected_result = (800, 600, 8, 'PNG-32')
        result = read_png_info('valid.png')
        self.assertEqual(result, expected_result)

    @patch('builtins.open', new_callable=mock_open, read_data=b'Invalid data')
    def test_invalid_png(self, mock_file):
        """ Test reading information from an invalid PNG file. """
        with self.assertRaises(ValueError):
            read_png_info('invalid.png')

    @patch('builtins.open', new_callable=mock_open)
    def test_incomplete_png(self, mock_file):
        """ Test reading information from an incomplete PNG file. """
        # Define the behavior of the fake read method to simulate file content reading
        mock_file.return_value.read.side_effect = [
            b'\x89PNG\r\n\x1a\n',  # PNG signature
            b'\x00\x00\x00\x02IHDR',  # IHDR chunk length and type (incomplete)
            b'',  # Indicate data is missing
        ]

        with self.assertRaises(struct.error):
            read_png_info('incomplete.png')

    def test_multiple_files_are_all_processed(self):
        """ Test that every file matched across arguments is processed, not just the first. """
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'png_info.py')

        with tempfile.TemporaryDirectory() as tmpdir:
            path_a = os.path.join(tmpdir, 'a.png')
            path_b = os.path.join(tmpdir, 'b.png')
            write_minimal_png(path_a, 10, 20)
            write_minimal_png(path_b, 30, 40)

            proc = subprocess.Popen([sys.executable, script_path, path_a, path_b],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            out, err = proc.communicate()
            out = out.decode('utf-8')

            self.assertEqual(proc.returncode, 0)
            self.assertIn('File: {}'.format(path_a), out)
            self.assertIn('File: {}'.format(path_b), out)

    def test_failing_file_is_reported_and_processing_continues(self):
        """ Test that a failing file is reported on stderr while remaining files still succeed. """
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'png_info.py')

        with tempfile.TemporaryDirectory() as tmpdir:
            path_bad = os.path.join(tmpdir, 'bad.png')
            path_good = os.path.join(tmpdir, 'good.png')
            with open(path_bad, 'wb') as f:
                f.write(b'not a png')
            write_minimal_png(path_good, 50, 60)

            proc = subprocess.Popen([sys.executable, script_path, path_bad, path_good],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            out, err = proc.communicate()
            out = out.decode('utf-8')
            err = err.decode('utf-8')

            self.assertEqual(proc.returncode, 1)
            self.assertIn('[ERROR]', err)
            self.assertIn(path_bad, err)
            self.assertIn('File: {}'.format(path_good), out)


if __name__ == '__main__':
    unittest.main()
