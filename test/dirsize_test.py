#!/usr/bin/env python

########################################################################
# dirsize_test.py: Test script for dirsize.py
#
#  Description:
#  Tests the functionality of the dirsize.py script, including file size
#  conversion and directory existence check.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2023-12-19
#       Initial release.
#
#  Usage:
#  Run this script from the command line using:
#      python test/dirsize_test.py
#
########################################################################

import os
import subprocess
import sys
import unittest

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import dirsize


class TestDirsize(unittest.TestCase):
    """ Test cases for dirsize.py script. """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'dirsize.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_convert_kibi(self):
        """ Test kibi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(1023), "1023.00 B")
        self.assertEqual(dirsize.convert_size(1024), "1.00 KiB")
        self.assertEqual(dirsize.convert_size(1025), "1.00 KiB")

    def test_convert_mebi(self):
        """ Test mebi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(1048575), "1024.00 KiB")
        self.assertEqual(dirsize.convert_size(1048576), "1.00 MiB")
        self.assertEqual(dirsize.convert_size(1048577), "1.00 MiB")

    def test_convert_gibi(self):
        """ Test gibi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(1073741823), "1024.00 MiB")
        self.assertEqual(dirsize.convert_size(1073741824), "1.00 GiB")
        self.assertEqual(dirsize.convert_size(1073741825), "1.00 GiB")

    def test_convert_tebi(self):
        """ Test tebi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(1099511627775), "1024.00 GiB")
        self.assertEqual(dirsize.convert_size(1099511627776), "1.00 TiB")
        self.assertEqual(dirsize.convert_size(1099511627777), "1.00 TiB")

    def test_convert_pebi(self):
        """ Test pebi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(1125899906842623), "1024.00 TiB")
        self.assertEqual(dirsize.convert_size(1125899906842624), "1.00 PiB")
        self.assertEqual(dirsize.convert_size(1125899906842625), "1.00 PiB")

    def test_convert_exbi(self):
        """ Test exbi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(
            1152921504606846975), "1024.00 PiB")
        self.assertEqual(dirsize.convert_size(1152921504606846976), "1.00 EiB")
        self.assertEqual(dirsize.convert_size(1152921504606846977), "1.00 EiB")

    def test_convert_zebi(self):
        """ Test zebi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(
            1180591620717411303423), "1024.00 EiB")
        self.assertEqual(dirsize.convert_size(
            1180591620717411303424), "1.00 ZiB")
        self.assertEqual(dirsize.convert_size(
            1180591620717411303425), "1.00 ZiB")

    def test_convert_yobi(self):
        """ Test yobi bytes of the convert_size function from dirsize.py. """
        self.assertEqual(dirsize.convert_size(
            1208925819614629174706175), "1024.00 ZiB")
        self.assertEqual(dirsize.convert_size(
            1208925819614629174706176), "1.00 YiB")
        self.assertEqual(dirsize.convert_size(
            1208925819614629174706177), "1.00 YiB")

    def test_directory_existence(self):
        """ Test if the script checks for the existence of the directory. """
        non_existent_dir = "/path/to/nonexistent/dir"
        result = subprocess.call(
            [sys.executable, "dirsize.py", non_existent_dir],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        self.assertNotEqual(result, 0)


if __name__ == '__main__':
    unittest.main()
