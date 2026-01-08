#!/usr/bin/env python

########################################################################
# namecalc_test.py: Test suite for Numerology Calculation Script
#
#  Description:
#  This test suite verifies the correctness of the namecalc.py script.
#  It tests the script's functionality by providing various input strings
#  and comparing the script's output against expected results.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Running the tests:
#  Execute the test script from the command line:
#      python test/namecalc_test.py
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Produce the expected numerology triangle output for input "1234512345 5432154321".
#    - Produce the expected numerology triangle output for input "1234512345 1234512345".
#    - Produce the expected numerology triangle output for input "111 153 111 115".
#    - Produce the expected numerology triangle output for input "1 2 3 4 5 5 4 3 2 1".
#    - Produce the expected numerology triangle output for input "5 a 4 b 3 c 2 a 1 1a2b3c4d5e".
#
#  Version History:
#  v1.0 2023-12-13
#       Initial release. Test suite for namecalc.rb script.
#
########################################################################

import os
import subprocess
import sys
import unittest
from contextlib import contextmanager
from io import StringIO

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import namecalc


@contextmanager
def captured_output():
    new_out = StringIO()
    old_out = sys.stdout
    try:
        sys.stdout = new_out
        yield new_out
    finally:
        sys.stdout = old_out

class TestNameCalc(unittest.TestCase):

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'namecalc.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def run_test(self, input_str, expected_output):
        with captured_output() as out:
            sys.argv = ['namecalc.py'] + input_str.split()
            namecalc.main()
        self.assertEqual(out.getvalue().strip(), expected_output.strip())

    def test_numerology_1234512345_5432154321(self):
        self.run_test(
            '1234512345 5432154321',
            " 1 2 3 4 5 1 2 3 4 5 5 4 3 2 1 5 4 3 2 1\n"
            "  3 5 7 9 6 3 5 7 9 0 9 7 5 3 6 9 7 5 3\n"
            "   8 2 6 5 9 8 2 6 9 9 6 2 8 9 5 6 2 8\n"
            "    0 8 1 4 7 0 8 5 8 5 8 0 7 4 1 8 0\n"
            "     8 9 5 1 7 8 3 3 3 3 8 7 1 5 9 8\n"
            "      7 4 6 8 5 1 6 6 6 1 5 8 6 4 7\n"
            "       1 0 4 3 6 7 2 2 7 6 3 4 0 1\n"
            "        1 4 7 9 3 9 4 9 3 9 7 4 1\n"
            "         5 1 6 2 2 3 3 2 2 6 1 5\n"
            "          6 7 8 4 5 6 5 4 8 7 6\n"
            "           3 5 2 9 1 1 9 2 5 3\n"
            "            8 7 1 0 2 0 1 7 8\n"
            "             5 8 1 2 2 1 8 5\n"
            "              3 9 3 4 3 9 3\n"
            "               2 2 7 7 2 2\n"
            "                4 9 4 9 4\n"
            "                 3 3 3 3\n"
            "                  6 6 6\n"
            "                   2 2")

    def test_numerology_1234512345_1234512345(self):
        self.run_test(
            '1234512345 1234512345',
            " 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5\n"
            "  3 5 7 9 6 3 5 7 9 6 3 5 7 9 6 3 5 7 9\n"
            "   8 2 6 5 9 8 2 6 5 9 8 2 6 5 9 8 2 6\n"
            "    0 8 1 4 7 0 8 1 4 7 0 8 1 4 7 0 8\n"
            "     8 9 5 1 7 8 9 5 1 7 8 9 5 1 7 8\n"
            "      7 4 6 8 5 7 4 6 8 5 7 4 6 8 5\n"
            "       1 0 4 3 2 1 0 4 3 2 1 0 4 3\n"
            "        1 4 7 5 3 1 4 7 5 3 1 4 7\n"
            "         5 1 2 8 4 5 1 2 8 4 5 1\n"
            "          6 3 0 2 9 6 3 0 2 9 6\n"
            "           9 3 2 1 5 9 3 2 1 5\n"
            "            2 5 3 6 4 2 5 3 6\n"
            "             7 8 9 0 6 7 8 9\n"
            "              5 7 9 6 3 5 7\n"
            "               2 6 5 9 8 2\n"
            "                8 1 4 7 0\n"
            "                 9 5 1 7\n"
            "                  4 6 8\n"
            "                   0 4")

    def test_numerology_111_153_111_115(self):
        self.run_test(
            '111 153 111 115',
            " 1 1 1 1 5 3 1 1 1 1 1 5\n"
            "  2 2 2 6 8 4 2 2 2 2 6\n"
            "   4 4 8 4 2 6 4 4 4 8\n"
            "    8 2 2 6 8 0 8 8 2\n"
            "     0 4 8 4 8 8 6 0\n"
            "      4 2 2 2 6 4 6\n"
            "       6 4 4 8 0 0\n"
            "        0 8 2 8 0\n"
            "         8 0 0 8\n"
            "          8 0 8\n"
            "           8 8")

    def test_numerology_1_2_3_4_5_5_4_3_2_1(self):
        self.run_test(
            '1 2 3 4 5 5 4 3 2 1',
            " 1 2 3 4 5 5 4 3 2 1\n"
            "  3 5 7 9 0 9 7 5 3\n"
            "   8 2 6 9 9 6 2 8\n"
            "    0 8 5 8 5 8 0\n"
            "     8 3 3 3 3 8\n"
            "      1 6 6 6 1\n"
            "       7 2 2 7\n"
            "        9 4 9\n"
            "         3 3")

    def test_numerology_5_a_4_b_3_c_2_a_1_1a2b3c4d5e(self):
        self.run_test(
            '5 a 4 b 3 c 2 a 1 1a2b3c4d5e',
            " 5 4 3 2 1 1 2 3 4 5\n"
            "  9 7 5 3 2 3 5 7 9\n"
            "   6 2 8 5 5 8 2 6\n"
            "    8 0 3 0 3 0 8\n"
            "     8 3 3 3 3 8\n"
            "      1 6 6 6 1\n"
            "       7 2 2 7\n"
            "        9 4 9\n"
            "         3 3")


if __name__ == '__main__':
    unittest.main()
