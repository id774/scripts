#!/usr/bin/env python

########################################################################
# simple_passwd_test.py: Unit tests for simple_passwd.py
#
#  Description:
#  This script tests the password generation functionality of simple_passwd.py.
#  It verifies password length and character composition with and without symbols.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-07-03
#       Update tests to reflect new rule: symbols are always included if enabled.
#  v1.0 2025-06-24
#       Initial test implementation for generate_passwd.
#
#  Test Cases:
#    - test_generate_with_symbols:
#        Confirms length is correct and at least one symbol is present.
#    - test_generate_without_symbols:
#        Confirms length is correct and no symbols are present.
#    - test_generate_invalid_length_zero:
#        Verifies that length zero raises SystemExit with error.
#    - test_generate_single_length_with_symbol:
#        Confirms that one-character password includes symbol if enabled.
#
########################################################################

import os
import sys

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import contextlib
import io
import string
import unittest
from unittest.mock import patch

from simple_passwd import generate_passwd


class TestSimplePassword(unittest.TestCase):
    def test_generate_with_symbols(self):
        with patch('builtins.print') as mock_print:
            generate_passwd(16, use_symbols=True)
            output = mock_print.call_args[0][0]
            self.assertEqual(len(output), 16)
            self.assertTrue(any(c in '_-!#&' for c in output))

    def test_generate_without_symbols(self):
        with patch('builtins.print') as mock_print:
            generate_passwd(20, use_symbols=False)
            output = mock_print.call_args[0][0]
            self.assertEqual(len(output), 20)
            self.assertTrue(all(c in string.ascii_letters + string.digits for c in output))

    def test_generate_invalid_length_zero(self):
        with contextlib.redirect_stderr(io.StringIO()):
            with self.assertRaises(SystemExit) as cm:
                generate_passwd(0, use_symbols=True)
        self.assertEqual(cm.exception.code, 1)

    def test_generate_single_length_with_symbol(self):
        with patch('builtins.print') as mock_print:
            generate_passwd(1, use_symbols=True)
            output = mock_print.call_args[0][0]
            self.assertEqual(len(output), 1)
            self.assertIn(output, '_-!#&')


if __name__ == '__main__':
    unittest.main()
