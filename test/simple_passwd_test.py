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
#  v1.0 2025-06-24
#       Initial test implementation for generate_passwd.
#
#  Test Cases:
#    - test_generate_length_with_symbols:
#        Checks that generated password is of correct length and includes symbols.
#    - test_generate_length_without_symbols:
#        Checks that generated password is of correct length and does not include symbols.
#    - test_generate_zero_length:
#        Ensures empty string is returned when length is zero.
#    - test_generate_symbols_toggle:
#        Verifies that symbol inclusion toggles as expected for large password.
#
########################################################################

import os
import sys

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import string
import unittest
from unittest.mock import patch

from simple_passwd import generate_passwd


class TestSimplePassword(unittest.TestCase):
    def test_generate_length_with_symbols(self):
        with patch('builtins.print') as mock_print:
            while True:
                generate_passwd(16, use_symbols=True)
                output = mock_print.call_args[0][0]
                if any(c in '_-!#&' for c in output):
                    self.assertEqual(len(output), 16)
                    break

    def test_generate_length_without_symbols(self):
        with patch('builtins.print') as mock_print:
            generate_passwd(20, use_symbols=False)
            output = mock_print.call_args[0][0]
            self.assertEqual(len(output), 20)
            self.assertTrue(all(c in string.ascii_letters + string.digits for c in output))

    def test_generate_zero_length(self):
        with patch('builtins.print') as mock_print:
            generate_passwd(0, use_symbols=True)
            output = mock_print.call_args[0][0]
            self.assertEqual(output, '')

    def test_generate_symbols_toggle(self):
        with patch('builtins.print') as mock_print:
            generate_passwd(100, use_symbols=True)
            output = mock_print.call_args[0][0]
            has_symbol = any(c in '_-!#&' for c in output)
            self.assertTrue(has_symbol)

        with patch('builtins.print') as mock_print:
            generate_passwd(100, use_symbols=False)
            output = mock_print.call_args[0][0]
            has_symbol = any(c in '_-!#&' for c in output)
            self.assertFalse(has_symbol)


if __name__ == '__main__':
    unittest.main()
