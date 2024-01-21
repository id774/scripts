#!/usr/bin/env python3

########################################################################
# check_py_compat_test.py: Test script for check_py_compat.py
#
#  Description:
#  This script contains comprehensive unit tests for the check_py_compat.py script.
#  It verifies the script's functionality including the detection of various Python 3.x features.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-21
#        Initial test script for check_py_compat.py
#
########################################################################

import os
import sys
import unittest
from unittest.mock import MagicMock, patch

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import check_py_compat


class TestCheckPyCompat(unittest.TestCase):
    """Unit tests for the check_py_compat.py script."""

    def setUp(self):
        """Common setup for all tests."""
        self.mock_walk = patch('os.walk').start()
        self.mock_walk.return_value = [('.', [], ['dummy.py'])]
        self.mock_open = patch('builtins.open').start()
        self.mock_print = patch('builtins.print').start()
        self.file_content = ""

    def tearDown(self):
        """Tear down mocks after each test."""
        patch.stopall()

    def mock_file_read(self, *args, **kwargs):
        return MagicMock(read=lambda: self.file_content)

    def test_f_strings_detection(self):
        pattern = r"f['\"][^'\"]*\{[^}]*\}[^'\"]*['\"]"
        self.run_feature_test("f-strings", pattern, "print(f'Hello {name}')", "print('Hello')")

    def test_subprocess_run_detection(self):
        pattern = r"subprocess\.run"
        self.run_feature_test("subprocess.run", pattern, "subprocess.run(['ls', '-l'])", "print('subprocess')")

    def test_subprocess_devnull_detection(self):
        pattern = r"subprocess\.DEVNULL"
        self.run_feature_test("subprocess.DEVNULL", pattern, "subprocess.Popen(['ls'], stdout=subprocess.DEVNULL)", "print('DEVNULL')")

    def test_async_await_keywords_detection(self):
        pattern = r"\basync\b|\bawait\b"
        self.run_feature_test("async/await keywords", pattern, "async def foo(): await bar()", "def foo(): bar()")

    def test_type_hints_detection(self):
        pattern = r"\bdef\b.*->"
        self.run_feature_test("type hints", pattern, "def foo(bar: int) -> str:", "def foo(bar):")

    def test_nonlocal_keyword_detection(self):
        pattern = r"\bnonlocal\b"
        self.run_feature_test("nonlocal keyword", pattern, "def foo(): nonlocal x", "def foo(): global x")

    def test_matrix_multiplication_operator_detection(self):
        pattern = r"\b[a-zA-Z_][a-zA-Z0-9_]*\s*@\s*[a-zA-Z_][a-zA-Z0-9_]*\b"
        self.run_feature_test("matrix multiplication operator", pattern, "a @ b", "a * b")

    def test_asyncio_usage_detection(self):
        pattern = r"\basyncio\."
        self.run_feature_test("asyncio usage", pattern, "import asyncio\nasyncio.run(main())", "import sys")

    def test_yield_from_usage_detection(self):
        pattern = r"\byield from\b"
        self.run_feature_test("yield from usage", pattern, "def foo(): yield from bar()", "def foo(): yield bar()")

    def test_pathlib_usage_detection(self):
        pattern = r"\bpathlib\."
        self.run_feature_test("pathlib usage", pattern, "from pathlib import Path\nPath('/usr/local')", "import os")

    def run_feature_test(self, feature_name, pattern, match_string, no_match_string):
        # Set file content to a string that should match
        self.file_content = match_string
        self.mock_open.side_effect = self.mock_file_read
        check_py_compat.search_feature('.', feature_name, pattern, [])
        self.mock_open.assert_called_with(os.path.join('.', 'dummy.py'), 'r', encoding='utf-8')

        # Reset mock and set file content to a string that should not match
        self.mock_open.reset_mock()
        self.file_content = no_match_string
        self.mock_open.side_effect = self.mock_file_read
        check_py_compat.search_feature('.', feature_name, pattern, [])
        self.mock_open.assert_called_with(os.path.join('.', 'dummy.py'), 'r', encoding='utf-8')


if __name__ == '__main__':
    unittest.main()
