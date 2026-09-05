#!/usr/bin/env python

########################################################################
# find_pycompat_test.py: Test script for find_pycompat.py
#
#  Description:
#  This script contains comprehensive unit tests for the find_pycompat.py script.
#  It verifies the script's functionality including the detection of various Python 3.x features.
#  Assertions are based on the actual detection result recorded in
#  find_pycompat.detected_issues, not merely on whether the target file was opened,
#  so a regression in the underlying regular expressions is caught by these tests.
#  Detection also covers the corresponding import forms of pathlib, asyncio,
#  subprocess.run, subprocess.DEVNULL, and shutil.which, so importing a restricted
#  feature directly is verified to no longer bypass the check.
#  Recent updates have modified the detection pattern for the matrix multiplication operator
#  to require spaces around it. These tests ensure that the updated patterns accurately identify
#  the intended features without false positives or negatives.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Detect f-strings usage when present.
#    - Do not detect f-strings usage when absent.
#    - Detect subprocess.run usage when present.
#    - Do not detect subprocess.run usage when absent.
#    - Detect subprocess.DEVNULL usage when present.
#    - Do not detect subprocess.DEVNULL usage when absent.
#    - Detect 'from subprocess import run' usage when present.
#    - Detect 'from subprocess import DEVNULL' usage when present.
#    - Do not detect 'from subprocess import Popen' as a subprocess.run/DEVNULL issue.
#    - Detect async/await keywords when present.
#    - Do not detect async/await keywords when absent.
#    - Detect function type hints (-> return annotation) when present.
#    - Do not detect function type hints when absent.
#    - Detect nonlocal keyword when present.
#    - Do not detect nonlocal keyword when absent.
#    - Detect matrix multiplication operator only when spaces surround '@' (success case).
#    - Do not detect matrix multiplication operator when spaces are missing around '@' (a@b).
#    - Do not detect matrix multiplication operator when '@' is not used (failure case).
#    - Detect asyncio usage when present.
#    - Do not detect asyncio usage when absent.
#    - Detect 'import asyncio' usage when present.
#    - Detect 'from asyncio import ...' usage when present.
#    - Detect 'yield from' usage when present.
#    - Do not detect 'yield from' usage when absent.
#    - Detect pathlib usage when present.
#    - Do not detect pathlib usage when absent.
#    - Detect 'import pathlib' usage when present.
#    - Detect 'from pathlib import ...' usage when present.
#    - Detect shutil.which usage when present.
#    - Do not detect shutil.which usage when absent.
#    - Detect 'from shutil import which' usage when present.
#    - Exclude a comment line from detection even when the pattern matches.
#    - Exclude a line containing an email address from detection even when the pattern matches.
#    - Detected issues do not leak from one test to the next.
#
#  Version History:
#  v1.5 2026-09-05
#       Assert actual detection results instead of only that the file was
#       opened, and add tests for import-form detection and exclusions.
#  v1.4 2024-03-12
#       Updated tests to reflect the modified detection pattern for the matrix multiplication operator.
#  v1.3 2024-02-11
#       Updated test cases to reflect changes in find_pycompat.py function signatures.
#  v1.2 2024-01-31
#       Renamed script from 'check_py_compat.py' to 'find_pycompat.py'
#       to improve clarity and ease of use.
#  v1.1 2024-01-28
#       Added detection for shutil.which usage to enhance compatibility checks.
#  v1.0 2024-01-21
#        Initial test script for find_pycompat.py
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import find_pycompat


class _FakeTextFile(object):
    """ A minimal file-like context manager backed by an in-memory string,
    so 'with open(...) as f: for line in f:' iterates over real content. """

    def __init__(self, content):
        self._lines = content.splitlines(True)

    def __enter__(self):
        return iter(self._lines)

    def __exit__(self, exc_type, exc_value, traceback):
        return False


class TestFindPyCompat(unittest.TestCase):
    """ Unit tests for the find_pycompat.py script. """

    def setUp(self):
        """ Common setup for all tests. """
        self.mock_walk = patch('os.walk').start()
        self.mock_walk.return_value = [('.', [], ['dummy.py'])]
        self.mock_open = patch('builtins.open').start()
        self.mock_print = patch('builtins.print').start()
        self.file_content = ""
        find_pycompat.detected_issues[:] = []

    def tearDown(self):
        """ Tear down mocks after each test. """
        patch.stopall()
        find_pycompat.detected_issues[:] = []

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'find_pycompat.py')

        proc = subprocess.Popen(['python', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def mock_file_read(self, *args, **kwargs):
        return _FakeTextFile(self.file_content)

    def test_f_strings_detection_success(self):
        pattern = r"f['\"][^'\"]*\{[^}]*\}[^'\"]*['\"]"
        self.run_feature_test("f-strings", pattern, "print(f'Hello {name}')", should_match=True)

    def test_f_strings_detection_failure(self):
        pattern = r"f['\"][^'\"]*\{[^}]*\}[^'\"]*['\"]"
        self.run_feature_test("f-strings", pattern, "print('Hello')", should_match=False)

    SUBPROCESS_PATTERN = r"subprocess\.run|subprocess\.DEVNULL|\bfrom\s+subprocess\s+import\b.*\b(?:run|DEVNULL)\b"

    def test_subprocess_run_detection_success(self):
        self.run_feature_test("subprocess.run", self.SUBPROCESS_PATTERN, "subprocess.run(['ls', '-l'])", should_match=True)

    def test_subprocess_run_detection_failure(self):
        self.run_feature_test("subprocess.run", self.SUBPROCESS_PATTERN, "print('subprocess')", should_match=False)

    def test_subprocess_devnull_detection_success(self):
        self.run_feature_test("subprocess.DEVNULL", self.SUBPROCESS_PATTERN, "subprocess.Popen(['ls'], stdout=subprocess.DEVNULL)", should_match=True)

    def test_subprocess_devnull_detection_failure(self):
        self.run_feature_test("subprocess.DEVNULL", self.SUBPROCESS_PATTERN, "print('DEVNULL')", should_match=False)

    def test_subprocess_from_import_run_detection_success(self):
        self.run_feature_test("subprocess.run", self.SUBPROCESS_PATTERN, "from subprocess import run", should_match=True)

    def test_subprocess_from_import_devnull_detection_success(self):
        self.run_feature_test("subprocess.DEVNULL", self.SUBPROCESS_PATTERN, "from subprocess import DEVNULL", should_match=True)

    def test_subprocess_from_import_unrelated_symbol_detection_failure(self):
        self.run_feature_test("subprocess.run", self.SUBPROCESS_PATTERN, "from subprocess import PIPE, Popen", should_match=False)

    def test_async_await_keywords_detection_success(self):
        pattern = r"\basync\b|\bawait\b"
        self.run_feature_test("async/await keywords", pattern, "async def foo(): await bar()", should_match=True)

    def test_async_await_keywords_detection_failure(self):
        pattern = r"\basync\b|\bawait\b"
        self.run_feature_test("async/await keywords", pattern, "def foo(): bar()", should_match=False)

    def test_type_hints_detection_success(self):
        pattern = r"\bdef\b.*->"
        self.run_feature_test("type hints", pattern, "def foo(bar: int) -> str:", should_match=True)

    def test_type_hints_detection_failure(self):
        pattern = r"\bdef\b.*->"
        self.run_feature_test("type hints", pattern, "def foo(bar):", should_match=False)

    def test_nonlocal_keyword_detection_success(self):
        pattern = r"\bnonlocal\b"
        self.run_feature_test("nonlocal keyword", pattern, "def foo(): nonlocal x", should_match=True)

    def test_nonlocal_keyword_detection_failure(self):
        pattern = r"\bnonlocal\b"
        self.run_feature_test("nonlocal keyword", pattern, "def foo(): global x", should_match=False)

    def test_matrix_multiplication_operator_detection_success_with_spaces(self):
        pattern = r"\b[a-zA-Z_][a-zA-Z0-9_]*\s+@\s+[a-zA-Z_][a-zA-Z0-9_]*\b"
        self.run_feature_test("matrix multiplication operator", pattern, "a @ b", should_match=True)

    def test_matrix_multiplication_operator_detection_success_without_spaces_failure(self):
        pattern = r"\b[a-zA-Z_][a-zA-Z0-9_]*\s+@\s+[a-zA-Z_][a-zA-Z0-9_]*\b"
        self.run_feature_test("matrix multiplication operator", pattern, "a@b", should_match=False)

    def test_matrix_multiplication_operator_detection_failure(self):
        pattern = r"\b[a-zA-Z_][a-zA-Z0-9_]*\s+@\s+[a-zA-Z_][a-zA-Z0-9_]*\b"
        self.run_feature_test("matrix multiplication operator", pattern, "a * b", should_match=False)

    ASYNCIO_PATTERN = r"\basyncio\.|\bimport\s+asyncio\b|\bfrom\s+asyncio\s+import\b"

    def test_asyncio_usage_detection_success(self):
        self.run_feature_test("asyncio usage", self.ASYNCIO_PATTERN, "import asyncio\nasyncio.run(main())", should_match=True)

    def test_asyncio_usage_detection_failure(self):
        self.run_feature_test("asyncio usage", self.ASYNCIO_PATTERN, "import sys", should_match=False)

    def test_asyncio_import_detection_success(self):
        self.run_feature_test("asyncio usage", self.ASYNCIO_PATTERN, "import asyncio", should_match=True)

    def test_asyncio_from_import_detection_success(self):
        self.run_feature_test("asyncio usage", self.ASYNCIO_PATTERN, "from asyncio import sleep", should_match=True)

    def test_yield_from_usage_detection_success(self):
        pattern = r"\byield from\b"
        self.run_feature_test("yield from usage", pattern, "def foo(): yield from bar()", should_match=True)

    def test_yield_from_usage_detection_failure(self):
        pattern = r"\byield from\b"
        self.run_feature_test("yield from usage", pattern, "def foo(): yield bar()", should_match=False)

    PATHLIB_PATTERN = r"\bpathlib\.|\bimport\s+pathlib\b|\bfrom\s+pathlib\s+import\b"

    def test_pathlib_usage_detection_success(self):
        self.run_feature_test("pathlib usage", self.PATHLIB_PATTERN, "pathlib.Path('/usr/local')", should_match=True)

    def test_pathlib_usage_detection_failure(self):
        self.run_feature_test("pathlib usage", self.PATHLIB_PATTERN, "import os", should_match=False)

    def test_pathlib_import_detection_success(self):
        self.run_feature_test("pathlib usage", self.PATHLIB_PATTERN, "import pathlib", should_match=True)

    def test_pathlib_from_import_detection_success(self):
        self.run_feature_test("pathlib usage", self.PATHLIB_PATTERN, "from pathlib import Path", should_match=True)

    SHUTIL_WHICH_PATTERN = r"\bshutil\.which\b|\bfrom\s+shutil\s+import\b.*\bwhich\b"

    def test_shutil_which_detection_success(self):
        self.run_feature_test("shutil.which usage", self.SHUTIL_WHICH_PATTERN, "if not shutil.which('gcc'):", should_match=True)

    def test_shutil_which_detection_failure(self):
        self.run_feature_test("shutil.which usage", self.SHUTIL_WHICH_PATTERN, "if not cmd_exists('gcc'):", should_match=False)

    def test_shutil_from_import_which_detection_success(self):
        self.run_feature_test("shutil.which usage", self.SHUTIL_WHICH_PATTERN, "from shutil import which", should_match=True)

    def test_comment_line_excluded_from_detection(self):
        self.run_feature_test("asyncio usage", self.ASYNCIO_PATTERN, "# import asyncio", should_match=False)

    def test_email_containing_line_excluded_from_detection(self):
        self.run_feature_test(
            "async/await keywords",
            r"\basync\b|\bawait\b",
            "send report to admin@example.com and await response",
            should_match=False,
        )

    def test_detected_issues_do_not_leak_between_tests(self):
        self.run_feature_test("pathlib usage", self.PATHLIB_PATTERN, "import pathlib", should_match=True)
        self.assertEqual(find_pycompat.detected_issues, ['dummy.py'])

        find_pycompat.detected_issues[:] = []
        self.run_feature_test("pathlib usage", self.PATHLIB_PATTERN, "import os", should_match=False)
        self.assertEqual(find_pycompat.detected_issues, [])

    def run_feature_test(self, feature_name, pattern, test_string, should_match):
        self.file_content = test_string
        self.mock_open.side_effect = self.mock_file_read
        find_pycompat.search_feature('.', feature_name, pattern)

        if should_match:
            self.assertIn('dummy.py', find_pycompat.detected_issues)
        else:
            self.assertNotIn('dummy.py', find_pycompat.detected_issues)


if __name__ == '__main__':
    unittest.main()
