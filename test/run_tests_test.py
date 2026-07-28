#!/usr/bin/env python

########################################################################
# run_tests_test.py: Test for run_tests.sh
#
#  Description:
#  This script verifies that run_tests.sh derives its overall result from
#  the exit status of each test script, not only from the presence of
#  success keywords in the captured output. A temporary fixture directory
#  is used as SCRIPTS so that the real test suite is not re-executed.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Treats a Python test that prints "OK" but exits non-zero as a failure.
#    - Reports success when the Python test prints "OK" and exits zero.
#    - Reports success when no Python or Ruby test scripts are present.
#
#  Version History:
#  v1.0 2026-07-26
#       Initial release.
#
########################################################################

import os
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RUN_TESTS = os.path.join(SCRIPT_DIR, 'run_tests.sh')


class TestRunTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        os.mkdir(os.path.join(self.tmp.name, 'test'))

    def tearDown(self):
        self.tmp.cleanup()

    def write_python_test(self, body):
        path = os.path.join(self.tmp.name, 'test', 'fixture_test.py')
        with open(path, 'w') as f:
            f.write(body)
        return path

    def run_run_tests(self):
        env = dict(os.environ)
        env['SCRIPTS'] = self.tmp.name
        process = subprocess.Popen(
            ['/bin/sh', RUN_TESTS, sys.executable],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)
        stdout, stderr = process.communicate()
        return process.returncode, stdout.decode('utf-8'), stderr.decode('utf-8')

    def test_ok_output_with_non_zero_status_fails(self):
        self.write_python_test(
            'import sys\n'
            'print("Ran 1 test in 0.001s")\n'
            'print("OK")\n'
            'sys.exit(1)\n'
        )
        status, _, stderr = self.run_run_tests()
        self.assertEqual(status, 1)
        self.assertIn('fixture_test.py', stderr)

    def test_ok_output_with_zero_status_passes(self):
        self.write_python_test(
            'print("Ran 1 test in 0.001s")\n'
            'print("OK")\n'
        )
        status, stdout, _ = self.run_run_tests()
        self.assertEqual(status, 0)
        self.assertIn('All tests passed successfully.', stdout)

    def test_no_test_scripts_passes(self):
        status, stdout, _ = self.run_run_tests()
        self.assertEqual(status, 0)
        self.assertIn('All tests passed successfully.', stdout)


if __name__ == '__main__':
    unittest.main()
