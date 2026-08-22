#!/usr/bin/env python

########################################################################
# install_conda_test.py: Regression tests for install_conda.sh
#
#  Description:
#  This test suite verifies Conda package-name handling and failure
#  propagation without invoking a real Conda installation.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      python test/install_conda_test.py
#
#  Test Cases:
#    - Preserve the complete xgboost package name during installation.
#    - Stop before package installation when the Conda update fails.
#    - Stop after the first package failure.
#
#  Version History:
#  v1.0 2026-08-22
#       Add regression tests for Conda installation failures and package names.
#
########################################################################

import os
import subprocess
import sys
import tempfile
import unittest


REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INSTALLER = os.path.join(REPO_ROOT, 'installer', 'install_conda.sh')


class TestInstallConda(unittest.TestCase):
    def run_installer(self, tmpdir, update_status='0', scipy_status='0'):
        bin_dir = os.path.join(tmpdir, 'bin')
        os.makedirs(bin_dir)
        log_path = os.path.join(tmpdir, 'conda.log')
        conda_path = os.path.join(bin_dir, 'conda')
        with open(conda_path, 'w') as conda:
            conda.write(
                '#!/bin/sh\n'
                'printf "%s\\n" "$*" >> "$FAKE_CONDA_LOG"\n'
                'if [ "$1" = "update" ] && [ "$2" = "-n" ] && '
                '[ "$3" = "base" ] && [ "$4" = "-y" ] && '
                '[ "$5" = "conda" ]; then\n'
                '    exit "$FAKE_CONDA_UPDATE_STATUS"\n'
                'fi\n'
                'if [ "$1" = "install" ] && [ "$3" = "scipy" ]; then\n'
                '    exit "$FAKE_CONDA_SCIPY_STATUS"\n'
                'fi\n'
                'exit 0\n'
            )
        os.chmod(conda_path, 0o755)

        env = os.environ.copy()
        env['FAKE_CONDA_LOG'] = log_path
        env['FAKE_CONDA_UPDATE_STATUS'] = update_status
        env['FAKE_CONDA_SCIPY_STATUS'] = scipy_status
        env['HOME'] = tmpdir
        proc = subprocess.Popen(
            ['sh', INSTALLER, tmpdir],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=env
        )
        stdout, stderr = proc.communicate()
        with open(log_path, 'r') as log:
            calls = log.read().splitlines()
        return proc.returncode, stdout.decode('utf-8'), stderr.decode('utf-8'), calls

    def test_success_preserves_xgboost_name(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            returncode, stdout, stderr, calls = self.run_installer(tmpdir)

        self.assertEqual(returncode, 0)
        self.assertIn('install -y xgboost', calls)
        self.assertFalse(any('xgboos' in call and 'xgboost' not in call
                             for call in calls))

    def test_update_failure_skips_package_installation(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            returncode, stdout, stderr, calls = self.run_installer(
                tmpdir, update_status='1'
            )

        self.assertNotEqual(returncode, 0)
        self.assertFalse(any(call.startswith('install ') for call in calls))
        self.assertNotIn(
            '[INFO] All specified conda packages have been installed.',
            stdout
        )

    def test_package_failure_stops_following_packages(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            returncode, stdout, stderr, calls = self.run_installer(
                tmpdir, scipy_status='1'
            )

        self.assertNotEqual(returncode, 0)
        self.assertNotIn('install -y pandas', calls)
        self.assertNotIn(
            '[INFO] All specified conda packages have been installed.',
            stdout
        )


if __name__ == '__main__':
    unittest.main()
