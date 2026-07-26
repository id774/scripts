#!/usr/bin/env python

########################################################################
# apache_log_analysis_cron_test.py: Test for cron/bin/apache_log_analysis
#
#  Description:
#  This script verifies that the cron wrapper runs all three Apache
#  analysis helpers independently and aggregates their exit statuses.
#  The wrapper is copied into a temporary directory with its production
#  paths rewritten to fixtures, so no system directory is touched.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Exits zero and logs start/end markers when all helpers succeed.
#    - Runs the third helper even when the second one fails, and exits non-zero.
#    - Records the failing helper name and its exit status in the job log.
#    - Exits non-zero when a helper is missing, while running the others.
#
#  Version History:
#  v1.0 2026-07-26
#       Initial release.
#
########################################################################

import os
import stat
import subprocess
import tempfile
import unittest

SCRIPT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CRON_SCRIPT = os.path.join(SCRIPT_DIR, 'cron', 'bin', 'apache_log_analysis')


class TestApacheLogAnalysisCron(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.joblog = os.path.join(self.tmp.name, 'apache_summary.log')
        self.access_log = os.path.join(self.tmp.name, 'ssl_access.log')
        with open(self.access_log, 'w') as f:
            f.write('127.0.0.1 - - [26/Jul/2026:20:15:40 +0900] "GET / HTTP/1.1" 200 1 "-" "UA"\n')

        self.helpers = {}
        for name in ('apache_log_analysis.sh', 'apache_calculater.py', 'apache_blog_analysis.py'):
            self.helpers[name] = os.path.join(self.tmp.name, name)

    def tearDown(self):
        self.tmp.cleanup()

    def write_helper(self, name, exit_status=0):
        path = self.helpers[name]
        with open(path, 'w') as f:
            f.write('#!/bin/sh\n')
            f.write('echo "ran %s"\n' % name)
            f.write('exit %d\n' % exit_status)
        os.chmod(path, os.stat(path).st_mode | stat.S_IXUSR)
        return path

    def build_wrapper(self):
        with open(CRON_SCRIPT, 'r') as f:
            content = f.read()

        replacements = {
            '"/var/log/sysadmin/apache_summary.log"': '"%s"' % self.joblog,
            '"/etc/cron.exec/apache_log_analysis.sh"': '"%s"' % self.helpers['apache_log_analysis.sh'],
            '"/etc/cron.exec/apache_calculater.py"': '"%s"' % self.helpers['apache_calculater.py'],
            '"/etc/cron.exec/apache_blog_analysis.py"': '"%s"' % self.helpers['apache_blog_analysis.py'],
            '"/var/log/apache2/ssl_access.log"': '"%s"' % self.access_log,
            '"/var/log/apache2/ssl_access.log.1"': '"%s.1"' % self.access_log,
        }
        for original, replacement in replacements.items():
            self.assertIn(original, content)
            content = content.replace(original, replacement)

        wrapper = os.path.join(self.tmp.name, 'apache_log_analysis')
        with open(wrapper, 'w') as f:
            f.write(content)
        return wrapper

    def run_wrapper(self):
        wrapper = self.build_wrapper()
        process = subprocess.Popen(['/bin/sh', wrapper],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        process.communicate()
        with open(self.joblog, 'r') as f:
            joblog = f.read()
        return process.returncode, joblog

    def test_all_helpers_succeed(self):
        for name in self.helpers:
            self.write_helper(name)
        status, joblog = self.run_wrapper()
        self.assertEqual(status, 0)
        self.assertIn('Job started on', joblog)
        self.assertIn('Job ended on', joblog)
        for name in self.helpers:
            self.assertIn('ran %s' % name, joblog)

    def test_later_helper_runs_after_a_failure(self):
        self.write_helper('apache_log_analysis.sh')
        self.write_helper('apache_calculater.py', exit_status=7)
        self.write_helper('apache_blog_analysis.py')
        status, joblog = self.run_wrapper()
        self.assertNotEqual(status, 0)
        self.assertIn('ran apache_blog_analysis.py', joblog)
        self.assertIn('apache_calculater.py failed with exit status 7', joblog)
        self.assertIn('Job started on', joblog)
        self.assertIn('Job ended on', joblog)

    def test_missing_helper_fails_and_others_run(self):
        self.write_helper('apache_log_analysis.sh')
        self.write_helper('apache_blog_analysis.py')
        status, joblog = self.run_wrapper()
        self.assertNotEqual(status, 0)
        self.assertIn('IP analysis script not found', joblog)
        self.assertIn('ran apache_blog_analysis.py', joblog)
        self.assertIn('Job ended on', joblog)


if __name__ == '__main__':
    unittest.main()
