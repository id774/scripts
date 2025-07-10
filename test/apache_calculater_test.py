#!/usr/bin/env python

########################################################################
# apache_calculater_test.py: Unit tests for apache_calculater.py
#
#  Description:
#  This script tests the functionality of ApacheCalculater, including hit
#  counting, cache percentage calculation, and log format validation.
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Count IP hits correctly excluding ignored IPs.
#    - Calculate 304 cache hit percentage.
#    - Validate proper log line format.
#    - Handle .gz compressed log files.
#    - Skip malformed or empty lines gracefully.
#
#  Version History:
#  v1.0 2025-06-24
#       Initial test implementation.
#
########################################################################

import gzip
import os
import subprocess
import sys
import tempfile
import unittest

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from apache_calculater import ApacheCalculater


class TestApacheCalculater(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.log_path = os.path.join(self.tmp.name, "access.log")

    def tearDown(self):
        self.tmp.cleanup()

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'apache_calculater.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def write_log(self, lines, gzip_mode=False):
        if gzip_mode:
            gz_path = self.log_path + ".gz"
            with gzip.open(gz_path, "wt") as f:
                for line in lines:
                    f.write(line + "\n")
            return gz_path
        else:
            with open(self.log_path, "w") as f:
                for line in lines:
                    f.write(line + "\n")
            return self.log_path

    def test_ip_hit_counting(self):
        log_lines = [
            "1.2.3.4 - - [01/Jan/2025:00:00:00 +0900] \"GET /index.html HTTP/1.1\" 200 1234",
            "5.6.7.8 - - [01/Jan/2025:00:01:00 +0900] \"GET /index.html HTTP/1.1\" 200 1234",
            "1.2.3.4 - - [01/Jan/2025:00:02:00 +0900] \"GET /index.html HTTP/1.1\" 200 1234",
        ]
        path = self.write_log(log_lines)
        result = ApacheCalculater.calculateApacheIpHits(path)
        self.assertEqual(result, [("1.2.3.4", 2), ("5.6.7.8", 1)])

    def test_client_cache_percentage(self):
        log_lines = [
            "1.2.3.4 - - [01/Jan/2025:00:00:00 +0900] \"GET /index.html HTTP/1.1\" 200 1234",
            "5.6.7.8 - - [01/Jan/2025:00:01:00 +0900] \"GET /index.html HTTP/1.1\" 304 0",
            "9.8.7.6 - - [01/Jan/2025:00:02:00 +0900] \"GET /index.html HTTP/1.1\" 304 0",
        ]
        path = self.write_log(log_lines)
        pct = ApacheCalculater.clientCachePercentage(path)
        self.assertEqual(round(pct, 2), 66.67)

    def test_is_valid_log_format(self):
        valid = "192.168.1.1 - - [01/Jan/2025:00:00:00 +0900] \"GET / HTTP/1.1\" 200 100"
        invalid = "garbage line with no IP"
        self.assertTrue(ApacheCalculater.isValidLogFormat(valid))
        self.assertFalse(ApacheCalculater.isValidLogFormat(invalid))

    def test_gzipped_log_support(self):
        log_lines = [
            '1.1.1.1 - - [01/Jan/2025:00:00:00 +0900] "GET /a HTTP/1.1" 304 0',
            '2.2.2.2 - - [01/Jan/2025:00:00:01 +0900] "GET /b HTTP/1.1" 200 123',
        ]
        path = self.write_log(log_lines, gzip_mode=True)
        hits = ApacheCalculater.calculateApacheIpHits(path)
        self.assertEqual(sorted(hits), sorted([("2.2.2.2", 1), ("1.1.1.1", 1)]))
        pct = ApacheCalculater.clientCachePercentage(path)
        self.assertEqual(round(pct, 2), 50.0)

    def test_skip_ignored_ips(self):
        log_lines = [
            "127.0.0.1 - - [01/Jan/2025] \"GET / HTTP/1.1\" 200 123",  # should be ignored
            "8.8.8.8 - - [01/Jan/2025] \"GET / HTTP/1.1\" 200 123",
        ]
        path = self.write_log(log_lines)
        result = ApacheCalculater.calculateApacheIpHits(path)
        self.assertEqual(result, [("8.8.8.8", 1)])


if __name__ == '__main__':
    unittest.main()
