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
#    - Format IP hit output as one IP per line for human-readable display.
#    - Calculate 304 cache hit percentage split into static vs non-static requests.
#    - Validate proper log line format.
#    - Handle .gz compressed log files.
#    - Skip malformed or empty lines gracefully.
#
#  Version History:
#  v1.2 2026-01-07
#       Add tests for human-readable IP hit output formatting.
#  v1.1 2025-12-27
#       Split client cache percentage into static vs non-static by excluding static assets from page-like metrics.
#  v1.0 2025-06-24
#       Initial test implementation.
#
########################################################################

import contextlib
import gzip
import io
import os
import subprocess
import sys
import tempfile
import unittest

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from apache_calculater import ApacheCalculater, printIpHits


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

    def test_client_cache_percentage_split(self):
        log_lines = [
            "1.2.3.4 - - [01/Jan/2025:00:00:00 +0900] \"GET /index.html HTTP/1.1\" 200 1234",
            "5.6.7.8 - - [01/Jan/2025:00:01:00 +0900] \"GET /index.html HTTP/1.1\" 304 0",
            "9.8.7.6 - - [01/Jan/2025:00:02:00 +0900] \"GET /style.css HTTP/1.1\" 304 0",
        ]
        path = self.write_log(log_lines)
        static_pct, nonstatic_pct = ApacheCalculater.clientCachePercentageSplit(path)
        self.assertEqual(round(static_pct, 2), 100.0)
        self.assertEqual(round(nonstatic_pct, 2), 50.0)

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
        static_pct, nonstatic_pct = ApacheCalculater.clientCachePercentageSplit(path)
        self.assertEqual(round(static_pct, 2), 0.0)
        self.assertEqual(round(nonstatic_pct, 2), 50.0)

    def test_skip_ignored_ips(self):
        log_lines = [
            "127.0.0.1 - - [01/Jan/2025] \"GET / HTTP/1.1\" 200 123",  # should be ignored
            "8.8.8.8 - - [01/Jan/2025] \"GET / HTTP/1.1\" 200 123",
        ]
        path = self.write_log(log_lines)
        result = ApacheCalculater.calculateApacheIpHits(path)
        self.assertEqual(result, [("8.8.8.8", 1)])

    def test_skip_malformed_or_empty_lines(self):
        log_lines = [
            "",  # empty
            "garbage line with no IP",  # malformed
            "999.999.999.999 - - [01/Jan/2025:00:00:00 +0900] \"GET / HTTP/1.1\" 200 100",  # invalid IPv4
            "1.2.3.4 - - [01/Jan/2025:00:00:00 +0900] \"GET / HTTP/1.1\" 200 100",  # valid
        ]
        path = self.write_log(log_lines)
        hits = ApacheCalculater.calculateApacheIpHits(path)
        self.assertEqual(hits, [("1.2.3.4", 1)])

    def test_print_ip_hits_formats_one_entry_per_line(self):
        ip_hits = [("1.2.3.4", 2), ("5.6.7.8", 1)]

        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            printIpHits(ip_hits)

        out = buf.getvalue().splitlines()

        # Header line first
        self.assertGreaterEqual(len(out), 3)
        self.assertEqual(out[0], "[INFO] IP Hits:")

        # One IP per line, IP and hits should appear on the same line.
        self.assertIn("1.2.3.4", out[1])
        self.assertTrue(out[1].rstrip().endswith("2"))
        self.assertIn("5.6.7.8", out[2])
        self.assertTrue(out[2].rstrip().endswith("1"))

    def test_print_ip_hits_handles_empty(self):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            printIpHits([])
        out = buf.getvalue().strip()
        self.assertEqual(out, "[INFO] IP Hits: (no hits)")


if __name__ == '__main__':
    unittest.main()
