#!/usr/bin/env python

########################################################################
# apache_calculater_test.py: Unit tests for apache_calculater.py
#
#  Description:
#  This script tests the functionality of ApacheCalculater, including hit
#  counting, cache percentage calculation, log format validation, and
#  human-readable output formatting. Tests are deterministic and do not
#  depend on external ignore list files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Count IP hits correctly excluding ignored IPs.
#    - Calculate percent safely when total is zero.
#    - Calculate 304 cache hit percentage split into static vs non-static requests.
#    - Calculate raw cache counters split into static vs non-static requests.
#    - Validate proper Apache log line format.
#    - Handle .gz compressed log files.
#    - Aggregate IP hits and cache counters across multiple log files.
#    - Print aggregated results when processing multiple log files in one invocation.
#    - Skip ignored IPs during hit counting.
#    - Skip malformed or empty log lines gracefully.
#    - Format IP hit output as one IP per line with a header for human-readable display.
#    - Print a clear "(no hits)" message when IP hit list is empty.
#
#  Version History:
#  v1.3 2026-01-09
#       Add tests for multi-log aggregation and run() output.
#       Add tests for clientCacheCounts() and calculatePercent().
#  v1.2 2026-01-07
#       Add tests for human-readable IP hit output formatting.
#       Stabilize tests by isolating ignore list behavior and using sys.executable.
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

from apache_calculater import (ApacheCalculater, aggregateLogs,
                               calculatePercent, printIpHits, run)


class TestApacheCalculater(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.log_path = os.path.join(self.tmp.name, "access.log")
        self._orig_load_ignore_list = ApacheCalculater.loadIgnoreList
        ApacheCalculater.loadIgnoreList = staticmethod(lambda: set(["127.0.0.1"]))

    def tearDown(self):
        ApacheCalculater.loadIgnoreList = self._orig_load_ignore_list
        self.tmp.cleanup()

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'apache_calculater.py')

        proc = subprocess.Popen([sys.executable, script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def write_log(self, lines, gzip_mode=False):
        if gzip_mode:
            gz_path = self.log_path + ".gz"
            with gzip.open(gz_path, "wt", encoding="utf-8") as f:
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

    def test_calculate_percent_handles_zero_total(self):
        self.assertEqual(calculatePercent(0, 0), 0)
        self.assertEqual(calculatePercent(1, 0), 0)
        self.assertEqual(round(calculatePercent(1, 2), 2), 50.0)
        self.assertEqual(round(calculatePercent(2, 4), 2), 50.0)

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

    def test_client_cache_counts(self):
        log_lines = [
            "1.2.3.4 - - [01/Jan/2025:00:00:00 +0900] \"GET /index.html HTTP/1.1\" 200 1234",
            "5.6.7.8 - - [01/Jan/2025:00:01:00 +0900] \"GET /index.html HTTP/1.1\" 304 0",
            "9.8.7.6 - - [01/Jan/2025:00:02:00 +0900] \"GET /style.css HTTP/1.1\" 304 0",
            "9.8.7.6 - - [01/Jan/2025:00:03:00 +0900] \"GET /style.css HTTP/1.1\" 200 10",
        ]
        path = self.write_log(log_lines)
        ts, cs, tn, cn = ApacheCalculater.clientCacheCounts(path)
        self.assertEqual((ts, cs, tn, cn), (2, 1, 2, 1))

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

    def test_aggregate_logs_multiple_files(self):
        log1 = [
            '1.1.1.1 - - [01/Jan/2025:00:00:00 +0900] "GET /index.html HTTP/1.1" 200 1',
            '1.1.1.1 - - [01/Jan/2025:00:00:01 +0900] "GET /index.html HTTP/1.1" 304 0',
            '2.2.2.2 - - [01/Jan/2025:00:00:02 +0900] "GET /style.css HTTP/1.1" 304 0',
        ]
        log2 = [
            '2.2.2.2 - - [01/Jan/2025:00:00:03 +0900] "GET /index.html HTTP/1.1" 200 1',
            '3.3.3.3 - - [01/Jan/2025:00:00:04 +0900] "GET /style.css HTTP/1.1" 200 1',
        ]

        p1 = os.path.join(self.tmp.name, "a.log")
        p2 = os.path.join(self.tmp.name, "b.log.gz")

        with open(p1, "w") as f:
            for line in log1:
                f.write(line + "\n")
        with gzip.open(p2, "wt", encoding="utf-8") as f:
            for line in log2:
                f.write(line + "\n")

        ip_hit_listing, ts, cs, tn, cn = aggregateLogs([p1, p2])

        # IP hits aggregated
        self.assertEqual(ip_hit_listing.get("1.1.1.1"), 2)
        self.assertEqual(ip_hit_listing.get("2.2.2.2"), 2)
        self.assertEqual(ip_hit_listing.get("3.3.3.3"), 1)

        # Cache counters aggregated
        # Static: style.css appears twice total (one 304, one 200) => total 2, cached 1
        # Non-static: index.html appears three times total (one 304, two 200) => total 3, cached 1
        self.assertEqual((ts, cs, tn, cn), (2, 1, 3, 1))

    def test_run_prints_aggregated_results(self):
        log1 = [
            '1.1.1.1 - - [01/Jan/2025:00:00:00 +0900] "GET /index.html HTTP/1.1" 200 1',
            '1.1.1.1 - - [01/Jan/2025:00:00:01 +0900] "GET /index.html HTTP/1.1" 304 0',
        ]
        log2 = [
            '2.2.2.2 - - [01/Jan/2025:00:00:02 +0900] "GET /style.css HTTP/1.1" 304 0',
        ]

        p1 = os.path.join(self.tmp.name, "c.log")
        p2 = os.path.join(self.tmp.name, "d.log")
        with open(p1, "w") as f:
            for line in log1:
                f.write(line + "\n")
        with open(p2, "w") as f:
            for line in log2:
                f.write(line + "\n")

        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            run([p1, p2])
        out = buf.getvalue()
        self.assertIn("[INFO] IP Hits:", out)
        self.assertIn("1.1.1.1", out)
        self.assertIn("2.2.2.2", out)
        self.assertIn("[INFO] Static Asset Cache Percentage:", out)
        self.assertIn("[INFO] Non-static Cache Percentage:", out)

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
