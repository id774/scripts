#!/usr/bin/env python

########################################################################
# apache_blog_analysis_test.py: Unit tests for apache_blog_analysis.py
#
#  Description:
#  Tests candidate/asset-confirmed page-view extraction, IP/User-Agent/
#  Referer/time-window correlation, estimated session aggregation, and
#  output formatting for apache_blog_analysis.py. Test cases follow the
#  scenarios enumerated in the "Apache access log blog view refinement"
#  requirements document (section 14.1, TC-001..TC-020).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2026-07-26
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

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import apache_blog_analysis as blog


class TestApacheBlogAnalysis(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self._orig_load_ignore_list = blog.load_ignore_list
        blog.load_ignore_list = lambda: set(["127.0.0.1"])
        self._orig_env_bot_re = os.environ.pop('BLOG_BOT_UA_RE', None)

    def tearDown(self):
        blog.load_ignore_list = self._orig_load_ignore_list
        if self._orig_env_bot_re is not None:
            os.environ['BLOG_BOT_UA_RE'] = self._orig_env_bot_re
        self.tmp.cleanup()

    def write_log(self, lines, name="access.log", gzip_mode=False):
        if gzip_mode:
            path = os.path.join(self.tmp.name, name + ".gz")
            with gzip.open(path, "wt", encoding="utf-8") as f:
                for line in lines:
                    f.write(line + "\n")
            return path
        path = os.path.join(self.tmp.name, name)
        with open(path, "w") as f:
            for line in lines:
                f.write(line + "\n")
        return path

    def run_and_capture(self, log_files):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            blog.run(log_files)
        return buf.getvalue()

    def counts_for_section(self, output, title):
        lines = output.splitlines()
        header = "[{0}]".format(title)
        start = lines.index(header) + 1
        counts = {}
        for line in lines[start:]:
            if line.startswith("["):
                break
            n, path = line.split(" ", 1)
            counts[path] = int(n)
        return counts

    # -- TC-001: normal browser view (article + matching CSS) --------------
    def test_normal_browser_view_confirms(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {"/2026/07/26/5128/": 1})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {"/2026/07/26/5128/": 1})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Sessions (Estimated)"), {"/2026/07/26/5128/": 1})

    # -- TC-002: asset returned 304 still confirms --------------------------
    def test_asset_304_confirms(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 304 0 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {"/2026/07/26/5128/": 1})

    # -- TC-003: referer points at a different article -----------------------
    def test_referer_mismatch_does_not_confirm(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/01/01/9999/" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})

    # -- TC-004: User-Agent mismatch ------------------------------------------
    def test_user_agent_mismatch_does_not_confirm(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0 (A)"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0 (B)"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})

    # -- TC-005: IP address mismatch ------------------------------------------
    def test_ip_mismatch_does_not_confirm(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.99 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})

    # -- TC-006: asset 61 seconds after the article request -------------------
    def test_asset_after_window_does_not_confirm(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:16:41 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})

    # -- TC-007: asset 3 seconds before the article request --------------------
    def test_asset_just_before_confirms(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {"/2026/07/26/5128/": 1})

    # -- TC-008: asset 6 seconds before exceeds the "before" allowance ---------
    def test_asset_too_far_before_does_not_confirm(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:46 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})

    # -- TC-009: multiple matching assets still count as one confirmed view ---
    def test_multiple_assets_count_as_single_confirmed_view(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:41 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:42 +0900] "GET /wp-includes/js/jquery.js HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/uploads/2026/07/a.png HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {"/2026/07/26/5128/": 1})

    # -- TC-010: bot User-Agent excluded from all three metrics ----------------
    def test_bot_user_agent_excluded(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Googlebot/2.1"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Googlebot/2.1"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Sessions (Estimated)"), {})

    # -- TC-011: HEAD request excluded ------------------------------------------
    def test_head_request_excluded(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "HEAD /entry/2026/07/26/5128/ HTTP/1.1" 200 0 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {})

    # -- TC-012: article 304 excluded from candidate page views -----------------
    def test_article_304_excluded(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 304 0 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {})

    # -- TC-013: article only, no asset request (browser cache) -----------------
    def test_article_without_asset_is_unconfirmed(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {"/2026/07/26/5128/": 1})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Sessions (Estimated)"), {"/2026/07/26/5128/": 1})

    # -- TC-014: reload 5 minutes apart stays in one session ---------------------
    def test_reload_within_session_timeout_is_one_session(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:00:00 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:05:00 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {"/2026/07/26/5128/": 2})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Sessions (Estimated)"), {"/2026/07/26/5128/": 1})

    # -- TC-015: 31 minutes apart splits into two sessions ------------------------
    def test_gap_beyond_session_timeout_splits_sessions(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:00:00 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:31:01 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {"/2026/07/26/5128/": 2})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Sessions (Estimated)"), {"/2026/07/26/5128/": 2})

    # -- TC-016: different articles are independent sessions -----------------------
    def test_different_articles_are_independent_sessions(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:00:00 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:00:05 +0900] "GET /entry/2026/07/25/5127/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Sessions (Estimated)"),
                          {"/2026/07/26/5128/": 1, "/2026/07/25/5127/": 1})

    # -- TC-017: query string is stripped from the aggregation key ------------------
    def test_query_string_aggregated_into_same_article(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:00:00 +0900] "GET /entry/2026/07/26/5128/?utm_source=a HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:00:05 +0900] "GET /entry/2026/07/26/5128/?utm_source=b HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {"/2026/07/26/5128/": 2})

    # -- TC-018: article and asset split across a plain log and a gzip log -----------
    def test_asset_confirmed_across_gzip_log(self):
        log1 = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ], name="a.log")
        log2 = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
        ], name="b.log", gzip_mode=True)
        out = self.run_and_capture([log1, log2])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {"/2026/07/26/5128/": 1})

    # -- TC-019: asset line appears before the article line in the input -------------
    def test_asset_before_article_in_log_order_still_confirms(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:41 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {"/2026/07/26/5128/": 1})

    # -- TC-020: ignore-listed IPs are excluded from all three metrics ----------------
    def test_ignored_ip_excluded(self):
        blog.load_ignore_list = lambda: set(["127.0.0.1", "203.0.113.10"])
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.10 - - [26/Jul/2026:20:15:43 +0900] "GET /wp-content/themes/x/style.css HTTP/1.1" 200 1200 "https://example.com/entry/2026/07/26/5128/" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access"), {})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Access (Asset Confirmed)"), {})
        self.assertEqual(self.counts_for_section(out, "Blog Entry Sessions (Estimated)"), {})

    # -- Section headers are always printed, even with zero matching entries ----------
    def test_zero_hits_still_prints_section_headers(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:15:40 +0900] "GET /about/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        self.assertIn("[Blog Entry Access]", out)
        self.assertIn("[Blog Entry Access (Asset Confirmed)]", out)
        self.assertIn("[Blog Entry Sessions (Estimated)]", out)

    # -- Sort order: date descending, then entry id descending -------------------------
    def test_sort_order_date_then_id_descending(self):
        log = self.write_log([
            '203.0.113.10 - - [26/Jul/2026:20:00:00 +0900] "GET /entry/2026/07/25/5127/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.11 - - [26/Jul/2026:20:00:00 +0900] "GET /entry/2026/07/26/5128/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
            '203.0.113.12 - - [26/Jul/2026:20:00:00 +0900] "GET /entry/2026/07/26/5001/ HTTP/1.1" 200 5000 "-" "Mozilla/5.0"',
        ])
        out = self.run_and_capture([log])
        lines = out.splitlines()
        start = lines.index("[Blog Entry Access]") + 1
        paths = [lines[start + i].split(" ", 1)[1] for i in range(3)]
        self.assertEqual(paths, ["/2026/07/26/5128/", "/2026/07/26/5001/", "/2026/07/25/5127/"])

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'apache_blog_analysis.py')

        proc = subprocess.Popen([sys.executable, script_path, '-h'],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_missing_log_file_errors(self):
        proc = subprocess.Popen(
            [sys.executable, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                                           'apache_blog_analysis.py'),
             os.path.join(self.tmp.name, 'no-such-file.log')],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = proc.communicate()
        self.assertEqual(proc.returncode, 1)
        self.assertIn(b'[ERROR]', err)


if __name__ == '__main__':
    unittest.main()
