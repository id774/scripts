#!/usr/bin/env python

########################################################################
# apache_blog_analysis.py: WordPress Blog Entry View Refinement Tool
#
#  Description:
#  This script analyzes Apache combined-format access logs (SSL access
#  logs) to refine simple "HTTP 200 on an article URL" counting into
#  three distinct, non-interchangeable metrics:
#
#    - Candidate page views:  GET + HTTP 200 on an article URL, after
#                             excluding empty and bot User-Agent strings.
#    - Asset-confirmed views: Candidate page views for which a WordPress
#                             theme/plugin/upload/core asset request from
#                             the same IP address and User-Agent, with a
#                             Referer pointing back at the article, was
#                             observed within a short time window.
#    - Estimated sessions:    Candidate page views for the same IP
#                             address, User-Agent and article URL,
#                             collapsed into one session within an idle
#                             timeout.
#
#  Asset requests confirm rendering only and are never added to any
#  page-view count. Browser caching, Service Workers, CDNs,
#  Referrer-Policy restrictions and aborted page loads all suppress them,
#  so the asset-confirmed count is a lower bound. None of the three
#  metrics is an exact human visitor count.
#
#  This script is standalone, following the same convention as
#  apache_calculater.py: it performs its own log reading (including gzip
#  support), apache_ignore.list lookup, and bot User-Agent and
#  article/asset pattern matching. It neither calls nor is called by
#  apache_log_analysis.sh or apache_calculater.py. All three are deployed
#  to /etc/cron.exec by installer/install_apache_log_analysis.sh and
#  invoked independently by cron/bin/apache_log_analysis.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      apache_blog_analysis.py <log_file> [<log_file> ...]
#
#  Example:
#      apache_blog_analysis.py /var/log/apache2/ssl_access.log
#      apache_blog_analysis.py /var/log/apache2/ssl_access.log /var/log/apache2/ssl_access.log.1.gz
#
#  Note:
#      Only explicitly specified log files are processed.
#      Rotated logs are not included automatically; specify them explicitly if needed.
#
#  Output:
#      Exactly three sections are printed to stdout, each a
#      "[Section Name]" header followed by "COUNT PATH" lines, one per
#      article URL, sorted by publish date then entry id, both descending:
#
#          [Blog Entry Access]                    candidate page views
#          [Blog Entry Access (Asset Confirmed)]  asset-confirmed subset
#          [Blog Entry Sessions (Estimated)]      estimated sessions
#
#      The "[Blog Entry Metrics: ...]" line preceding them in the cron
#      joblog is printed by cron/bin/apache_log_analysis, not by this
#      script, and records only which log files that run was given.
#
#  The script ignores IPs listed in apache_ignore.list, searched relative
#  to this script's own directory, then /etc/cron.config (search order:
#  <script_dir>/etc/, <script_dir>/../etc/, /etc/cron.config).
#
#  Requirements:
#  - Python Version: 3.2 or later
#
#  Version History:
#  v1.0 2026-07-26
#       Initial release. A standalone companion to apache_log_analysis.sh
#       (deployed and invoked independently, not called by it) that
#       correlates article requests with WordPress asset requests by IP,
#       User-Agent, Referer and timestamp to report candidate page views,
#       asset-confirmed page views, and estimated sessions.
#
########################################################################

import gzip
import os
import re
import sys
from collections import Counter, defaultdict
from datetime import datetime, timedelta, timezone
from urllib.parse import urlsplit

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------

# WordPress paths treated as display-relevant static assets.
WORDPRESS_ASSET_PATH_RE = re.compile(
    r'^/(wp-content/(themes|plugins|uploads)/|wp-includes/)')

# Extensions treated as display-relevant static assets (matched after the
# query string has already been stripped from the request path).
WORDPRESS_ASSET_EXTENSION_RE = re.compile(
    r'[.](css|js|map|woff2?|ttf|otf|eot|png|jpe?g|gif|svg|webp|ico|avif)$',
    re.IGNORECASE)

# Paths that must never be treated as a display-confirming asset, even if
# they happen to fall under a WordPress path prefix above.
WORDPRESS_ASSET_EXCLUDE_PATH_RE = re.compile(
    r'^/(wp-cron\.php|wp-json/|xmlrpc\.php|wp-admin/admin-ajax\.php)')

# Trailing "/YYYY/MM/DD/NNNN/" article path, with any leading path allowed.
ARTICLE_TAIL_RE = re.compile(r'(/[0-9]{4}/[0-9]{2}/[0-9]{2}/[0-9]+/)$')

# Exclude likely automated clients (case-insensitive). Intentionally
# maintained independently of the BLOG_BOT_UA_RE constant in
# apache_log_analysis.sh: the two scripts do not share configuration.
BOT_UA_RE = (
    r'(bot|spider|crawl|slurp|archiver|fetch|scanner|monitor|'
    r'googlebot|bingbot|duckduckbot|baiduspider|yandexbot|'
    r'ahrefsbot|semrushbot|mj12bot|dotbot|'
    r'facebookexternalhit|twitterbot|slackbot|'
    r'curl|wget|python-requests|go-http-client)'
)

# Asset requests are only accepted as confirmation when they fall within
# [article_time - BEFORE, article_time + AFTER] seconds.
ASSET_CONFIRM_BEFORE_SECONDS = 5
ASSET_CONFIRM_AFTER_SECONDS = 60

# Candidate page views for the same IP + User-Agent + article URL within
# this many seconds of each other are collapsed into one estimated session.
BLOG_SESSION_TIMEOUT_SECONDS = 1800

IGNORE_DEFAULT_IP = "127.0.0.1"

MONTHS = {
    'Jan': 1, 'Feb': 2, 'Mar': 3, 'Apr': 4, 'May': 5, 'Jun': 6,
    'Jul': 7, 'Aug': 8, 'Sep': 9, 'Oct': 10, 'Nov': 11, 'Dec': 12,
}

# Apache combined log format:
#   IP - - [dd/Mon/yyyy:HH:MM:SS +ZZZZ] "METHOD PATH PROTO" STATUS SIZE "REFERER" "UA"
LOG_LINE_RE = re.compile(
    r'^(?P<ip>\S+)\s+\S+\s+\S+\s+\[(?P<time>[^\]]+)\]\s+'
    r'"(?P<request>[^"]*)"\s+(?P<status>\S+)\s+(?P<size>\S+)\s+'
    r'"(?P<referer>[^"]*)"\s+"(?P<ua>[^"]*)"'
)

APACHE_TIME_RE = re.compile(
    r'^(\d{2})/([A-Za-z]{3})/(\d{4}):(\d{2}):(\d{2}):(\d{2}) ([+-]\d{4})$')


class MutableCandidate(object):
    """ A candidate page view, with a mutable confirmation flag. """

    __slots__ = ('ip', 'ua', 'article_path', 'ts', 'confirmed')

    def __init__(self, ip, ua, article_path, ts):
        self.ip = ip
        self.ua = ua
        self.article_path = article_path
        self.ts = ts
        self.confirmed = False


class Asset(object):
    """ A WordPress asset request usable as a display-confirmation signal. """

    __slots__ = ('ip', 'ua', 'ref_article_path', 'ts')

    def __init__(self, ip, ua, ref_article_path, ts):
        self.ip = ip
        self.ua = ua
        self.ref_article_path = ref_article_path
        self.ts = ts


def usage():
    """ Display the script header as usage information and exit. """
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, 'r', encoding='utf-8') as f:
            for line in f:
                if line.strip().startswith('#' * 10):
                    if not in_header:
                        in_header = True
                        continue
                    else:
                        break
                if in_header and line.startswith('#'):
                    if line.startswith('# '):
                        print(line[2:], end='')
                    else:
                        print(line[1:], end='')
    except Exception as e:
        print("[ERROR] Failed to read usage information: %s" % str(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)


def parse_apache_time(raw):
    """
    Convert an Apache log timestamp ("dd/Mon/yyyy:HH:MM:SS +ZZZZ") into an
    absolute Unix epoch (float seconds), honoring the embedded UTC offset so
    that logs mixing multiple timezones remain comparable. Returns None on
    a malformed timestamp.
    """
    m = APACHE_TIME_RE.match(raw)
    if not m:
        return None
    day, mon, year, hh, mm, ss, tz = m.groups()
    month = MONTHS.get(mon)
    if not month:
        return None
    sign = 1 if tz[0] == '+' else -1
    offset_minutes = sign * (int(tz[1:3]) * 60 + int(tz[3:5]))
    try:
        dt = datetime(int(year), month, int(day), int(hh), int(mm), int(ss),
                      tzinfo=timezone(timedelta(minutes=offset_minutes)))
    except ValueError:
        return None
    return dt.timestamp()


def request_path(raw_request_target):
    """
    Strip query string and fragment from a request-line path or a full
    Referer URL, returning the path component only. Returns "" when the
    input is empty, "-", or otherwise unusable.
    """
    if not raw_request_target or raw_request_target == '-':
        return ''
    try:
        return urlsplit(raw_request_target).path
    except ValueError:
        return ''


def extract_article_path(raw_request_target):
    """
    Extract the canonical "/.../YYYY/MM/DD/NNNN/" article path from a
    request path or Referer URL. Returns None when no such path is found.
    """
    path = request_path(raw_request_target)
    if not path:
        return None
    m = ARTICLE_TAIL_RE.search(path)
    if not m:
        return None
    return m.group(1)


def is_wordpress_asset_path(path):
    """ Determine whether a (query-stripped) path is a WordPress display asset. """
    if not path:
        return False
    if WORDPRESS_ASSET_EXCLUDE_PATH_RE.match(path):
        return False
    if not WORDPRESS_ASSET_PATH_RE.match(path):
        return False
    return WORDPRESS_ASSET_EXTENSION_RE.search(path) is not None


def parse_line(line, bot_re):
    """
    Parse one combined-log line into (candidate_or_None, asset_or_None).
    """
    m = LOG_LINE_RE.match(line)
    if not m:
        return None, None

    ts = parse_apache_time(m.group('time'))
    if ts is None:
        return None, None

    request = m.group('request')
    tokens = request.split()
    if len(tokens) < 2:
        return None, None
    method, raw_path = tokens[0], tokens[1]

    status = m.group('status')
    ip = m.group('ip')
    ua = m.group('ua')
    referer = m.group('referer')

    candidate = None
    asset = None

    if method == 'GET' and status == '200':
        article_path = extract_article_path(raw_path)
        if article_path and ua and ua != '-' and not bot_re.search(ua):
            candidate = MutableCandidate(ip=ip, ua=ua, article_path=article_path, ts=ts)

    if method == 'GET' and status in ('200', '304'):
        path_only = request_path(raw_path)
        if is_wordpress_asset_path(path_only):
            ref_article_path = extract_article_path(referer)
            if ref_article_path:
                asset = Asset(ip=ip, ua=ua, ref_article_path=ref_article_path, ts=ts)

    return candidate, asset


def open_log_lines(path):
    """ Yield decoded lines from a plain or gzip-compressed log file. """
    if path.endswith('.gz'):
        return gzip.open(path, 'rt', encoding='utf-8', errors='replace')
    return open(path, 'rt', encoding='utf-8', errors='replace')


def load_ignore_list():
    """
    Load IP addresses to ignore, searching this script's own directory,
    its parent directory, then /etc/cron.config, matching
    apache_log_analysis.sh's dirname "$0"-based resolution so both tools
    agree on which apache_ignore.list to use regardless of the caller's
    current working directory. Falls back to 127.0.0.1 only.
    """
    ignore_ips = set([IGNORE_DEFAULT_IP])

    script_dir = os.path.dirname(os.path.abspath(__file__))
    candidate_paths = [
        os.path.join(script_dir, "etc", "apache_ignore.list"),
        os.path.join(script_dir, "..", "etc", "apache_ignore.list"),
        "/etc/cron.config/apache_ignore.list",
    ]

    for ignore_file in candidate_paths:
        if os.path.isfile(ignore_file):
            try:
                with open(ignore_file, "r", encoding="utf-8") as f:
                    for line in f:
                        raw = line.split("#", 1)[0].strip()
                        if not raw:
                            continue
                        ip = raw.split()[0]
                        if ip:
                            ignore_ips.add(ip)
                break
            except Exception:
                continue

    return ignore_ips


def collect_candidates_and_assets(log_files, ignore_ips, bot_re):
    """ Stream all log files once, returning (candidates, assets) lists. """
    candidates = []
    assets = []

    for log_file in log_files:
        with open_log_lines(log_file) as f:
            for line in f:
                ip = line.split(' ', 1)[0]
                if ip in ignore_ips:
                    continue
                candidate, asset = parse_line(line, bot_re)
                if candidate is not None:
                    candidates.append(candidate)
                if asset is not None:
                    assets.append(asset)

    return candidates, assets


def correlate_assets(candidates, assets, before_seconds, after_seconds):
    """
    Mark each candidate as asset-confirmed when a matching WordPress asset
    request (same IP, same User-Agent, Referer pointing at the same
    article) falls within the allowed time window. Each asset is assigned
    to at most one candidate: the one with the smallest absolute time
    difference, preferring a candidate at or before the asset time on ties.
    """
    buckets = defaultdict(list)
    for c in candidates:
        buckets[(c.ip, c.ua, c.article_path)].append(c)

    for a in assets:
        bucket = buckets.get((a.ip, a.ua, a.ref_article_path))
        if not bucket:
            continue

        best = None
        best_diff = None
        for c in bucket:
            diff = a.ts - c.ts
            if diff < -before_seconds or diff > after_seconds:
                continue
            if best is None:
                best, best_diff = c, diff
                continue
            if abs(diff) < abs(best_diff):
                best, best_diff = c, diff
            elif abs(diff) == abs(best_diff) and diff >= 0 and best_diff < 0:
                best, best_diff = c, diff

        if best is not None:
            best.confirmed = True


def compute_session_counts(candidates, timeout_seconds):
    """
    Collapse candidate page views sharing the same IP, User-Agent and
    article URL into estimated sessions using a sliding idle timeout, and
    return per-article session counts.
    """
    buckets = defaultdict(list)
    for c in candidates:
        buckets[(c.ip, c.ua, c.article_path)].append(c)

    session_counts = Counter()
    for (ip, ua, article_path), group in buckets.items():
        group.sort(key=lambda c: c.ts)
        last_ts = None
        for c in group:
            if last_ts is None or (c.ts - last_ts) > timeout_seconds:
                session_counts[article_path] += 1
            last_ts = c.ts

    return session_counts


def article_sort_key(article_path):
    """ Sort key: date (YYYYMMDD) descending, then entry id descending. """
    segments = [s for s in article_path.split('/') if s]
    year, month, day, entry_id = segments[-4], segments[-3], segments[-2], segments[-1]
    return (year + month + day, int(entry_id))


def print_section(title, counts):
    """ Print one "[Title]" section with "count path" lines. """
    print("[{0}]".format(title))
    for path in sorted(counts.keys(), key=article_sort_key, reverse=True):
        print("{0} {1}".format(counts[path], path))


def validate_log_files(log_files):
    for log_file in log_files:
        if not os.path.isfile(log_file):
            print("[ERROR] Log file not found at {0}.".format(log_file), file=sys.stderr)
            sys.exit(1)


def run(log_files):
    ignore_ips = load_ignore_list()
    bot_re = re.compile(BOT_UA_RE, re.IGNORECASE)

    candidates, assets = collect_candidates_and_assets(log_files, ignore_ips, bot_re)
    correlate_assets(candidates, assets, ASSET_CONFIRM_BEFORE_SECONDS, ASSET_CONFIRM_AFTER_SECONDS)

    candidate_counts = Counter(c.article_path for c in candidates)
    confirmed_counts = Counter(c.article_path for c in candidates if c.confirmed)
    session_counts = compute_session_counts(candidates, BLOG_SESSION_TIMEOUT_SECONDS)

    print_section("Blog Entry Access", candidate_counts)
    print_section("Blog Entry Access (Asset Confirmed)", confirmed_counts)
    print_section("Blog Entry Sessions (Estimated)", session_counts)


def main():
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    log_files = sys.argv[1:]
    validate_log_files(log_files)
    run(log_files)
    return 0


if __name__ == '__main__':
    sys.exit(main())
