#!/usr/bin/env python

########################################################################
# apache_calculater.py: Apache Log File Analysis Tool
#
#  Description:
#  This script analyzes Apache log files to calculate the number of hits per IP
#  address and the percentage of client cache hits. It supports .gz compressed
#  log files and can process multiple log files in a single invocation.
#  When multiple log files are specified, IP hit counts and cache statistics
#  are aggregated across all provided logs before reporting results.
#
#  log files and excludes IPs listed in apache_ignore.list, which is searched
#  in ./etc/, ../etc/ and /etc/cron.config in that order.
#  It is designed to provide insights into web server traffic and client behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      apache_calculater.py <log_file> [<log_file> ...]
#
#  The script ignores IPs listed in /etc/cron.config/apache_ignore.list
#  if the file exists. You can customize it as needed.
#
#  Example:
#      apache_calculater.py /var/log/apache2/access.log
#      apache_calculater.py /var/log/apache2/access.log /var/log/apache2/access.log.1.gz
#      apache_calculater.py /var/log/apache2/ssl_access.log*
#
#  Multiple log files are treated as a single logical dataset.
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Version History:
#  v2.2 2026-01-09
#       Allow multiple Apache log files to be specified and processed together.
#       Aggregate IP hit counts and cache metrics across all input logs.
#  v2.1 2026-01-07
#       Format IP hit results for human-readable output (one IP per line).
#  v2.0 2026-01-06
#       Fix ignore list lookup order description, unify common filtering/parsing,
#       robustly extract status from combined log, and strengthen IPv4 validation.
#  v1.9 2025-12-27
#       Split client cache percentage into static vs non-static by excluding static assets from page-like metrics.
#  v1.8 2025-07-30
#       Support ignore list lookup in /etc/cron.config first, before falling back to local etc/ paths.
#  v1.7 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.4 2023-12-25
#       Added error handling for non-existent log files.
#       Added log format validation and refactored file opening logic.
#  v1.3 2023-12-17
#       Enhanced the logic for loading the ignore list by searching
#       in both the current directory's etc folder and the script's
#       relative parent directory's etc folder.
#  v1.2 2023-12-14
#       Added support for .gz log files and implemented IP ignore list.
#  v1.1 2023-12-06
#       Refactored for improved clarity and removed unnecessary path manipulation.
#  v1.0 2011-04-20
#       Initial release.
#
########################################################################

import gzip
import os
import re
import sys

EXCLUDE_PATH_RE = re.compile(r'\.(css|js|map|woff2?|ttf|otf|eot|png|jpe?g|gif|svg|webp|ico|avif)(\?.*)?$', re.IGNORECASE)


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
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)

class ApacheCalculater(object):

    @staticmethod
    def extractRequestPath(line):
        """
        Extract request path from a typical Apache combined log line.
        Example: "GET /path HTTP/1.1" -> "/path"
        Returns empty string on failure.
        """
        parts = line.split('"')
        if len(parts) < 2:
            return ""
        req = parts[1]
        tokens = req.split()
        if len(tokens) < 2:
            return ""
        return tokens[1]

    @staticmethod
    def extractStatusCode(line):
        """
        Extract HTTP status code from a typical Apache combined log line.
        Returns empty string on failure.
        """
        parts = line.split('"')
        if len(parts) < 3:
            return ""
        tail = parts[2].lstrip()
        if not tail:
            return ""
        tokens = tail.split()
        if not tokens:
            return ""
        return tokens[0]

    @staticmethod
    def isValidIPv4(ip):
        """
        Validate IPv4 address without external dependencies (Python 3.1 compatible).
        """
        if not ip:
            return False
        parts = ip.split(".")
        if len(parts) != 4:
            return False
        for p in parts:
            if p == "" or not p.isdigit():
                return False
            # Avoid octet overflow; allow leading zeros
            n = int(p, 10)
            if n < 0 or n > 255:
                return False
        return True

    @classmethod
    def isStaticAssetRequest(cls, line):
        """
        Determine whether the log line is for a static asset request.
        Matching is based on request path extension.
        """
        path = cls.extractRequestPath(line)
        if not path:
            return False
        return EXCLUDE_PATH_RE.search(path) is not None

    @classmethod
    def iterFilteredLines(cls, log, ignore_ips):
        """
        Yield log lines after applying common filters (ignore IP list).
        """
        with cls.openLogFile(log) as contents:
            for line in contents:
                ip = line.split(" ", 1)[0]
                if ip in ignore_ips:
                    continue
                yield line

    @staticmethod
    def loadIgnoreList():
        """
        Load the ignore list from the first available location.
        Use localhost as default if the list is empty or file is not found.

        Search order:
        ./etc/ -> ../etc/ -> /etc/cron.config

        Returns:
            set: A set of IPs to ignore.
        """
        ignore_ips = set({"127.0.0.1"})  # Default value

        # Search in this order: ./etc/, ../etc/, /etc/cron.config
        candidate_paths = [
            os.path.join(os.getcwd(), "etc", "apache_ignore.list"),
            os.path.join(os.path.dirname(__file__), "..", "etc", "apache_ignore.list"),
            "/etc/cron.config/apache_ignore.list"
        ]

        for ignore_file in candidate_paths:
            if os.path.isfile(ignore_file):
                try:
                    with open(ignore_file, "r") as file:
                        for line in file:
                            raw = line.split("#", 1)[0].strip()
                            if not raw:
                                continue
                            ip = raw.split()[0]
                            if ip:
                                ignore_ips.add(ip)
                    break
                except Exception:
                    continue  # Skip unreadable files

        return ignore_ips

    @classmethod
    def openLogFile(cls, log):
        """
        Open an Apache log file (regular or gzipped).

        Args:
            log (str): Path to the Apache log file.

        Returns:
            file object: Opened file object.
        """
        if log.endswith(".gz"):
            return gzip.open(log, "rt", encoding="utf-8", errors="replace")
        return open(log, "rt", encoding="utf-8", errors="replace")

    @classmethod
    def calculateApacheIpHits(cls, log):
        """
        Calculate the number of hits per IP address from an Apache log file.

        Args:
            log (str): Path to the Apache log file.

        Returns:
            list of tuples: Sorted list of (IP, hits) tuples.
        """
        ipHitListing = {}
        ignore_ips = cls.loadIgnoreList()

        for line in cls.iterFilteredLines(log, ignore_ips):
            ip = line.split(" ", 1)[0]
            if cls.isValidIPv4(ip):
                ipHitListing[ip] = ipHitListing.get(ip, 0) + 1

        return sorted(ipHitListing.items(), reverse=True, key=lambda x: x[1])

    @classmethod
    def clientCachePercentageSplit(cls, log):
        """
        Calculate cache percentage split into static and non-static requests.

        Args:
            log (str): Path to the Apache log file.

        Returns:
            tuple: (static_percent, non_static_percent)
        """
        ignore_ips = cls.loadIgnoreList()
        total_static, cached_static = 0, 0
        total_nonstatic, cached_nonstatic = 0, 0

        for line in cls.iterFilteredLines(log, ignore_ips):
            status = cls.extractStatusCode(line)
            if not status:
                continue
            is_cached = (status == "304")

            if cls.isStaticAssetRequest(line):
                total_static += 1
                if is_cached:
                    cached_static += 1
            else:
                total_nonstatic += 1
                if is_cached:
                    cached_nonstatic += 1

        static_percent = float(100 * cached_static) / total_static if total_static > 0 else 0
        nonstatic_percent = float(100 * cached_nonstatic) / total_nonstatic if total_nonstatic > 0 else 0
        return static_percent, nonstatic_percent

    @classmethod
    def clientCacheCounts(cls, log):
        """
        Return raw cache counters split into static and non-static requests.

        Returns:
            tuple: (total_static, cached_static, total_nonstatic, cached_nonstatic)
        """
        ignore_ips = cls.loadIgnoreList()
        total_static, cached_static = 0, 0
        total_nonstatic, cached_nonstatic = 0, 0

        for line in cls.iterFilteredLines(log, ignore_ips):
            status = cls.extractStatusCode(line)
            if not status:
                continue
            is_cached = (status == "304")

            if cls.isStaticAssetRequest(line):
                total_static += 1
                if is_cached:
                    cached_static += 1
            else:
                total_nonstatic += 1
                if is_cached:
                    cached_nonstatic += 1

        return total_static, cached_static, total_nonstatic, cached_nonstatic

    @classmethod
    def isValidLogFormat(cls, line):
        """
        Check if a log line follows a basic Apache log format.

        Args:
            line (str): A line from the Apache log file.

        Returns:
            bool: True if the line follows the basic format, False otherwise.
        """
        ip = line.split(" ", 1)[0]
        if not cls.isValidIPv4(ip):
            return False
        # Basic continuation check: IP followed by at least one space and some more text
        pattern = r'^\S+\s+\S+'
        return re.match(pattern, line) is not None

def printIpHits(ip_hits):
    """
    Print IP hit counts in a human-readable, one-entry-per-line format.
    """
    if not ip_hits:
        print("[INFO] IP Hits: (no hits)")
        return

    print("[INFO] IP Hits:")

    max_ip_len = 0
    for ip, hits in ip_hits:
        if len(ip) > max_ip_len:
            max_ip_len = len(ip)

    for ip, hits in ip_hits:
        print("{0} {1}".format(ip.ljust(max_ip_len), hits))


def validateLogFiles(log_files):
    """
    Validate existence and basic format of all specified log files.
    """
    for log_file in log_files:
        if not os.path.exists(log_file):
            print("[ERROR] Log file does not exist - {0}".format(log_file), file=sys.stderr)
            sys.exit(2)

        with ApacheCalculater.openLogFile(log_file) as contents:
            for line in contents:
                if not ApacheCalculater.isValidLogFormat(line):
                    print("[ERROR] Invalid log format detected in file - {0}".format(log_file), file=sys.stderr)
                    sys.exit(3)
                break

def aggregateLogs(log_files):
    """
    Aggregate IP hit counts and cache statistics across multiple log files.
    """
    ip_hit_listing = {}
    total_static, cached_static = 0, 0
    total_nonstatic, cached_nonstatic = 0, 0

    for log_file in log_files:
        for ip, hits in ApacheCalculater.calculateApacheIpHits(log_file):
            ip_hit_listing[ip] = ip_hit_listing.get(ip, 0) + hits

        ts, cs, tn, cn = ApacheCalculater.clientCacheCounts(log_file)
        total_static += ts
        cached_static += cs
        total_nonstatic += tn
        cached_nonstatic += cn

    return ip_hit_listing, total_static, cached_static, total_nonstatic, cached_nonstatic


def main():
    if len(sys.argv) < 2:
        usage()

    log_files = sys.argv[1:]

    validateLogFiles(log_files)

    ip_hit_listing, total_static, cached_static, total_nonstatic, cached_nonstatic = \
        aggregateLogs(log_files)

    ip_hits_sorted = sorted(ip_hit_listing.items(), reverse=True, key=lambda x: x[1])
    printIpHits(ip_hits_sorted)

    static_pct = float(100 * cached_static) / total_static if total_static > 0 else 0
    nonstatic_pct = float(100 * cached_nonstatic) / total_nonstatic if total_nonstatic > 0 else 0
    print("[INFO] Static Asset Cache Percentage:", static_pct)
    print("[INFO] Non-static Cache Percentage:", nonstatic_pct)

    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main())
