#!/usr/bin/env python

########################################################################
# apache_calculater.py: Apache Log File Analysis Tool
#
#  Description:
#  This script analyzes Apache log files to calculate the number of hits per IP
#  address and the percentage of client cache hits. It supports .gz compressed
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
#      apache_calculater.py <log_file>
#
#  The script ignores IPs listed in /etc/cron.config/apache_ignore.list
#  if the file exists. You can customize it as needed.
#
#  Example:
#      apache_calculater.py /var/log/apache2/access.log
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Version History:
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
    def loadIgnoreList():
        """
        Load the ignore list from the first available location.
        Use localhost as default if the list is empty or file is not found.

        Returns:
            set: A set of IPs to ignore.
        """
        ignore_ips = set({"127.0.0.1"})  # Default value

        # Search in this order: /etc/cron.config, ./etc/, ../etc/
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
                            stripped_line = line.strip()
                            if stripped_line and not stripped_line.startswith("#"):
                                ignore_ips.add(stripped_line)
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
        return gzip.open(log, "rt") if log.endswith(".gz") else open(log, "rt")

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

        with cls.openLogFile(log) as contents:
            for line in contents:
                # Extract the IP address from each log entry
                ip = line.split(" ", 1)[0]
                if ip in ignore_ips:
                    continue
                if 6 < len(ip) <= 15:  # Validate the length of the IP address
                    ipHitListing[ip] = ipHitListing.get(ip, 0) + 1

        return sorted(ipHitListing.items(), reverse=True, key=lambda x: x[1])

    @classmethod
    def clientCachePercentage(cls, log):
        """
        Calculate the percentage of cached client requests.

        Args:
            log (str): Path to the Apache log file.

        Returns:
            float: The percentage of cached requests.
        """
        totalRequests, cachedRequests = 0, 0

        with cls.openLogFile(log) as contents:
            for line in contents:
                totalRequests += 1
                # Check if the response status is 304 (Not Modified)
                if line.split(" ")[8] == "304":
                    cachedRequests += 1

        return float(100 * cachedRequests) / totalRequests if totalRequests > 0 else 0

    @classmethod
    def isValidLogFormat(cls, line):
        """
        Check if a log line follows a basic Apache log format.

        Args:
            line (str): A line from the Apache log file.

        Returns:
            bool: True if the line follows the basic format, False otherwise.
        """
        # Basic pattern: IP followed by at least two spaces and some more text
        pattern = r'^\d{1,3}(\.\d{1,3}){3}\s+\S+'
        return re.match(pattern, line) is not None

def main():
    if len(sys.argv) != 2:
        usage()

    log_file = sys.argv[1]

    if not os.path.exists(log_file):
        print("[ERROR] Log file does not exist - {0}".format(log_file), file=sys.stderr)
        sys.exit(2)

    # Check for valid log format
    with ApacheCalculater.openLogFile(log_file) as contents:
        for line in contents:
            if not ApacheCalculater.isValidLogFormat(line):
                print(
                    "[ERROR] Invalid log format detected in file - {0}".format(log_file), file=sys.stderr)
                sys.exit(3)
            break  # Check only the first line for format

    # Calculate and display the results
    print("[INFO] IP Hits:", ApacheCalculater.calculateApacheIpHits(log_file))
    print("[INFO] Client Cache Percentage:",
          ApacheCalculater.clientCachePercentage(log_file))
    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main())
