#!/usr/bin/env python

########################################################################
# apache_calculater.py: Apache Log File Analysis Tool
#
#  Description:
#  This script analyzes Apache log files to calculate the number of hits per IP
#  address and the percentage of client cache hits. It supports .gz compressed
#  log files and excludes IPs listed in apache_ignore.list. It is designed to
#  provide insights into web server traffic and client behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-14
#       Added support for .gz log files and implemented IP ignore list.
#  v1.1 2023-12-06
#       Refactored for improved clarity and removed unnecessary path manipulation.
#  v1.0 2011-04-20
#       Initial release.
#
#  Usage:
#  python apache_calculater.py <log_file>
#  Example: python apache_calculater.py /var/log/apache2/access.log
#
########################################################################

import sys
import gzip

class ApacheCalculater(object):

    @staticmethod
    def loadIgnoreList(ignore_file):
        """
        Load the ignore list from a file.

        Args:
            ignore_file (str): Path to the ignore list file.

        Returns:
            set: A set of IPs to ignore.
        """
        ignore_ips = set()
        try:
            with open(ignore_file, "r") as file:
                for line in file:
                    ignore_ips.add(line.strip())
        except FileNotFoundError:
            pass  # If the file doesn't exist, return an empty set
        return ignore_ips

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
        ignore_ips = cls.loadIgnoreList("./etc/apache_ignore.list")

        open_func = gzip.open if log.endswith(".gz") else open

        with open_func(log, "rt") as contents:
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
        open_func = gzip.open if log.endswith(".gz") else open

        with open_func(log, "rt") as contents:
            for line in contents:
                totalRequests += 1
                # Check if the response status is 304 (Not Modified)
                if line.split(" ")[8] == "304":
                    cachedRequests += 1

        return float(100 * cachedRequests) / totalRequests if totalRequests > 0 else 0

def main():
    if len(sys.argv) != 2:
        print("Usage: apache_calculater.py log_file_name")
        sys.exit(1)

    log_file = sys.argv[1]
    # Calculate and display the results
    print("IP Hits:", ApacheCalculater.calculateApacheIpHits(log_file))
    print("Client Cache Percentage:",
          ApacheCalculater.clientCachePercentage(log_file))


if __name__ == '__main__':
    main()
