#!/usr/bin/env python

########################################################################
# apache_calculater.py: Apache Log File Analysis Tool
#
#  Description:
#  This script analyzes Apache log files to calculate the number of hits per IP
#  address and the percentage of client cache hits. It is designed to provide
#  insights into web server traffic and client behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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

class ApacheCalculater(object):

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
        with open(log, "r") as contents:
            for line in contents:
                # Extract the IP address from each log entry
                ip = line.split(" ", 1)[0]
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
        with open(log, "r") as contents:
            for line in contents:
                totalRequests += 1
                # Check if the response status is 304 (Not Modified)
                if line.split(" ")[8] == "304":
                    cachedRequests += 1
        return float(100 * cachedRequests) / totalRequests

def main():
    if len(sys.argv) != 2:
        print("Usage: apache_calculater.py log_file_name")
        sys.exit(1)

    log_file = sys.argv[1]
    # Calculate and display the results
    print("IP Hits:", ApacheCalculater.calculateApacheIpHits(log_file))
    print("Client Cache Percentage:", ApacheCalculater.clientCachePercentage(log_file))

if __name__ == '__main__':
    main()

