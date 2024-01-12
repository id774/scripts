#!/usr/bin/env python

########################################################################
# pyping.py: Ping a Range of IP Addresses in a Subnet
#
#  Description:
#  This script pings a range of IP addresses within a specified subnet.
#  It's useful for quickly checking the status of multiple IPs.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-12
#      Initial release. Python version of the rubyping.rb script.
#
#  Usage:
#  pyping.py <subnet> <start_ip> <end_ip>
#  Example: pyping.py 192.168.11. 1 32
#
#  Notes:
#  - Ensure you have permissions to send pings to the target IPs.
#  - This script may take time to complete based on the range specified.
#
########################################################################

import sys
import subprocess
import threading

def ping(ip):
    """Send a ping request to the specified IP address."""
    try:
        subprocess.check_output(
            ["ping", "-c", "1", "-i", "1", ip], stderr=subprocess.DEVNULL)
        print("{} --> alive".format(ip))
    except subprocess.CalledProcessError:
        print("{} --> -----".format(ip))

def main(subnet, start_ip, end_ip):
    """Ping a range of IP addresses in the given subnet."""
    threads = []

    for n in range(start_ip, end_ip + 1):
        ip = subnet + str(n)
        thread = threading.Thread(target=ping, args=(ip,))
        threads.append(thread)
        thread.start()

    for thread in threads:
        thread.join()


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: pyping.py <subnet> <start_ip> <end_ip>")
        print("Example: pyping.py 192.168.11. 1 32")
        sys.exit(1)

    subnet = sys.argv[1]
    start_ip = int(sys.argv[2])
    end_ip = int(sys.argv[3])

    main(subnet, start_ip, end_ip)
