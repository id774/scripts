#!/usr/bin/env python

########################################################################
# pyping.py: Ping a Range of IP Addresses in a Subnet
#
#  Description:
#  This script pings a range of IP addresses within a specified subnet.
#  It can display results either immediately (faster) or in ascending
#  order of IP addresses for better readability.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-01-06
#      Added `--ordered` option with shorthand `-o` for sorted output.
#  v1.1 2024-01-15
#      Suppressed standard error output from ping command.
#      Improved command line argument interface using argparse.
#  v1.0 2024-01-12
#      Initial release. Python version of the rubyping.rb script.
#
#  Usage:
#  pyping.py [-h] [-o] subnet start_ip end_ip
#
#  Ping a range of IP addresses in a subnet.
#
#  positional arguments:
#    subnet         Subnet part of the IP. Example: "192.168.11."
#    start_ip       Start of the IP range. Example: 1
#    end_ip         End of the IP range. Example: 32
#
#  options:
#    -h, --help     show this help message and exit
#    -o, --ordered  Display results in ascending order of IP addresses.
#
#  Notes:
#  - Ensure you have permissions to send pings to the target IPs.
#  - This script may take time to complete based on the range specified.
#
########################################################################

import argparse
import os
import subprocess
import sys
import threading


def usage():
    script_path = os.path.abspath(__file__)
    in_header = False
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
    sys.exit(0)

def ping(ip, results):
    """
    Send a ping request to the specified IP address.

    Args:
        ip (str): The target IP address to ping.
        results (dict): A shared dictionary to store ping results.
    """
    try:
        with open(os.devnull, 'w') as DEVNULL:
            # Execute the ping command and suppress standard error output
            subprocess.check_output(
                ["ping", "-c", "1", "-i", "1", ip], stderr=DEVNULL)
            results[ip] = "alive"
    except subprocess.CalledProcessError:
        results[ip] = "-----"


def main(subnet, start_ip, end_ip, ordered):
    """
    Ping a range of IP addresses in the given subnet.

    Args:
        subnet (str): Subnet prefix, e.g., "192.168.1."
        start_ip (int): Start of the IP address range.
        end_ip (int): End of the IP address range.
        ordered (bool): Whether to sort the results before displaying.
    """
    threads = []  # List to store threading.Thread objects
    results = {}  # Dictionary to store ping results

    # Create and start threads for each IP in the range
    for n in range(start_ip, end_ip + 1):
        ip = subnet + str(n)
        thread = threading.Thread(target=ping, args=(ip, results))
        threads.append(thread)
        thread.start()

    # Wait for all threads to complete
    for thread in threads:
        thread.join()

    # Display results based on the `ordered` flag
    if ordered:
        # Sort IP addresses in ascending order before displaying
        for ip in sorted(results.keys(), key=lambda x: tuple(map(int, x.split('.')))):
            print("{} --> {}".format(ip, results[ip]))

    else:
        # Display results as they are collected
        for ip, status in results.items():
            print("{} --> {}".format(ip, status))


if __name__ == "__main__":
    if len(sys.argv) < 3 or sys.argv[1] in ('-h', '--help'):
        usage()

    # Setup argparse for command-line arguments
    parser = argparse.ArgumentParser(
        description='Ping a range of IP addresses in a subnet.')
    parser.add_argument('subnet', type=str,
                        help='Subnet part of the IP. Example: "192.168.11."')
    parser.add_argument('start_ip', type=int,
                        help='Start of the IP range. Example: 1')
    parser.add_argument('end_ip', type=int,
                        help='End of the IP range. Example: 32')
    parser.add_argument('-o', '--ordered', action='store_true',
                        help='Display results in ascending order of IP addresses.')

    # Parse command-line arguments
    args = parser.parse_args()

    # Call the main function with parsed arguments
    main(args.subnet, args.start_ip, args.end_ip, args.ordered)
