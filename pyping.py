#!/usr/bin/env python

########################################################################
# pyping.py: Ping a Range of IP Addresses in a Subnet
#
#  Description:
#  This script pings a range of IP addresses within a specified subnet.
#  It can display results either immediately (faster) or in ascending
#  order of IP addresses for better readability.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      pyping.py [-h] [-o] subnet start_ip end_ip
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
#  Requirements:
#  - Python Version: 3.2 or later
#  - ping(8) must be present on PATH and executable.
#
#  Notes:
#  - Ensure you have permissions to send pings to the target IPs.
#  - This script may take time to complete based on the range specified.
#
#  Exit Status:
#  0. Ping sweep completed.
#  1. Invalid range or ping execution failed after startup.
#  2. Invalid command-line arguments.
#  9. Unsupported Python version.
#  126. ping exists on PATH but is not executable.
#  127. ping is not installed.
#
#  Version History:
#  v2.0 2026-09-20
#       Validate host ranges and distinguish unreachable hosts from
#       ping command execution failures.
#  v1.4 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-01-06
#       Added `--ordered` option with shorthand `-o` for sorted output.
#  v1.1 2024-01-15
#       Suppressed standard error output from ping command.
#       Improved command line argument interface using argparse.
#  v1.0 2024-01-12
#       Initial release.
#
########################################################################

import argparse
import os
import subprocess
import sys
import threading


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

def find_command_with_status(command):
    """
    Search PATH for the given command.

    Return a tuple (path, status). The status is 0 when an executable
    candidate is found, 126 when a same-named file exists on PATH but no
    candidate is executable, and 127 when no candidate exists at all.
    """
    found_non_executable = False
    for directory in os.environ.get("PATH", "").split(os.pathsep):
        candidate = os.path.join(directory if directory else ".", command)
        if not os.path.isfile(candidate):
            continue
        if os.access(candidate, os.X_OK):
            return candidate, 0
        found_non_executable = True
    if found_non_executable:
        return None, 126
    return None, 127


def ping(ip, results, errors):
    """
    Send a ping request to the specified IP address.

    Args:
        ip (str): The target IP address to ping.
        results (dict): A shared dictionary to store network reachability results.
        errors (dict): A shared dictionary to store ping execution errors.
    """
    try:
        with open(os.devnull, 'w') as DEVNULL:
            # Execute the ping command and suppress standard error output
            subprocess.check_output(
                ["ping", "-c", "1", "-i", "1", ip], stderr=DEVNULL)
            results[ip] = "alive"
    except subprocess.CalledProcessError:
        results[ip] = "-----"
    except OSError as e:
        errors[ip] = str(e)


def validate_range(start_ip, end_ip):
    """ Validate the host-number range used for the ping sweep. """
    if start_ip < 0 or end_ip > 255:
        print("[ERROR] IP range values must be between 0 and 255.", file=sys.stderr)
        return False

    if start_ip > end_ip:
        print("[ERROR] start_ip must be less than or equal to end_ip.", file=sys.stderr)
        return False

    return True


def main(subnet, start_ip, end_ip, ordered):
    """
    Ping a range of IP addresses in the given subnet.

    Args:
        subnet (str): Subnet prefix, e.g., "192.168.1."
        start_ip (int): Start of the IP address range.
        end_ip (int): End of the IP address range.
        ordered (bool): Whether to sort the results before displaying.
    """
    if not validate_range(start_ip, end_ip):
        return 1

    cmd_path, cmd_status = find_command_with_status("ping")
    if cmd_status == 126:
        print("[ERROR] Command 'ping' is not executable. Please check the permissions.", file=sys.stderr)
        return 126
    if cmd_status == 127:
        print("[ERROR] Command 'ping' is not installed. Please install ping and try again.", file=sys.stderr)
        return 127

    threads = []  # List to store threading.Thread objects
    results = {}  # Dictionary to store network reachability results
    errors = {}   # Dictionary to store ping execution errors

    # Create and start threads for each IP in the range
    for n in range(start_ip, end_ip + 1):
        ip = subnet + str(n)
        thread = threading.Thread(target=ping, args=(ip, results, errors))
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

    if errors:
        # Report ping execution failures in stable numeric IP order
        for ip in sorted(errors.keys(), key=lambda x: tuple(map(int, x.split('.')))):
            print("[ERROR] Ping execution failed for %s: %s" % (ip, errors[ip]), file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    if len(sys.argv) < 3 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    if sys.version_info < (3, 2):
        print("[ERROR] This script requires Python 3.2 or later.", file=sys.stderr)
        sys.exit(9)

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
    sys.exit(main(args.subnet, args.start_ip, args.end_ip, args.ordered))
