#!/usr/bin/env python

########################################################################
# userlist.py: User List Display Script
#
#  Description:
#  This script lists user accounts with UIDs greater than a specified threshold.
#  The threshold is set based on the existence of system-specific files,
#  distinguishing between Debian-based, RedHat-based, and macOS systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-07-08
#       Fixed compatibility issues with Python 3.4.
#  v1.4 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2024-01-17
#       Ported from Shell script to Python. Maintains functionality across
#       different systems with compatibility considerations.
#  v1.1 2023-11-29
#       Improved script to support macOS by using 'dscacheutil' for user information retrieval.
#       Maintained compatibility with Linux and other Unix-like systems.
#  v1.0 2014-11-16
#       Initial release. Detects system type and displays user accounts with
#       UIDs above the system-specific threshold.
#
#  Usage:
#  Run the script without any arguments:
#      userlist.py
#
#  The script automatically detects the system type and sets the UID threshold
#  accordingly. It then displays a list of user accounts with UIDs above this threshold.
#
#  Requirements:
#  - Python Version: 3.1 or later
#
########################################################################

import os
import platform
import subprocess
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

def parse_dscacheutil_output(output):
    users = []
    current_user = {}
    for line in output.splitlines():
        if not line.strip():
            if current_user:
                users.append(current_user)
                current_user = {}
            continue
        if ': ' in line:
            key, value = line.split(': ', 1)
            current_user[key.strip()] = value.strip()
    if current_user:
        users.append(current_user)
    return users

def show_userlist(threshold):
    if platform.system() == 'Darwin':
        try:
            output = subprocess.check_output(['dscacheutil', '-q', 'user']).decode('utf-8')
            users = parse_dscacheutil_output(output)
            for user_info in users:
                try:
                    uid = int(user_info.get('uid', -1))
                    name = user_info.get('name', '')
                    if uid >= threshold:
                        print(name)
                except (ValueError, TypeError):
                    continue
        except Exception as e:
            print("Error retrieving user list: %s" % str(e), file=sys.stderr)
    else:
        try:
            with open('/etc/passwd', 'r') as fo:
                for line in fo:
                    parts = line.strip().split(':')
                    if len(parts) >= 3 and parts[2].isdigit():
                        uid = int(parts[2])
                        if uid >= threshold:
                            print(parts[0])
        except Exception as e:
            print("Error reading /etc/passwd: %s" % str(e), file=sys.stderr)

def main():
    threshold = 0
    if os.path.isfile('/etc/debian_version'):
        threshold = 1000
    elif os.path.isfile('/etc/redhat-release'):
        threshold = 500
    elif platform.system() == 'Darwin':
        threshold = 500

    if threshold != 0:
        show_userlist(threshold)

    return 0


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())
