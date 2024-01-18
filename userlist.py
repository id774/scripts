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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#      python userlist.py
#
#  The script automatically detects the system type and sets the UID threshold
#  accordingly. It then displays a list of user accounts with UIDs above this threshold.
#
########################################################################

import os
import platform
import subprocess


def show_userlist(threshold):
    if platform.system() == 'Darwin':
        # macOS
        output = subprocess.check_output(
            ['dscacheutil', '-q', 'user']).decode()
        users = output.split('\n\n')
        for user in users:
            user_info = dict(line.split(': ')
                             for line in user.split('\n') if line)
            uid = int(user_info.get('uid', -1))
            name = user_info.get('name', '')
            if uid >= threshold:
                print(name)
    else:
        # Linux/Unix
        with open('/etc/passwd', 'r') as fo:
            for line in fo:
                parts = line.strip().split(':')
                if len(parts) >= 3 and parts[2].isdigit():
                    uid = int(parts[2])
                    if uid >= threshold:
                        print(parts[0])

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


if __name__ == '__main__':
    main()
