#!/usr/bin/env python
#
########################################################################
# User Shell Listing Script (usershells.py)
#
#  Description:
#  This script lists user accounts and their corresponding shells. It supports
#  both macOS (using 'dscl') and other Unix-like systems (using /etc/passwd),
#  excluding accounts with non-interactive shells like 'false', 'nologin', 'sync',
#  'shutdown', and 'halt'.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 11/29,2023
#       Added support for macOS by using 'dscl' for retrieving user and shell information.
#  v1.1 8/30,2018
#       Improved shell filtering to exclude non-interactive system accounts such as 'sync',
#       'shutdown', and 'halt'.
#  v1.0 2/14,2017
#       Initial release. Lists user accounts and their shells, excluding 'false' and 'nologin'.
#
# Usage:
#  Run the script without any arguments:
#      python usershells.py
#
#  The script detects the operating system and outputs a list of user accounts with interactive shells.
#
########################################################################

import subprocess
import platform
import os

def get_shells_from_passwd():
    shells = {}
    with open("/etc/passwd", 'r') as fo:
        for line in fo:
            line = line.strip()
            fields = line.split(":")
            shells[fields[0]] = fields[-1]
    return shells

def get_shells_from_dscl():
    shells = {}
    users = subprocess.check_output(['dscl', '.', '-list', '/Users']).decode().splitlines()
    for user in users:
        shell = subprocess.check_output(['dscl', '.', '-read', f'/Users/{user}', 'UserShell']).decode().split()[1]
        shells[user] = shell
    return shells

def main():
    os_type = platform.system()
    shells = get_shells_from_dscl() if os_type == 'Darwin' else get_shells_from_passwd()

    for account, shell in shells.items():
        if all(x not in shell for x in ['false', 'nologin', 'sync', 'shutdown', 'halt']):
            print("{0:11} => {1}".format(account, shell))

if __name__ == '__main__':
    main()

