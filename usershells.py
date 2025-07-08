#!/usr/bin/env python

########################################################################
# usershells.py: User Shell Listing Script
#
#  Description:
#  This script lists user accounts and their corresponding shells. It supports
#  both macOS (using 'dscl') and other Unix-like systems (using /etc/passwd),
#  excluding accounts with non-interactive shells like 'false', 'nologin', 'sync',
#  'shutdown', and 'halt'.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.6 2025-07-08
#       Fixed compatibility issues with Python 3.4.
#  v1.5 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#  v1.2 2023-11-29
#       Added support for macOS by using 'dscl' for retrieving user and shell information.
#  v1.1 2018-08-30
#       Improved shell filtering to exclude non-interactive system accounts such as 'sync',
#       'shutdown', and 'halt'.
#  v1.0 2017-02-14
#       Initial release. Lists user accounts and their shells, excluding 'false' and 'nologin'.
#
#  Usage:
#  Run the script without any arguments:
#      usershells.py
#
#  The script detects the operating system and outputs a list of user accounts with interactive shells.
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

def get_shells_from_passwd():
    shells = {}
    try:
        with open("/etc/passwd", 'r') as fo:
            for line in fo:
                line = line.strip()
                fields = line.split(":")
                if len(fields) >= 7:
                    shells[fields[0]] = fields[-1]
    except Exception as e:
        print("Error reading /etc/passwd: %s" % str(e), file=sys.stderr)
    return shells

def get_shells_from_dscl():
    shells = {}
    try:
        users = subprocess.check_output(
            ['dscl', '.', '-list', '/Users']).decode('utf-8').splitlines()
        for user in users:
            try:
                shell_output = subprocess.check_output(
                    ['dscl', '.', '-read', '/Users/' + user, 'UserShell']
                ).decode('utf-8').strip().split()
                if len(shell_output) >= 2:
                    shells[user] = shell_output[1]
            except Exception:
                continue
    except Exception as e:
        print("Error retrieving user list from dscl: %s" % str(e), file=sys.stderr)
    return shells

def main():
    os_type = platform.system()
    shells = get_shells_from_dscl() if os_type == 'Darwin' else get_shells_from_passwd()

    for account, shell in shells.items():
        if all(x not in shell for x in ['false', 'nologin', 'sync', 'shutdown', 'halt']):
            print("{0:11} => {1}".format(account, shell))

    return 0


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())
