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
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script without any arguments:
#      usershells.py
#
#  The script detects the operating system and outputs a list of user accounts with interactive shells.
#
#  Options:
#      -n, --name-only   Output usernames only
#      -c, --colon       Output in "username:/shell" format
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Exit Status:
#  0. User shell enumeration completed.
#  1. Platform user-shell source retrieval or decoding failed.
#  9. Unsupported Python version.
#
#  Version History:
#  v1.9 2026-09-20
#       Return failure for user-source errors while continuing expected
#       per-user UserShell omissions on macOS.
#  v1.8 2026-07-08
#       Specify UTF-8 encoding when reading /etc/passwd.
#  v1.7 2025-08-06
#       Add --name-only and --colon output options.
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
#       Initial release.
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
    """ Return (shells, status); status is 1 on read/decode failure. """
    shells = {}
    try:
        with open("/etc/passwd", 'r', encoding='utf-8') as fo:
            for line in fo:
                line = line.strip()
                fields = line.split(":")
                if len(fields) >= 7:
                    shells[fields[0]] = fields[-1]
    except (OSError, UnicodeError) as e:
        print("[ERROR] Failed to read /etc/passwd: %s" % str(e), file=sys.stderr)
        return {}, 1
    return shells, 0


def get_shells_from_dscl():
    """ Return (shells, status); status is 1 on source retrieval/decode failure. """
    shells = {}
    status = 0
    try:
        users = subprocess.check_output(
            ['dscl', '.', '-list', '/Users']).decode('utf-8').splitlines()
    except (subprocess.CalledProcessError, OSError, UnicodeError) as e:
        print("[ERROR] Failed to retrieve user list from dscl: %s" % str(e),
              file=sys.stderr)
        return {}, 1

    for user in users:
        try:
            shell_output = subprocess.check_output(
                ['dscl', '.', '-read', '/Users/' + user, 'UserShell']
            ).decode('utf-8').strip().split()
            if len(shell_output) >= 2:
                shells[user] = shell_output[1]
        except subprocess.CalledProcessError:
            # UserShell attribute not present for this account; expected skip.
            continue
        except (OSError, UnicodeError) as e:
            print("[ERROR] Failed to retrieve UserShell for %s: %s" % (user, str(e)),
                  file=sys.stderr)
            status = 1
            continue

    return shells, status


def main():
    args = sys.argv[1:]
    name_only = '--name-only' in args or '-n' in args
    colon_format = '--colon' in args or '-c' in args

    os_type = platform.system()
    if os_type == 'Darwin':
        shells, status = get_shells_from_dscl()
    else:
        shells, status = get_shells_from_passwd()

    for account, shell in shells.items():
        if all(x not in shell for x in ['false', 'nologin', 'sync', 'shutdown', 'halt']):
            if name_only:
                print(account)
            elif colon_format:
                print("%s:%s" % (account, shell))
            else:
                print("{0:11} => {1}".format(account, shell))

    return status


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main())
