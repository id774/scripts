#!/usr/bin/env python

########################################################################
# unixtime2date.py: Convert Unix Timestamp to Human-Readable Date
#
#  Description:
#  This script converts a Unix timestamp to a human-readable date format in ISO 8601.
#  It uses the local timezone for conversion.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.2 2023-12-11
#       Modified output format to ISO 8601 in local timezone.
#  v1.1 2023-12-06
#       Removed Python version check and added comments for clarity.
#  v1.0 2014-07-29
#       Initial release.
#
#  Usage:
#  unixtime2date.py <unix_timestamp>
#
#  Example:
#  unixtime2date.py 1609459200
#  -> 2021-01-01T09:00:00+09:00 (if your local timezone is UTC+9)
#
#  Requirements:
#  - Python Version: 3.3 or later
#
########################################################################

import datetime
import os
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

def unixtime2date(its):
    """ Convert Unix timestamp to human-readable date in local timezone (ISO 8601 format). """
    local_datetime = datetime.datetime.fromtimestamp(
        its, datetime.timezone.utc).astimezone()
    return local_datetime.isoformat()

def main(args):
    """ Process the command line arguments and output the result. """
    if len(args) > 1:
        try:
            print(unixtime2date(int(args[1])))
            return 0
        except ValueError:
            print("[ERROR] Invalid timestamp. Please enter a valid Unix timestamp.", file=sys.stderr)
            return 1
    else:
        usage()


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main(sys.argv))
