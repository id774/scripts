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
########################################################################

import datetime
import os
import sys


def usage():
    script_path = os.path.abspath(__file__)
    in_usage = False
    with open(script_path, 'r', encoding='utf-8') as f:
        for line in f:
            if line.startswith('#  Usage:'):
                in_usage = True
                print(line[2:].strip())
                continue
            if in_usage:
                if line.startswith('#' * 10):
                    break
                if line.startswith('#'):
                    print(line[2:].strip())
    sys.exit(0)

def unixtime2date(its):
    """Convert Unix timestamp to human-readable date in local timezone (ISO 8601 format)."""
    local_datetime = datetime.datetime.fromtimestamp(
        its, datetime.timezone.utc).astimezone()
    return local_datetime.isoformat()

def main(args):
    """Process the command line arguments and output the result."""
    if len(args) > 1:
        try:
            print(unixtime2date(int(args[1])))
        except ValueError:
            print("[ERROR] Invalid timestamp. Please enter a valid Unix timestamp.", file=sys.stderr)
            sys.exit(1)
    else:
        print("[INFO] Usage: unixtime2date.py <unix_timestamp>")
        print("[INFO] Example: unixtime2date.py 1609459200")


if __name__ == '__main__':
    if len(sys.argv) == 1 or sys.argv[1] in ('-h', '--help'):
        usage()
    main(sys.argv)
