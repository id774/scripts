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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
import sys


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
            print("Invalid timestamp. Please enter a valid Unix timestamp.")
    else:
        print("Usage: unixtime2date.py <unix_timestamp>")
        print("Example: unixtime2date.py 1609459200")


if __name__ == '__main__':
    main(sys.argv)
