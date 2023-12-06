#!/usr/bin/env python

########################################################################
# unixtime2date.py: Convert Unix Timestamp to Human-Readable Date
#
#  Description:
#  This script converts a Unix timestamp to a human-readable date format.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  -> 2021/01/01 09:00:00
#
########################################################################

import sys
import datetime

def unixtime2date(its):
    """Convert Unix timestamp to human-readable date."""
    return datetime.datetime.fromtimestamp(its).strftime('%Y/%m/%d %H:%M:%S')

def main(args):
    """Process the command line arguments and output the result."""
    print(unixtime2date(int(args[1])))


if __name__ == '__main__':
    if len(sys.argv) > 1:
        main(sys.argv)
    else:
        print("Usage: unixtime2date.py <unix_timestamp>")
        print("Example: unixtime2date.py 1609459200")
