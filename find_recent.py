#!/usr/bin/env python

########################################################################
# find_recent.py: List files updated after a specified datetime
#
#  Description:
#  This script lists all files in the specified directory (or current directory if not specified)
#  and its subdirectories that have been modified after a specified date and optional time,
#  displaying their modification time. Hidden directories are ignored by default.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-02-25
#       Initial release. Added functionality to list files based on modification date,
#       displaying their modification time, and ignoring hidden directories by default.
#
#  Usage:
#  Run this script with a date argument in the format YYYY-MM-DD and an optional
#  time in the format HH:MM. Use '-a' option to include hidden directories.
#  Examples: ./find_recent.py 2024-01-01
#            ./find_recent.py 2024-01-01 13:00
#            ./find_recent.py -a 2024-01-01 13:00 /path/to/directory
#
#  Notes:
#  - This script requires Python 3.9 or later.
#  - If the time is not specified, it defaults to 00:00 (midnight) of the given date.
#  - Hidden directories are ignored by default. Use '-a' to include them.
#
#  Error Conditions and Return Codes:
#  0. Success.
#  1. Missing arguments.
#  2. Incorrect datetime format.
#
########################################################################

import os
import sys
from datetime import datetime, timezone
from pathlib import Path


# Function to parse command line arguments
def parse_arguments():
    """
    Parse command line arguments.

    Returns:
        A tuple containing date_str, time_str, path, and include_hidden.
    """
    include_hidden = '-a' in sys.argv
    if include_hidden:
        sys.argv.remove('-a')

    if len(sys.argv) < 2 or len(sys.argv) > 4:
        print("Usage: find_recent.py [-a] <YYYY-MM-DD> [HH:MM] [path]")
        sys.exit(1)

    date_arg = sys.argv[1]
    time_arg = sys.argv[2] if len(sys.argv) >= 3 and ':' in sys.argv[2] else None
    path_arg = sys.argv[3] if len(sys.argv) == 4 else (sys.argv[2] if len(sys.argv) == 3 and time_arg is None else ".")

    return date_arg, time_arg, path_arg, include_hidden

# Function to convert string arguments to datetime object
def parse_datetime(date_str, time_str=None):
    """
    Parse date and time strings into a datetime object.

    Args:
        date_str: A string representing the date in YYYY-MM-DD format.
        time_str: An optional string representing the time in HH:MM format.

    Returns:
        A datetime object representing the specified date and time.
    """
    datetime_str = date_str + " " + time_str if time_str else date_str
    datetime_format = "%Y-%m-%d %H:%M" if time_str else "%Y-%m-%d"
    dt = datetime.strptime(datetime_str, datetime_format)
    return dt.replace(tzinfo=timezone.utc)

# Function to list recent files
def list_recent_files(root_dir, datetime_obj, include_hidden):
    """
    List files that have been modified after the specified datetime.

    Args:
        root_dir: The root directory to search for files.
        datetime_obj: A datetime object to compare file modification times against.
        include_hidden: A boolean indicating whether to include hidden directories and files.
    """
    for dirpath, _, filenames in os.walk(root_dir):
        if not include_hidden and '/.' in dirpath:
            continue  # Skip hidden directories
        for file in filenames:
            if not include_hidden and file.startswith('.'):
                continue  # Skip hidden files
            file_path = Path(dirpath) / file
            mtime = datetime.fromtimestamp(file_path.stat().st_mtime, tz=timezone.utc)
            if mtime > datetime_obj:
                print("{} - {}".format(mtime.strftime('%Y-%m-%d %H:%M:%S'), file_path))


if __name__ == "__main__":
    date_arg, time_arg, path_arg, include_hidden = parse_arguments()
    datetime_obj = parse_datetime(date_arg, time_arg)
    list_recent_files(path_arg, datetime_obj, include_hidden)
