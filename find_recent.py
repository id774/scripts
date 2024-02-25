#!/usr/bin/env python

########################################################################
# find_recent.py: List files updated after a specified datetime
#
#  Description:
#  This script lists all files within a specified directory (or the current
#  directory by default) and its subdirectories that have been modified after
#  a given date and time. It displays the modification time of each file.
#  Hidden directories are ignored by default unless the '-a' option is used.
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
#  Run this script with a date argument in the format YYYY-MM-DD and an
#  optional time in the format HH:MM. You can also specify a directory path.
#  Use the '-a' option to include hidden directories.
#  Examples:
#    ./find_recent.py 2024-01-01
#    ./find_recent.py 2024-01-01 13:00
#    ./find_recent.py 2024-01-01 /path/to/directory
#    ./find_recent.py -a 2024-01-01 13:00 /path/to/directory
#
#  Notes:
#  - This script is compatible with Python 3.2 and later versions.
#  - If the time is not specified, it defaults to 00:00 (midnight) of the given date.
#  - Hidden directories are ignored by default. Use the '-a' option to include them.
#
#  Error Conditions and Return Codes:
#  0: Success
#  1: Missing or incorrect arguments
#  2: Incorrect datetime format
#  3. Python version not supported.
#
########################################################################

import os
import sys
from datetime import datetime, timezone


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
    for dirpath, dirnames, filenames in os.walk(root_dir):
        if not include_hidden:
            # Filter out hidden directories
            dirnames[:] = [d for d in dirnames if not d.startswith('.')]
            # Filter out hidden files
            filenames = [f for f in filenames if not f.startswith('.')]

        for file in filenames:
            file_path = os.path.join(dirpath, file)  # Changed from Path(dirpath) / file
            mtime = datetime.fromtimestamp(os.path.getmtime(file_path), tz=timezone.utc)  # Changed from file_path.stat().st_mtime
            if mtime > datetime_obj:
                print("{} - {}".format(mtime.strftime('%Y-%m-%d %H:%M:%S'), file_path))  # No change in print statement


if __name__ == "__main__":
    # Check Python version
    if sys.version_info < (3, 2):
        print("Error: This script requires Python 3.2 or later.")
        sys.exit(3)
    date_arg, time_arg, path_arg, include_hidden = parse_arguments()
    datetime_obj = parse_datetime(date_arg, time_arg)
    list_recent_files(path_arg, datetime_obj, include_hidden)
