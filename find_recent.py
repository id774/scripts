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
#  Run this script with the '-d' option followed by the date in YYYY-MM-DD format
#  and optionally time in HH:MM format. You can also specify a directory path with '-p'.
#  Use the '-a' option to include hidden directories.
#  Examples:
#     ./find_recent.py -d "2024-01-01"
#     ./find_recent.py -d "2024-01-01 13:00"
#     ./find_recent.py -d "2024-01-01" -p "/path/to/directory"
#     ./find_recent.py -d "2024-01-01 13:00" -a -p "/path/to/directory"
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
#  3: Specified path does not exist
#  4. Python version not supported.
#
########################################################################

import argparse
import os
import sys
from datetime import datetime, timezone


# Function to parse command line arguments
def parse_arguments():
    """
    Parses command line arguments.

    Returns:
        datetime_str: A string representing the date and optional time in ISO format.
        path: The directory path to search. Defaults to current directory if not specified.
        include_hidden: Boolean flag indicating whether to include hidden directories in the search.
    """
    parser = argparse.ArgumentParser(description='List files updated after a specified datetime.')

    # Add argument for date and optional time in ISO format (YYYY-MM-DD or "YYYY-MM-DD HH:MM")
    parser.add_argument('-d', '--datetime', required=True, help='Date and optional time in ISO format (YYYY-MM-DD or "YYYY-MM-DD HH:MM"). Time is optional and defaults to 00:00.')

    # Add argument for the search directory path, defaulting to the current directory
    parser.add_argument('-p', '--path', default='.', help='Directory path to search, defaults to current directory.')

    # Add argument to include hidden directories in the search
    parser.add_argument('-a', '--all', action='store_true', help='Include hidden directories in the search.')

    args = parser.parse_args()

    # date_arg and time_arg are combined in args.datetime, so they need to be split
    datetime_parts = args.datetime.split(' ')
    date_arg = datetime_parts[0]
    time_arg = datetime_parts[1] if len(datetime_parts) > 1 else None

    return date_arg, time_arg, args.path, args.all


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

    try:
        dt = datetime.strptime(datetime_str, datetime_format)
    except ValueError:
        print("Error: The datetime '{}' does not match the required format '{}'.".format(datetime_str, datetime_format))
        sys.exit(2)

    return dt.replace(tzinfo=timezone.utc)

def check_directory_exists(path):
    """
    Check if the specified directory exists.

    Args:
        path: The directory path to check.
    """
    if not os.path.exists(path):
        print("Error: The specified path '{}' does not exist.".format(path))
        sys.exit(3)

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
        sys.exit(4)

    date_arg, time_arg, path_arg, include_hidden = parse_arguments()
    datetime_obj = parse_datetime(date_arg, time_arg)

    # Call the new function to check directory existence
    check_directory_exists(path_arg)

    list_recent_files(path_arg, datetime_obj, include_hidden)
