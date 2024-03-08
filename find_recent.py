#!/usr/bin/env python

########################################################################
# find_recent.py: List files updated within a specified datetime range
#
#  Description:
#  This script lists all files within a specified directory (or the current
#  directory by default) and its subdirectories that have been modified within
#  a given datetime range. It displays the modification time of each file next
#  to the filename. Hidden directories are ignored by default unless the '-a'
#  option is used. The '-f' option can be used to list filenames only, without
#  path or modification time.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-03-08
#       Added '-s' and '-e' options for specifying start and end datetime.
#       Maintained '-d' option for backward compatibility.
#  v1.1 2024-03-03
#       Added '-f' option to list filenames only.
#  v1.0 2024-02-25
#       Initial release. Added functionality to list files based on modification date,
#       displaying their modification time, and ignoring hidden directories by default.
#
#  Usage:
#  Run this script with the '-d' option followed by the date in YYYY-MM-DD format
#  and optionally time in HH:MM format. You can also specify a directory path with '-p'.
#  Use the '-a' option to include hidden directories. The '-f' option lists filenames only.
#  Use '-s' and '-e' options to specify a datetime range.
#  Examples:
#     ./find_recent.py -d "2024-01-01"
#     ./find_recent.py -s "2024-01-01" -e "2024-01-02 13:00"
#     ./find_recent.py -s "2024-01-01" -p "/path/to/directory"
#     ./find_recent.py -d "2024-03-02" -f
#     ./find_recent.py -e "2024-01-02"
#
#  Notes:
#  - This script is compatible with Python 3.2 and later versions.
#  - If the time is not specified for '-d', '-s', or '-e', it defaults to 00:00 (midnight) for '-d' and '-s', and to 23:59 (end of day) for '-e'.
#  - Hidden directories are ignored by default. Use the '-a' option to include them.
#  - The '-f' option lists filenames only, without path or modification time.
#  - Either the start datetime '-s' or the end datetime '-e' can be specified independently for more flexible searches.
#
#  Error Conditions and Return Codes:
#  0: Success
#  1: Specified path does not exist or no arguments provided
#  2: Incorrect datetime format
#  3: Python version not supported.
#
########################################################################

import argparse
import os
import sys
from datetime import datetime, timezone


def parse_arguments():
    """
    Parses command line arguments and returns the parsed arguments.
    Includes options for specifying start and end datetimes, directory path,
    and flags for including hidden directories or listing filenames only.
    """
    parser = argparse.ArgumentParser(description='List files updated within a specified datetime range.')
    parser.add_argument('-d', '--datetime', nargs='+', help='Date and optional time in ISO format (YYYY-MM-DD [HH:MM]). Acts as the start datetime if -s is not provided.')
    parser.add_argument('-s', '--start', nargs='+', help='Start datetime in ISO format (YYYY-MM-DD [HH:MM]). Overrides -d if provided.')
    parser.add_argument('-e', '--end', nargs='+', help='End datetime in ISO format (YYYY-MM-DD [HH:MM]). Specifies the end of the datetime range.')
    parser.add_argument('-p', '--path', default='.', help='Directory path to search, defaults to current directory.')
    parser.add_argument('-a', '--all', action='store_true', help='Include hidden directories in the search.')
    parser.add_argument('-f', '--filenames', action='store_true', help='List filenames only, without path or modification time.')
    args = parser.parse_args()

    # Ensure that at least one datetime argument is provided
    if not any([args.datetime, args.start, args.end]):
        parser.print_help()
        sys.exit(1)

    return args

def parse_datetime(args):
    """
    Converts the date and optional time arguments into a datetime object.
    Supports both date-only and date with time formats.
    """
    if not args:
        return None
    datetime_str = ' '.join(args)  # Combines date and time parts if time is provided
    try:
        # First, try parsing with both date and time
        return datetime.strptime(datetime_str, "%Y-%m-%d %H:%M").replace(tzinfo=timezone.utc)
    except ValueError:
        # If that fails, try parsing with date only
        try:
            return datetime.strptime(datetime_str, "%Y-%m-%d").replace(tzinfo=timezone.utc)
        except ValueError:
            print("Error: The datetime '{}' does not match the required format.".format(datetime_str))
            sys.exit(2)

def check_directory_exists(path):
    """
    Checks if the specified directory exists. Exits with error if it does not.
    """
    if not os.path.exists(path):
        print("Error: The specified path '{}' does not exist.".format(path))
        sys.exit(1)

def list_recent_files(root_dir, start_datetime, end_datetime, include_hidden, filenames_only):
    """
    Walks through the directory tree from the root_dir, listing files modified within the specified datetime range.
    Can optionally list filenames only and include or exclude hidden directories.
    """
    for dirpath, dirnames, filenames in os.walk(root_dir):
        # Exclude hidden directories if not specified by the user
        if not include_hidden:
            dirnames[:] = [d for d in dirnames if not d.startswith('.')]
            filenames = [f for f in filenames if not f.startswith('.')]

        for file in filenames:
            file_path = os.path.join(dirpath, file)
            mtime = datetime.fromtimestamp(os.path.getmtime(file_path), tz=timezone.utc)
            # Check if file modification time is within the specified datetime range
            if (start_datetime is None or mtime >= start_datetime) and (end_datetime is None or mtime <= end_datetime):
                if filenames_only:
                    print(file)
                else:
                    print("{} - {}".format(mtime.strftime('%Y-%m-%d %H:%M:%S'), file_path))

def main():
    """
    Main function that orchestrates the flow of the script, including argument parsing,
    validation, and initiating the file listing process.
    """
    # Ensure the script is run with Python 3.2 or later
    if sys.version_info < (3, 2):
        print("Error: This script requires Python 3.2 or later.")
        sys.exit(3)

    args = parse_arguments()
    start_datetime = parse_datetime(args.start or args.datetime)
    end_datetime = parse_datetime(args.end)

    check_directory_exists(args.path)
    list_recent_files(args.path, start_datetime, end_datetime, args.all, args.filenames)


if __name__ == "__main__":
    main()
