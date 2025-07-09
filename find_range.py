#!/usr/bin/env python

########################################################################
# find_range.py: List files updated within a specified datetime range
#
#  Description:
#  This script lists all files within a specified directory (or the current
#  directory by default) and its subdirectories that have been modified within
#  a given datetime range. By default, it displays the modification time of each file next
#  to the filename in UTC. Hidden directories are ignored by default unless the '-a'
#  option is used. The '-f' option can be used to list filenames only, without
#  path or modification time. The '-fp' option can be used to list full path with filename only,
#  without modification time. Note: By default, all input and output times are treated as UTC,
#  unless the '--localtime' option is used to use the local timezone instead.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.9 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.8 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.7 2024-11-15
#       Added '-fp' option to list full path and filename only without modification time.
#  v1.6 2024-06-07
#       Fixed the issue where the local time range was not properly handled.
#  v1.5 2024-03-28
#       Enhanced the output format to include timezone offset in a format like '+09:00' or '-05:00'
#       when using local timezone with '-l' option.
#  v1.4 2024-03-16
#       Fixed a bug where hidden files and directories were not properly excluded
#       when the '-a' option was not used.
#  v1.3 2024-03-14
#       Modified the output format to ISO 8601, indicating UTC dates and times.
#       Added '-l' option to use local timezone for input and output times.
#  v1.2 2024-03-08
#       Added '-s' and '-e' options for specifying start and end datetime in UTC.
#       Maintained '-d' option for backward compatibility.
#       Renamed script to find_range.py to better reflect its functionality
#       of searching files within a specified datetime range in UTC.
#  v1.1 2024-03-03
#       Added '-f' option to list filenames only.
#  v1.0 2024-02-25
#       Initial release. Added functionality to list files based on modification date,
#       displaying their modification time in UTC, and ignoring hidden directories by default.
#
#  Usage:
#  Run this script with the appropriate options to list files modified within a specified datetime range.
#      find_range.py [-h] [-d DATETIME [DATETIME ...]] [-s START [START ...]] [-e END [END ...]] [-p PATH] [-a] [-f] [-fp] [-l]
#  Examples:
#      find_range.py -d "2024-01-01"
#      find_range.py -s "2024-01-01" -e "2024-01-02 13:00"
#      find_range.py -d "2024-01-01" -p "/path/to/directory"
#      find_range.py -d "2024-03-02" -f
#      find_range.py -e "2024-01-02"
#      find_range.py -l -s "2024-01-01" -e "2024-01-02 13:00"
#      find_range.py -fp -s "2024-11-15 00:47" -e "2024-11-15 00:48"
#
#  Options:
#  -d, --datetime:  Date and optional time in ISO format (YYYY-MM-DD [HH:MM]).
#                   Acts as the start datetime if -s is not provided.
#  -s, --start:     Start datetime in ISO format (YYYY-MM-DD [HH:MM]). Overrides -d if provided.
#  -e, --end:       End datetime in ISO format (YYYY-MM-DD [HH:MM]). Specifies the end of the datetime range.
#  -p, --path:      Directory path to search, defaults to current directory.
#  -a, --all:       Include hidden directories in the search.
#  -f, --filenames: List filenames only, without path or modification time.
#  -fp, --fullpath: List full path and filename only, without modification time.
#  -l, --localtime: Use local timezone for input and output times instead of UTC.
#
#  Requirements:
#  - Python Version: 3.3 or later
#
#  Notes:
#  - This script is compatible with Python 3.3 and later versions.
#  - If the time is not specified for '-d', '-s', or '-e',
#    it defaults to 00:00 (midnight) for '-d' and '-s', and to 23:59 (end of day) for '-e',
#    all in UTC unless '--localtime' is used.
#  - Hidden directories are ignored by default. Use the '-a' option to include them.
#  - The '-f' option lists filenames only, without path or modification time.
#  - The '-fp' option lists the full path and filename only, without modification time.
#  - Either the start datetime '-s' or the end datetime '-e' can be specified independently for
#    more flexible searches, both expected to be in UTC unless '--localtime' is used.
#
#  Error Conditions and Return Codes:
#  0: Success
#  1: Specified path does not exist or no arguments provided
#  2: Incorrect datetime format or mutually exclusive options '-f' and '-fp' were used together
#  3: Python version not supported
#
########################################################################

import argparse
import os
import sys
from datetime import datetime, timezone


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
    parser.add_argument('-fp', '--fullpath', action='store_true', help='List full path and filename only, without modification time.')
    parser.add_argument('-l', '--localtime', action='store_true', help='Use local timezone for input and output times instead of UTC.')
    args = parser.parse_args()

    # Check for invalid combination of options
    if args.filenames and args.fullpath:
        parser.error("Options '-f' and '-fp' cannot be used together.")

    # Ensure that at least one datetime argument is provided
    if not any([args.datetime, args.start, args.end]):
        parser.print_help()
        sys.exit(1)

    return args

def parse_datetime(args, use_localtime=False):
    """
    Converts the date and optional time arguments into a datetime object.
    Supports both date-only and date with time formats.
    """
    if not args:
        return None
    datetime_str = ' '.join(args)  # Combines date and time parts if time is provided
    try:
        # First, try parsing with both date and time
        parsed_datetime = datetime.strptime(datetime_str, "%Y-%m-%d %H:%M")
        if use_localtime:
            local_tz = datetime.now().astimezone().tzinfo
            return parsed_datetime.replace(tzinfo=local_tz)
        else:
            return parsed_datetime.replace(tzinfo=timezone.utc)
    except ValueError:
        # If that fails, try parsing with date only
        try:
            parsed_datetime = datetime.strptime(datetime_str, "%Y-%m-%d")
            if use_localtime:
                local_tz = datetime.now().astimezone().tzinfo
                return parsed_datetime.replace(tzinfo=local_tz)
            else:
                return parsed_datetime.replace(tzinfo=timezone.utc)
        except ValueError:
            print("[ERROR] The datetime '{}' does not match the required format.".format(datetime_str), file=sys.stderr)
            sys.exit(2)

def check_directory_exists(path):
    """
    Checks if the specified directory exists. Exits with error if it does not.
    """
    if not os.path.exists(path):
        print("[ERROR] The specified path '{}' does not exist.".format(path), file=sys.stderr)
        sys.exit(1)

def list_recent_files(root_dir, start_datetime, end_datetime, include_hidden, filenames_only, fullpath_only, use_localtime=False):
    """
    Walks through the directory tree from the root_dir, listing files modified within the specified datetime range.
    Can optionally list filenames only and include or exclude hidden directories.
    """
    for dirpath, dirnames, filenames in os.walk(root_dir):
        # Exclude hidden directories if not specified by the user
        if not include_hidden:
            dirnames[:] = [d for d in dirnames if not d.startswith('.')]  # Exclude hidden directories from dirnames to prevent walking into them
            if os.path.basename(dirpath).startswith('.'):
                continue  # Skip hidden directories entirely
            filenames = [f for f in filenames if not f.startswith('.')]

        for file in filenames:
            file_path = os.path.join(dirpath, file)
            if use_localtime:
                mtime = datetime.fromtimestamp(os.path.getmtime(file_path)).astimezone()
            else:
                mtime = datetime.fromtimestamp(os.path.getmtime(file_path), tz=timezone.utc)
            # Check if file modification time is within the specified datetime range
            if (start_datetime is None or mtime >= start_datetime) and (end_datetime is None or mtime <= end_datetime):
                if filenames_only:
                    print(file)
                elif fullpath_only:
                    print(file_path)
                else:
                    # Format the modification time in ISO 8601 format, indicating UTC with 'Z'
                    if use_localtime:
                        # Convert timezone offset to '鬮ｮ逕ｻ魑･h:mm' format
                        tz_offset = mtime.strftime('%z')  # Get timezone offset, e.g., '+0900'
                        tz_formatted = "{}:{}".format(tz_offset[:-2], tz_offset[-2:])  # Format to '鬮ｮ逕ｻ魑･h:mm'
                        print("{}{} - {}".format(mtime.strftime('%Y-%m-%dT%H:%M:%S'), tz_formatted, file_path))
                    else:
                        print("{} - {}".format(mtime.strftime('%Y-%m-%dT%H:%M:%SZ'), file_path))

def main():
    """
    Main function that orchestrates the flow of the script, including argument parsing,
    validation, and initiating the file listing process.
    """
    # Ensure the script is run with Python 3.3 or later
    if sys.version_info < (3, 3):
        print("[ERROR] This script requires Python 3.3 or later.", file=sys.stderr)
        return 3

    args = parse_arguments()
    start_datetime = parse_datetime(args.start or args.datetime, use_localtime=args.localtime)
    end_datetime = parse_datetime(args.end, use_localtime=args.localtime)

    check_directory_exists(args.path)
    list_recent_files(args.path, start_datetime, end_datetime, args.all, args.filenames, args.fullpath, use_localtime=args.localtime)

    return 0


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())
