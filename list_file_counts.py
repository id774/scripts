#!/usr/bin/env python

########################################################################
# list_file_counts.py: List file counts in subdirectories
#
#  Description:
#  This script traverses a specified directory and lists the number of
#  files present in each subdirectory. It provides a summary of the
#  file counts for all subdirectories directly under the given directory.
#  The output is sorted in descending order by file count for better clarity.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.2 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.1 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.0 2025-01-05
#       Initial release.
#
#  Usage:
#  python list_file_counts.py [directory]
#  Example: python list_file_counts.py /path/to/directory
#
########################################################################

import os
import sys


def usage():
    """ Display the script header as usage information and exit. """
    script_path = os.path.abspath(__file__)
    in_header = False
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
    sys.exit(0)

def get_subdirectories(base_dir):
    """ Return a list of subdirectories in the specified directory. """
    try:
        return [d for d in os.listdir(base_dir) if os.path.isdir(os.path.join(base_dir, d))]
    except OSError as e:
        raise ValueError("[ERROR] Error accessing directory '{}': {}".format(base_dir, e))

def count_files_in_directory(directory):
    """ Return the number of files in a directory. """
    try:
        return len([f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))])
    except OSError as e:
        raise ValueError("[ERROR] Error accessing directory '{}': {}".format(directory, e))

def count_files_in_subdirectories(base_dir):
    """ Count the number of files in each subdirectory of a base directory. """
    subdirs = get_subdirectories(base_dir)
    file_counts = {}

    for subdir in subdirs:
        subdir_path = os.path.join(base_dir, subdir)
        try:
            file_counts[subdir] = count_files_in_directory(subdir_path)
        except ValueError as e:
            print(e, file=sys.stderr)
            continue

    return file_counts

def sort_file_counts(file_counts):
    """ Sort file counts dictionary by file count in descending order. """
    return sorted(file_counts.items(), key=lambda x: x[1], reverse=True)

def print_file_counts(base_dir, sorted_counts):
    """ Print file counts for each subdirectory. """
    print("[INFO] File counts in subdirectories of '{}':".format(base_dir))
    for subdir, count in sorted_counts:
        print("{:30}: {}".format(subdir, count))

def main(args):
    """ Main function for script execution. """
    base_dir = args[1]

    if not os.path.exists(base_dir):
        print("[ERROR] The directory '{}' does not exist.".format(base_dir), file=sys.stderr)
        return 1

    if not os.path.isdir(base_dir):
        print("[ERROR] '{}' is not a directory.".format(base_dir), file=sys.stderr)
        return 1

    file_counts = count_files_in_subdirectories(base_dir)
    sorted_counts = sort_file_counts(file_counts)
    print_file_counts(base_dir, sorted_counts)
    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main(sys.argv))
