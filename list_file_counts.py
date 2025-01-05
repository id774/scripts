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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-05
#       Initial release.
#
#  Usage:
#  python list_file_counts.py [directory]
#  Example: python list_file_counts.py /path/to/directory
#
########################################################################

import os


def get_subdirectories(base_dir):
    """Return a list of subdirectories in the specified directory."""
    try:
        return [d for d in os.listdir(base_dir) if os.path.isdir(os.path.join(base_dir, d))]
    except OSError as e:
        raise ValueError("Error accessing directory '{}': {}".format(base_dir, e))

def count_files_in_directory(directory):
    """Return the number of files in a directory."""
    try:
        return len([f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))])
    except OSError as e:
        raise ValueError("Error accessing directory '{}': {}".format(directory, e))

def count_files_in_subdirectories(base_dir):
    """Count the number of files in each subdirectory of a base directory."""
    subdirs = get_subdirectories(base_dir)
    file_counts = {}

    for subdir in subdirs:
        subdir_path = os.path.join(base_dir, subdir)
        try:
            file_counts[subdir] = count_files_in_directory(subdir_path)
        except ValueError as e:
            print(e)
            continue

    return file_counts

def sort_file_counts(file_counts):
    """Sort file counts dictionary by file count in descending order."""
    return sorted(file_counts.items(), key=lambda x: x[1], reverse=True)

def print_file_counts(base_dir, sorted_counts):
    """Print file counts for each subdirectory."""
    print("File counts in subdirectories of '{}':".format(base_dir))
    for subdir, count in sorted_counts:
        print("{:30}: {}".format(subdir, count))

def main(args):
    """Main function for script execution."""
    if len(args) != 2:
        print("""
Usage: python list_file_counts.py [directory]

This script lists the number of files in each subdirectory of the specified directory.
The output is sorted in descending order by file count.

Arguments:
  directory    The path of the directory to analyze.

Example:
  python list_file_counts.py /path/to/directory
        """)
        return 1

    base_dir = args[1]

    if not os.path.exists(base_dir):
        print("The directory '{}' does not exist.".format(base_dir))
        return 1

    if not os.path.isdir(base_dir):
        print("'{}' is not a directory.".format(base_dir))
        return 1

    file_counts = count_files_in_subdirectories(base_dir)
    sorted_counts = sort_file_counts(file_counts)
    print_file_counts(base_dir, sorted_counts)
    return 0


if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))
