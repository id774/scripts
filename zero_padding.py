#!/usr/bin/env python

########################################################################
# zero_padding.py: File Renaming Script
#
#  Description:
#  This script is designed to rename files within a specified directory by
#  zero-padding the numeric portions of file names to a uniform number of digits.
#  This functionality facilitates organized and consistent naming conventions,
#  particularly useful for sorting and managing large collections of files.
#  For example, files named "photo1.jpg", "photo2.jpg", and "photo10.jpg"
#  would be renamed to "photo0001.jpg", "photo0002.jpg", and "photo0010.jpg",
#  assuming a padding target of 4 digits.
#
#  The script supports a quiet mode option to suppress logging output, which is
#  beneficial for operations requiring minimal verbosity. The implementation
#  includes a main function for structured execution and a separate function for
#  argument parsing, enhancing readability and maintainability.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script with the directory path and the number of digits for padding as arguments.
#  Optionally, use the -q option to enable quiet mode, which suppresses logging output.
#  Example:
#      zero_padding.py /path/to/directory 4
#      zero_padding.py /path/to/directory 4 -q
#
#  The first command will rename files in '/path/to/directory', padding the numeric part to 4 digits,
#  with logging output. The second command does the same but in quiet mode (no logging output).
#
#  Requirements:
#  - Python Version: 3.2 or later
#
#  Version History:
#  v1.7 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2024-03-27
#       Modified the rename_files function to avoid renaming files that already
#       conform to the desired naming convention, enhancing efficiency.
#  v1.4 2024-01-20
#       Refactored to include a main function and separate argument parser setup function.
#  v1.3 2024-01-11
#       Added quiet mode option (-q) and refactored with logging and improved error handling.
#  v1.2 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#  v1.1 2023-05-06
#       Adjusted to fit the numeric part of file names to the specified number of digits.
#  v1.0 2023-02-28
#       Initial release. Basic functionality for zero-padding numeric parts of file names.
#
########################################################################

import argparse
import logging
import os
import sys


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

def setup_argument_parser():
    """ Set up and return the argument parser. """
    parser = argparse.ArgumentParser(
        description='A program to uniformly rename file names by zero-padding their numeric parts')
    parser.add_argument('dir_path', help='directory path')
    parser.add_argument('num_digits', type=int, help='number of digits for padding')
    parser.add_argument('-q', '--quiet', action='store_true', help='enable quiet mode')
    return parser

def setup_logger(quiet_mode):
    if quiet_mode:
        logging.basicConfig(level=logging.CRITICAL)
    else:
        logging.basicConfig(level=logging.INFO,
                            format='%(levelname)s: %(message)s')

def rename_files(dir_path, num_digits, quiet_mode):
    try:
        files = os.listdir(dir_path)
    except OSError as e:
        logging.error("Failed to list directory: %s", e)
        return

    # Process each file name
    for file_name in files:
        # Extract the numeric part of the file name
        name_parts = os.path.splitext(file_name)
        name_base = name_parts[0]
        name_ext = name_parts[1]
        num_part = ''
        for c in reversed(name_base):
            if not c.isdigit():
                break
            num_part = c + num_part
        if not num_part:
            continue

        # Fit the number part to the specified number of digits
        if len(num_part) > num_digits:
            new_num_part = num_part[-num_digits:]
        else:
            new_num_part = num_part.zfill(num_digits)

        new_name_base = name_base[:-len(num_part)] + new_num_part

        # Create a new file name and rename the file
        new_file_name = new_name_base + name_ext

        # Skip renaming if the file name will remain the same
        if new_file_name == file_name:
            continue

        old_file_path = os.path.join(dir_path, file_name)
        new_file_path = os.path.join(dir_path, new_file_name)

        try:
            os.rename(old_file_path, new_file_path)
            if not quiet_mode:
                logging.info("%s -> %s", file_name, new_file_name)
        except OSError as e:
            logging.error("Failed to rename %s to %s: %s", old_file_path, new_file_path, e)

def main():
    """ Main function to control the flow of the program. """
    parser = setup_argument_parser()
    args = parser.parse_args()

    setup_logger(args.quiet)

    if not os.path.isdir(args.dir_path):
        logging.error("%s is not a valid directory.", args.dir_path)
        return 1

    rename_files(args.dir_path, args.num_digits, args.quiet)
    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    if sys.version_info < (3, 2):
        print("[ERROR] This script requires Python 3.2 or later.", file=sys.stderr)
        sys.exit(9)

    sys.exit(main())
