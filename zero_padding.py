#!/usr/bin/env python

########################################################################
# zero_padding.py: File Renaming Script
#
#  Description:
#  This script renames files in a given directory by zero-padding the numeric
#  part of the file names to a specified number of digits. It includes
#  a quiet mode option to suppress logging output and has improved structure
#  with the addition of a main function and separate argument parser setup.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  Usage:
#  Run the script with the directory path and the number of digits for padding as arguments.
#  Optionally, use the -q option to enable quiet mode, which suppresses logging output.
#  Example:
#      python zero_padding.py /path/to/directory 4
#      python zero_padding.py /path/to/directory 4 -q
#  The first command will rename files in '/path/to/directory', padding the numeric part to 4 digits,
#  with logging output. The second command does the same but in quiet mode (no logging output).
#
########################################################################

import argparse
import logging
import os


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
        old_file_path = os.path.join(dir_path, file_name)
        new_file_path = os.path.join(dir_path, new_file_name)
        os.rename(old_file_path, new_file_path)

        # Display the renaming
        if not quiet_mode:
            logging.info("%s -> %s", file_name, new_file_name)

        try:
            os.rename(old_file_path, new_file_path)
        except OSError as e:
            logging.error("Failed to rename %s to %s: %s",
                          old_file_path, new_file_path, e)

def main():
    """ Main function to control the flow of the program. """
    parser = setup_argument_parser()
    args = parser.parse_args()

    setup_logger(args.quiet)

    if not os.path.isdir(args.dir_path):
        logging.error("%s is not a valid directory.", args.dir_path)
        exit(1)

    rename_files(args.dir_path, args.num_digits, args.quiet)


if __name__ == '__main__':
    main()
