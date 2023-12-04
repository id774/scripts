#!/usr/bin/env python

import argparse
import os

########################################################################
# File Renaming Script
#
#  Description:
#  This script renames files in a given directory by zero-padding the numeric
#  part of the file names to a specified number of digits.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 5/6,2023
#       Adjusted to fit the numeric part of file names to the specified number of digits.
#  v1.0 2/28,2023
#       Initial release. Basic functionality for zero-padding numeric parts of file names.
#
# Usage:
#  Run the script with the directory path and the number of digits for padding as arguments.
#  Example:
#      python zero_padding.py /path/to/directory 4
#  This will rename files in '/path/to/directory', padding the numeric part to 4 digits.
#
########################################################################

def rename_files(dir_path, num_digits):
    # Retrieve all files in the directory
    files = os.listdir(dir_path)

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
        print(f"{file_name} -> {new_file_name}")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='A program to uniformly rename file names by zero-padding their numeric parts')
    parser.add_argument('dir_path', help='directory path')
    parser.add_argument('num_digits', type=int, help='number of digits for padding')
    args = parser.parse_args()

    if not os.path.isdir(args.dir_path):
        print(f"{args.dir_path} is not a valid directory.")
        exit()

    rename_files(args.dir_path, args.num_digits)

