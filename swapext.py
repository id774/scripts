#!/usr/bin/env python

########################################################################
# swapext.py: File Extension Swapper
#
#  Description:
#  This script changes the file extensions within a specified directory.
#  It walks through the directory, renaming files from one extension to another.
#  This is useful for batch processing file extensions in a directory.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-01-10
#       Updated with enhanced error handling, logging, and refactored functions.
#  v1.1 2014-08-14
#       Minor formatting revisions for readability and consistency.
#  v1.0 2011-04-19
#       Initial release. Basic functionality for swapping file extensions in a directory tree.
#
#  Usage:
#  Run the script with the directory and extensions:
#      python swapext.py dir before_extension after_extension
#
#  Example:
#      python swapext.py ./images jpg png
#  This will change all '.jpg' files to '.png' within the './images' directory.
#
########################################################################

import os
import sys
import logging

# Logger configuration
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

def validate_args(dir, before_ext, after_ext):
    if not os.path.isdir(dir):
        logging.error("Specified directory does not exist: %s", dir)
        sys.exit(1)

    if not before_ext.startswith('.') or not after_ext.startswith('.'):
        logging.error("Extensions must start with a '.'")
        sys.exit(1)

def rename_file(old_path, new_path):
    try:
        os.rename(old_path, new_path)
        logging.info("Renamed: %s -> %s", old_path, new_path)
    except OSError as e:
        logging.error("Error renaming %s to %s: %s", old_path, new_path, e)

def swap_extensions(dir, before_ext, after_ext):
    if not after_ext.startswith('.'):
        after_ext = '.' + after_ext

    for path, subdirs, files in os.walk(dir):
        for oldfile in files:
            if oldfile.endswith(before_ext):
                base_name = os.path.splitext(oldfile)[0]
                newfile = base_name + after_ext
                rename_file(os.path.join(path, oldfile),
                            os.path.join(path, newfile))

def main():
    if len(sys.argv) != 4:
        logging.error(
            "Usage: swapext.py <dir> <before_extension> <after_extension>")
        sys.exit(1)

    dir, before_ext, after_ext = sys.argv[1], sys.argv[2], sys.argv[3]
    validate_args(dir, before_ext, after_ext)
    swap_extensions(dir, before_ext, after_ext)


if __name__ == '__main__':
    main()
