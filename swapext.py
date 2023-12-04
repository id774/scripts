#!/usr/bin/env python
#
########################################################################
# swapext: File Extension Swapper
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
#  v1.1 8/14,2014
#       Minor formatting revisions for readability and consistency.
#  v1.0 4/19,2011
#       Initial release. Basic functionality for swapping file extensions in a directory tree.
#
# Usage:
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

def swap_extensions(dir, before, after):
    if before[:1] != '.':
        before = '.' + before

    thelen = -len(before)
    if after[:1] != '.':
        after = '.' + after

    for path, subdirs, files in os.walk(dir):
        for oldfile in files:
            if oldfile[thelen:] == before:
                oldfile = os.path.join(path, oldfile)
                newfile = oldfile[:thelen] + after
                os.rename(oldfile, newfile)
                print(oldfile + " -> " + newfile)

def main():
    if len(sys.argv) != 4:
        print("Usage: swapext dir before after")
        sys.exit(1)

    swap_extensions(sys.argv[1], sys.argv[2], sys.argv[3])

if __name__ == '__main__':
    main()
