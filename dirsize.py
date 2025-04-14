#!/usr/bin/env python

########################################################################
# dirsize.py: Directory Size Summary Script
#
#  Description:
#  This script displays a detailed list of files and directories in the
#  specified directory (or the current directory if none is specified),
#  including hidden files, and calculates the total size of these files
#  in human-readable format (KiB, MiB, GiB, TiB, PiB, EiB, ZiB, YiB, RiB, QiB).
#  It's useful for quickly estimating the size of contents in a directory
#  without including subdirectories. The script does not recursively account
#  for sizes within subdirectories and is designed for simplicity, providing
#  a clear overview of the immediate directory contents.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.1 2024-08-07
#       Added -h option to display help. Modified script to show help when no directory is specified.
#  v1.0 2023-12-19
#       Initial release. Displays directory contents and calculates total size
#       using binary prefixes.
#
#  Usage:
#  ./dirsize.py [directory]
#  If no directory is specified, the current directory is used.
#
########################################################################

import os
import subprocess
import sys
from decimal import Decimal, getcontext


def convert_size(size_bytes):
    if size_bytes == 0:
        return "0B"
    i = 0
    prefix = ["B", "KiB", "MiB", "GiB", "TiB",
              "PiB", "EiB", "ZiB", "YiB", "RiB", "QiB"]
    size_bytes = Decimal(size_bytes)
    getcontext().prec = 50

    while size_bytes >= Decimal(1024) and i < len(prefix) - 1:
        size_bytes /= Decimal(1024)
        i += 1

    return "{:.2f} {}".format(size_bytes, prefix[i])

def display_help():
    print("""
Usage: ./dirsize.py [options] [directory]

Options:
  -h                Display this help message and exit

If no directory is specified, it displays this help message.
To check the current directory, use:
  ./dirsize.py .
""")

def main():
    if len(sys.argv) == 1:
        display_help()
        sys.exit(0)
    elif sys.argv[1] == '-h':
        display_help()
        sys.exit(0)

    directory = sys.argv[1]

    # Check if the directory exists
    if not os.path.exists(directory):
        print("[ERROR] Directory '{}' does not exist.".format(directory), file=sys.stderr)
        sys.exit(1)

    total_size = 0

    try:
        # Run ls -all command and print output
        subprocess.call(["ls", "-all", directory])

        # Calculate total size of files in directory
        for entry in os.scandir(directory):
            if entry.is_file():
                total_size += entry.stat().st_size

        print("[INFO] Total size:", convert_size(total_size))
    except Exception as e:
        print("[ERROR]", e, file=sys.stderr)


if __name__ == "__main__":
    main()
