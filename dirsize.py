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
#  v1.4 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.1 2024-08-07
#       Added -h option to display help. Modified script to show help when no directory is specified.
#  v1.0 2023-12-19
#       Initial release. Displays directory contents and calculates total size
#       using binary prefixes.
#
#  Usage:
#      dirsize.py [options] [directory]
#
#  Options:
#    -h                Display this help message and exit
#
#  If no directory is specified, it displays this help message.
#  To check the current directory, use:
#    ./dirsize.py .
#
########################################################################

import os
import subprocess
import sys
from decimal import Decimal, getcontext


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

def main():
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help'):
        usage()

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
        return 0
    except Exception as e:
        print("[ERROR]", e, file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
