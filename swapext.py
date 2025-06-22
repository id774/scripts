#!/usr/bin/env python

########################################################################
# swapext.py: File Extension Swapper
#
#  Description:
#  This script changes the file extensions within a specified directory.
#  It walks through the directory, renaming files from one extension to another.
#  It now includes dry-run capability and safety confirmation before execution.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-04-15
#       Replaced sys.argv parsing with OptionParser.
#       Added -x option to enable execution (default is dry-run).
#       Added confirmation prompt before executing changes.
#  v1.3 2024-01-11
#       Added '-q' option for quiet mode.
#  v1.2 2024-01-10
#       Updated with enhanced error handling, logging, and refactored functions.
#  v1.1 2014-08-14
#       Minor formatting revisions for readability and consistency.
#  v1.0 2011-04-19
#       Initial release.
#
#  Usage:
#  swapext.py <dir> <before_ext> <after_ext> [-x] [-q]
#
#  Options:
#    -x    Execute mode (default is dry-run)
#    -q    Quiet mode (suppress output)
#
########################################################################

import os
import sys
from optparse import OptionParser


def usage():
    script_path = os.path.abspath(__file__)
    in_usage = False
    with open(script_path, 'r', encoding='utf-8') as f:
        for line in f:
            if line.startswith('#  Usage:'):
                in_usage = True
                print(line[2:].strip())
                continue
            if in_usage:
                if line.startswith('#' * 10):
                    break
                if line.startswith('#'):
                    print(line[2:].strip())
    sys.exit(0)

def validate_args(args):
    if len(args) != 3:
        parser.error("Invalid number of arguments")

    target_dir, before_ext, after_ext = args

    if not os.path.isdir(target_dir):
        print("[ERROR] Specified directory does not exist: {}".format(target_dir), file=sys.stderr)
        sys.exit(1)

    if not before_ext.startswith('.') or not after_ext.startswith('.'):
        print("[ERROR] Extensions must start with a '.'", file=sys.stderr)
        sys.exit(1)

    return target_dir, before_ext, after_ext

def confirm_execution(target_dir):
    print("[INFO] Target directory: {}".format(os.path.abspath(target_dir)))
    confirmation = input("Are you sure you want to execute the rename operations in this directory? (yes/no): ").strip().lower()
    if confirmation not in ('yes', 'y'):
        print("[ERROR] Execution cancelled.", file=sys.stderr)
        sys.exit(1)

def rename_file(old_path, new_path, dry_run, quiet_mode):
    if not quiet_mode:
        msg = "Renamed: {} -> {}".format(old_path, new_path)
        if dry_run:
            print("[INFO] DRY RUN: " + msg)
        else:
            print("[INFO] " + msg)

    if not dry_run:
        try:
            os.rename(old_path, new_path)
        except OSError as e:
            print("[ERROR] Error renaming {} to {}: {}".format(old_path, new_path, e), file=sys.stderr)
            sys.exit(1)

def swap_extensions(target_dir, before_ext, after_ext, dry_run, quiet_mode):
    for path, _, files in os.walk(target_dir):
        for oldfile in files:
            if oldfile.endswith(before_ext):
                base_name = os.path.splitext(oldfile)[0]
                newfile = base_name + after_ext
                old_path = os.path.join(path, oldfile)
                new_path = os.path.join(path, newfile)
                rename_file(old_path, new_path, dry_run, quiet_mode)

def main():
    usage = "usage: %prog <dir> <before_ext> <after_ext> [-x] [-q]"
    parser = OptionParser(usage=usage)
    parser.add_option("-x", action="store_true", dest="execute_mode", default=False,
                      help="execute rename operations (default is dry-run)")
    parser.add_option("-q", action="store_true", dest="quiet_mode", default=False,
                      help="suppress output")
    (options, args) = parser.parse_args()

    if len(args) != 3:
        parser.error("Invalid number of arguments")

    target_dir, before_ext, after_ext = validate_args(args)

    if options.execute_mode:
        confirm_execution(target_dir)

    swap_extensions(target_dir, before_ext, after_ext, not options.execute_mode, options.quiet_mode)


if __name__ == '__main__':
    if len(sys.argv) == 1 or sys.argv[1] in ('-h', '--help'):
        usage()
    main()
