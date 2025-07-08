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
#  v2.3 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v2.2 2025-06-30
#       Added checks: equal extensions, dir write access, conflict overwrite, zero-match warning.
#       Clarified extension format in usage and error message.
#  v2.1 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
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
#     (Note: Extensions must start with a dot, e.g., '.txt' '.md')
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

def validate_args(args):
    if len(args) != 3:
        parser.error("Invalid number of arguments")

    target_dir, before_ext, after_ext = args

    if not os.path.isdir(target_dir):
        print("[ERROR] Specified directory does not exist: {}".format(target_dir), file=sys.stderr)
        sys.exit(1)

    if not os.access(target_dir, os.R_OK):
        print("[ERROR] Cannot read from directory: {}".format(target_dir), file=sys.stderr)
        sys.exit(1)

    if not os.access(target_dir, os.W_OK):
        print("[ERROR] Cannot write to directory: {}".format(target_dir), file=sys.stderr)
        sys.exit(1)

    if not before_ext.startswith('.') or not after_ext.startswith('.'):
        print("[ERROR] Extensions must start with a '.' (e.g., '.txt', '.md')", file=sys.stderr)
        sys.exit(1)

    if before_ext == after_ext:
        print("[ERROR] Before and after extensions are the same. Nothing to do.", file=sys.stderr)
        sys.exit(1)

    return target_dir, before_ext, after_ext

def confirm_execution(target_dir):
    print("[INFO] Target directory: {}".format(os.path.abspath(target_dir)))
    confirmation = input("Are you sure you want to execute the rename operations in this directory? (yes/no): ").strip().lower()
    if confirmation not in ('yes', 'y'):
        print("[ERROR] Execution cancelled.", file=sys.stderr)
        sys.exit(1)

def rename_file(old_path, new_path, dry_run, quiet_mode):
    if os.path.exists(new_path):
        print("[ERROR] Target file already exists: {}".format(new_path), file=sys.stderr)
        sys.exit(1)

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
    count = 0
    for path, _, files in os.walk(target_dir):
        for oldfile in files:
            if oldfile.endswith(before_ext):
                base_name = os.path.splitext(oldfile)[0]
                newfile = base_name + after_ext
                old_path = os.path.join(path, oldfile)
                new_path = os.path.join(path, newfile)
                rename_file(old_path, new_path, dry_run, quiet_mode)
                count += 1

    if count == 0:
        print("[WARN] No files with extension {} found.".format(before_ext), file=sys.stderr)

def main():
    usage_msg = "usage: %prog <dir> <before_ext> <after_ext> [-x] [-q]"
    global parser
    parser = OptionParser(usage=usage_msg)
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

    return 0


if __name__ == '__main__':
    if len(sys.argv) < 4 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())
