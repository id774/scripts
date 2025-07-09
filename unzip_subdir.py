#!/usr/bin/env python

########################################################################
# unzip_subdir.py: Unzip Files to Subdirectories
#
#  Description:
#  This script unzips each .zip file in the specified source directory
#  into separate subdirectories. If a subdirectory already exists, it
#  skips the extraction for that zip file. The --dry-run option lists
#  the zip files that would be extracted without performing the actual
#  extraction.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.6 2025-07-08
#       Fixed compatibility issues with Python 3.4.
#  v1.5 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-14
#       Fixed SyntaxWarning by converting regex string to raw format in re.sub().
#  v1.2 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#  v1.1 2023-12-06
#       Added --dry-run option and enhanced documentation.
#  v1.0 2010-02-14
#       Initial release.
#
#  Usage:
#  unzip_subdir.py [options] source_dir
#  Options:
#    -d, --dry-run: List files without extracting
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Notes:
#  - Ensure that you have the necessary permissions to read and write
#    files in the source directory.
#  - The script does not overwrite existing directories.
#
########################################################################

import os
import re
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

def unzip_files(args, dry_run=False):
    for root, dirs, files in os.walk(args[0]):
        for f in files:
            if not f.lower().endswith(".zip"):
                continue

            d = re.sub(r"\.zip\Z", "", os.path.basename(f), flags=re.IGNORECASE)
            target_dir = os.path.join(root, d)
            if os.path.exists(target_dir):
                continue

            if dry_run:
                print("[INFO] DRY RUN: Would unzip {} into {}".format(f, target_dir))
            else:
                try:
                    os.mkdir(target_dir)
                    current = os.getcwd()
                    os.chdir(target_dir)
                    cmd = "unzip {}".format(os.path.join(args[0], f))
                    os.system(cmd)
                    os.chdir(current)
                except Exception as e:
                    print("Error unzipping {}: {}".format(f, str(e)), file=sys.stderr)


def main():
    parser = OptionParser(usage="usage: %prog [options] source_dir")
    parser.add_option("-d", "--dry-run", help="List files without extracting",
                      action="store_true", dest="dry_run", default=False)
    (options, args) = parser.parse_args()

    if len(args) < 1:
        parser.print_help()
        return 1
    else:
        unzip_files(args, options.dry_run)
        return 0


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main())
