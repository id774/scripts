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
#  Notes:
#  - Ensure that you have the necessary permissions to read and write
#    files in the source directory.
#  - The script does not overwrite existing directories.
#
########################################################################

import os
import re
from optparse import OptionParser


def unzip_files(args, dry_run=False):
    for root, dirs, files in os.walk(args[0]):
        for f in files:
            d = re.sub(r"\.zip\Z", "", os.path.basename(f))
            target_dir = os.path.join(root, d)
            if os.path.exists(target_dir):
                continue

            if dry_run:
                print("[INFO] DRY RUN: Would unzip {} into {}".format(f, target_dir))
            else:
                os.mkdir(target_dir)
                os.chdir(target_dir)
                cmd = "unzip {}".format(os.path.join(args[0], f))
                os.system(cmd)
                os.chdir("..")

def main():
    parser = OptionParser(usage="usage: %prog [options] source_dir")
    parser.add_option("-d", "--dry-run", help="List files without extracting",
                      action="store_true", dest="dry_run", default=False)
    (options, args) = parser.parse_args()

    if len(args) < 1:
        parser.print_help()
    else:
        unzip_files(args, options.dry_run)


if __name__ == '__main__':
    main()
