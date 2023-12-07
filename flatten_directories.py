#!/usr/bin/env python

########################################################################
# flatten_directories.py: Flatten Directories and Manipulate Files
#
#  Description:
#  This script flattens the directory structure by either moving, copying,
#  or renaming files to the base directory. It supports deletion of empty
#  directories and can operate in a quiet mode. It also includes a dry-run
#  mode to simulate file operations without making actual changes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#  v1.2 2023-12-07
#       Enhanced dry-run mode output for clarity.
#  v1.1 2023-09-11
#       Added rename-only mode.
#  v1.0 2023-06-27
#       Initial release.
#
#  Usage:
#  python flatten_directories.py [options]
#  Options:
#    -m, --move         Move files instead of copying
#    -d, --delete       Delete empty directories
#    -q, --quiet        Suppress operation info
#    -x, --execute      Execute file operations
#    -r, --rename-only  Only rename files, without moving or copying
#
#  Notes:
#  - Use with caution as it can significantly modify directory contents.
#  - It's recommended to backup data before executing with the --execute option.
#
########################################################################

import os
import shutil
from optparse import OptionParser

# Set up command-line options using OptionParser.
parser = OptionParser()
parser.add_option("-m", "--move", action="store_true", dest="move_mode", default=False,
                  help="move files instead of copying them")
parser.add_option("-d", "--delete", action="store_true", dest="delete_mode", default=False,
                  help="delete empty directories")
parser.add_option("-q", "--quiet", action="store_true", dest="quiet_mode", default=False,
                  help="suppress operation info")
parser.add_option("-x", "--execute", action="store_true", dest="execute_mode", default=False,
                  help="execute file operations")
parser.add_option("-r", "--rename-only", action="store_true", dest="rename_only_mode", default=False,
                  help="only rename the files by adding directory name, without moving or copying")
(options, args) = parser.parse_args()

def print_action(action, source, destination=None):
    """ Prints the action being performed or simulated. """
    action_message = "{} {}".format(action, source)
    if destination:
        action_message += " -> {}".format(destination)

    if options.execute_mode:
        print(action_message)
    else:
        print("[DRY RUN] {}".format(action_message))

def handle_directory(path):
    """Recursively processes a directory."""
    entries = os.listdir(path)

    for entry in entries:
        old_path = os.path.join(path, entry)
        if os.path.isdir(old_path):
            handle_directory(old_path)
        else:
            new_filename = "{}_{}".format(path.replace('/', '_'), entry)
            new_path = os.path.join(path, new_filename)

            if options.rename_only_mode:
                if options.execute_mode:
                    os.rename(old_path, new_path)
                if not options.quiet_mode:
                    print_action("Renamed", old_path, new_path)
            elif options.move_mode:
                if options.execute_mode:
                    shutil.move(old_path, new_filename)
                if not options.quiet_mode:
                    print_action("Moved", old_path, new_filename)
            else:
                if options.execute_mode:
                    shutil.copy(old_path, new_filename)
                if not options.quiet_mode:
                    print_action("Copied", old_path, new_filename)

    if options.delete_mode and not os.listdir(path):
        if options.execute_mode:
            os.rmdir(path)
        if not options.quiet_mode:
            print_action("Deleted directory", path)


subdirectories = [d for d in os.listdir('.') if os.path.isdir(d)]
for subdir in subdirectories:
    handle_directory(subdir)
