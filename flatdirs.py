#!/usr/bin/env python

########################################################################
# flatdirs.py: Flatten Directories and Manipulate Files
#
#  Description:
#  This script flattens the directory structure by either moving, copying,
#  or renaming files to the base directory. It supports deletion of empty
#  directories and can operate in a quiet mode. If no options are specified,
#  the script displays a help message and exits. A target directory must be
#  explicitly specified using the --target option.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.9 2024-12-12
#       Added --target option to require explicit target directory specification.
#  v1.8 2024-03-22
#       Updated to display a help message and exit if no options are specified.
#  v1.7 2024-03-05
#       Fixed issue with deleting directories containing subdirectories.
#  v1.6 2024-01-20
#       Refactored to encapsulate option parser configuration in a separate function.
#  v1.5 2024-01-13
#       Changed script name to flatdirs.py for simplicity.
#  v1.4 2024-01-11
#       Refactored handle_directory function to accept options as a parameter.
#  v1.3 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#       Modified behavior to require at least one option and display help message otherwise.
#  v1.2 2023-12-07
#       Enhanced dry-run mode output for clarity.
#  v1.1 2023-09-11
#       Added rename-only mode.
#  v1.0 2023-06-27
#       Initial release.
#
#  Usage:
#  python flatdirs.py --target <directory> [options]
#  Options:
#    -m, --move         Move files instead of copying (default if no option is provided)
#    -c, --copy         Copy files instead of moving
#    -d, --delete       Delete empty directories
#    -q, --quiet        Suppress operation info
#    -x, --execute      Execute file operations (default is dry run)
#    -r, --rename-only  Only rename files, without moving or copying
#    -t, --target       Specify the target directory to flatten (required)
#
#  Notes:
#  - Use with caution as it can significantly modify directory contents.
#  - It's recommended to backup data before executing with the --execute option.
#
########################################################################

import os
import shutil
from optparse import OptionParser


def setup_option_parser():
    """ Set up command-line options using OptionParser. """
    parser = OptionParser()
    parser.add_option("-m", "--move", action="store_true", dest="move_mode", default=False,
                      help="move files instead of copying them")
    parser.add_option("-c", "--copy", action="store_true", dest="copy_mode", default=False,
                      help="copy files instead of moving them")
    parser.add_option("-d", "--delete", action="store_true", dest="delete_mode", default=False,
                      help="delete empty directories")
    parser.add_option("-q", "--quiet", action="store_true", dest="quiet_mode", default=False,
                      help="suppress operation info")
    parser.add_option("-x", "--execute", action="store_true", dest="execute_mode", default=False,
                      help="execute file operations (default is dry run)")
    parser.add_option("-r", "--rename-only", action="store_true", dest="rename_only_mode", default=False,
                      help="only rename the files by adding directory name, without moving or copying")
    parser.add_option("-t", "--target", dest="target_dir", help="specify the target directory to flatten")
    return parser

def print_action(action, source, destination=None, options=None):
    """ Prints the action being performed or simulated. """
    action_message = "{}".format(action) + " {}".format(source)
    if destination:
        action_message += " -> {}".format(destination)

    if options and not options.execute_mode:
        print("[DRY RUN] {}".format(action_message))
    else:
        print(action_message)

def handle_directory(path, options):
    """ Recursively processes a directory. """
    try:
        entries = os.listdir(path)
    except FileNotFoundError:
        return

    for entry in entries:
        old_path = os.path.join(path, entry)
        if os.path.isdir(old_path):
            handle_directory(old_path, options)
            try:
                if options.delete_mode and not os.listdir(old_path):
                    if options.execute_mode:
                        os.rmdir(old_path)
                    if not options.quiet_mode:
                        print_action("Deleted empty directory", old_path, options=options)
            except FileNotFoundError:
                print("Warning: Directory not found during deletion - {}".format(old_path))
        else:
            try:
                new_filename = "{}_{}".format(path.replace('/', '_'), entry)
                new_path = os.path.join(options.target_dir, new_filename)

                if options.rename_only_mode:
                    if options.execute_mode:
                        os.rename(old_path, new_path)
                    if not options.quiet_mode:
                        print_action("Renamed", old_path, new_path, options)
                elif options.copy_mode or (not options.move_mode and not options.copy_mode):
                    if options.execute_mode:
                        shutil.copy(old_path, new_path)
                    if not options.quiet_mode:
                        print_action("Copied", old_path, new_path, options)
                elif options.move_mode:
                    if options.execute_mode:
                        shutil.move(old_path, new_path)
                    if not options.quiet_mode:
                        print_action("Moved", old_path, new_path, options)
            except FileNotFoundError:
                print("Warning: File not found during operation - {}".format(old_path))
            except PermissionError:
                print("Warning: Permission denied - {}".format(old_path))

def main(options):
    """ Main function to process directories. Check if any options are set; if not, display help. """
    # Ensure target directory is specified
    if not options.target_dir:
        print("Error: The --target option is required.")
        parser.print_help()
        exit(1)

    # Ensure the target directory exists
    if not os.path.exists(options.target_dir):
        print("Error: The target directory '{}' does not exist.".format(options.target_dir))
        exit(1)

    # Process each subdirectory in the target directory
    subdirectories = [d for d in os.listdir(options.target_dir) if os.path.isdir(os.path.join(options.target_dir, d))]
    for subdir in subdirectories:
        handle_directory(os.path.join(options.target_dir, subdir), options)


if __name__ == '__main__':
    parser = setup_option_parser()
    (options, args) = parser.parse_args()
    main(options)
