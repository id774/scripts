#!/usr/bin/env python

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

def handle_directory(path):
    """Recursively processes a directory."""
    # Get the list of files and subdirectories in the directory.
    entries = os.listdir(path)

    for entry in entries:
        old_path = os.path.join(path, entry)

        # If the entry is a directory, process it recursively.
        if os.path.isdir(old_path):
            handle_directory(old_path)
        else:
            # Form the new filename by adding the directory name.
            new_filename = f"{path.replace('/', '_')}_{entry}"

            # If rename-only mode is active, rename the file.
            if options.rename_only_mode:
                if options.execute_mode:
                    os.rename(old_path, os.path.join(path, new_filename))
                if not options.quiet_mode:
                    print(f"Renamed {old_path} -> {os.path.join(path, new_filename)}")
            # If move mode is active, move the file.
            elif options.move_mode:
                if options.execute_mode:
                    shutil.move(old_path, new_filename)
                if not options.quiet_mode:
                    print(f"Moved {old_path} -> {new_filename}")
            # Otherwise, copy the file.
            else:
                if options.execute_mode:
                    shutil.copy(old_path, new_filename)
                if not options.quiet_mode:
                    print(f"Copied {old_path} -> {new_filename}")

    # If delete mode is active and the directory is empty, delete it.
    if options.delete_mode and not os.listdir(path):
        if options.execute_mode:
            os.rmdir(path)
        if not options.quiet_mode:
            print(f"Deleted directory {path}")

# Process all subdirectories in the current directory.
subdirectories = [d for d in os.listdir('.') if os.path.isdir(d)]
for subdir in subdirectories:
    handle_directory(subdir)

