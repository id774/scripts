#!/usr/bin/env python

import os
import shutil
from optparse import OptionParser

# Add options using OptParser.
parser = OptionParser()
parser.add_option("-m", "--move", action="store_true", dest="move_mode", default=False,
                  help="move files instead of copying them")
parser.add_option("-d", "--delete", action="store_true", dest="delete_mode", default=False,
                  help="delete empty directories")
parser.add_option("-q", "--quiet", action="store_true", dest="quiet_mode", default=False,
                  help="suppress operation info")
parser.add_option("-x", "--execute", action="store_true", dest="execute_mode", default=False,
                  help="execute file operations")
(options, args) = parser.parse_args()

def handle_directory(path):
    # Get files and subdirectories in the directory.
    entries = os.listdir(path)

    # Perform operations for each entry.
    for entry in entries:
        old_path = os.path.join(path, entry)

        # If the entry is a directory, process it recursively.
        if os.path.isdir(old_path):
            handle_directory(old_path)
        else:
            # If the entry is a file, create a new filename and move or copy it.
            new_filename = f"{path.replace('/', '_')}_{entry}"
            if options.move_mode:
                if options.execute_mode:
                    shutil.move(old_path, new_filename)
                if not options.quiet_mode:
                    print(f"Moved {old_path} -> {new_filename}")
            else:
                if options.execute_mode:
                    shutil.copy(old_path, new_filename)
                if not options.quiet_mode:
                    print(f"Copied {old_path} -> {new_filename}")

    # If the directory is empty and the delete option is active, delete it.
    if options.delete_mode and not os.listdir(path):
        if options.execute_mode:
            os.rmdir(path)
        if not options.quiet_mode:
            print(f"Deleted directory {path}")

# Get all subdirectories in the current directory and start processing.
subdirectories = [d for d in os.listdir('.') if os.path.isdir(d)]
for subdir in subdirectories:
    handle_directory(subdir)

