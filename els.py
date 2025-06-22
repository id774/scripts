#!/usr/bin/env python

########################################################################
# els.py: Extended File List with Detailed Timestamps
#
#  Description:
#  This script provides an extended file listing similar to `ls -l`, but
#  includes additional timestamp information such as access time (atime),
#  modification time (mtime), change time (ctime), and creation time (birth).
#  It displays detailed file metadata, including permissions, size, owner,
#  group, and timestamps, formatted for easy reading.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-01-31
#       Initial release.
#
#  Usage:
#  Run the script without arguments to list the current directory:
#      els.py
#
#  Specify a directory path to list its contents:
#      els.py /path/to/directory
#
#  Requirements:
#  - Python 3.x must be installed.
#  - The script must have execution permissions (`chmod +x els.py`).
#
#  Notes:
#  - This script retrieves timestamps using `os.stat()`. The `birth` time is
#    available only on macOS (APFS, HFS+). On Linux (ext4), it will display "N/A".
#  - File ownership is resolved using `pwd` and `grp` modules. If a user or group
#    is not found, the UID/GID is displayed instead.
#  - The file mode (permissions) is formatted using `stat.filemode()`, matching
#    the style of `ls -l`.
#  - The output is sorted by filename in ascending order.
#  - Symbolic links are listed as regular files without following the link.
#  - Large directories may take longer to process due to multiple system calls.
#  - If a directory is not readable due to permissions, the script will display an error.
#
########################################################################

import grp
import os
import pwd
import stat
import sys
import time

# Exported functions for testing and external usage
__all__ = ["format_time", "get_owner", "get_group", "format_file_entry", "get_file_info"]

def usage():
    script_path = os.path.abspath(__file__)
    in_header = False
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
    sys.exit(0)

def format_time(timestamp):
    """ Convert timestamp to 'YYYY-MM-DD HH:MM:SS' format in local time """
    if timestamp is None:
        raise ValueError("Timestamp cannot be None")
    return time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(timestamp))

def get_owner(uid):
    """ Convert UID to username """
    try:
        return pwd.getpwuid(uid).pw_name
    except KeyError:
        return str(uid)

def get_group(gid):
    """ Convert GID to group name """
    try:
        return grp.getgrgid(gid).gr_name
    except KeyError:
        return str(gid)

def format_file_entry(path):
    """ Retrieve and format file metadata for a single file """
    st = os.stat(path)

    return {
        "mode": stat.filemode(st.st_mode),
        "size": st.st_size,
        "owner": get_owner(st.st_uid),
        "group": get_group(st.st_gid),
        "atime": format_time(st.st_atime),  # Last access time
        "mtime": format_time(st.st_mtime),  # Last modification time
        "ctime": format_time(st.st_ctime),  # Last metadata change time
        "birth": format_time(st.st_birthtime) if hasattr(st, "st_birthtime") else "N/A",
        "name": os.path.basename(path)
    }

def get_file_info(path="."):
    """ Get file metadata, whether it's a single file or a directory listing """
    if not os.path.exists(path):
        return "[ERROR] '{}' does not exist.".format(path)

    try:
        if os.path.isfile(path):
            return [format_file_entry(path)]  # Return as a list for consistency
        elif os.path.isdir(path):
            # Check if os.scandir() exists before trying to use it
            if hasattr(os, "scandir"):
                entry = os.scandir(path)  # Call to test existence
                if hasattr(entry, "__exit__"):  # Check if context manager is supported
                    with os.scandir(path) as entries:
                        return [format_file_entry(entry.path) for entry in sorted(entries, key=lambda e: e.name)]
            # If os.scandir() is unavailable or lacks __exit__, fallback to os.listdir()
            return [format_file_entry(os.path.join(path, entry)) for entry in sorted(os.listdir(path))]
        else:
            return "[ERROR] '{}' is neither a file nor a directory.".format(path)
    except PermissionError:
        return "[ERROR] Permission denied for '{}'.".format(path)

def main():
    """ Main function to handle CLI execution """
    target_path = sys.argv[1] if len(sys.argv) > 1 else "."
    result = get_file_info(target_path)

    if isinstance(result, str):
        print(result)
        sys.exit(1)

    # Print header
    print("{:<10} {:>10} {:<10} {:<10} {:<19} {:<19} {:<19} {:<19} {}".format(
        "Mode", "Size", "Owner", "Group", "Atime", "Mtime", "Ctime", "Birth", "Name"
    ))
    print("-" * 150)

    # Print file entries
    for entry in result:
        print("{:<10} {:>10} {:<10} {:<10} {:<19} {:<19} {:<19} {:<19} {}".format(
            entry["mode"], entry["size"], entry["owner"], entry["group"],
            entry["atime"], entry["mtime"], entry["ctime"], entry["birth"], entry["name"]
        ))


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    main()
