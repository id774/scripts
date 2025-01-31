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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-31
#       Initial release.
#
#  Usage:
#  Run the script without arguments to list the current directory:
#      ./els.py
#
#  Specify a directory path to list its contents:
#      ./els.py /path/to/directory
#
#  Specify a file path to show its detailed information:
#      ./els.py /path/to/file
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
#  - The output is sorted by filename in ascending order when listing a directory.
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


def format_time(timestamp):
    """ Convert timestamp to 'YYYY-MM-DD HH:MM:SS' format """
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
    """ Retrieve and format file metadata for a given file or directory """
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

def list_files(paths):
    """ List details of the given files or directories """
    results = []
    for path in paths:
        if os.path.isfile(path):
            results.append(format_file_entry(path))
        elif os.path.isdir(path):
            try:
                with os.scandir(path) as entries:
                    results.extend([format_file_entry(entry.path) for entry in sorted(entries, key=lambda e: e.name)])
            except PermissionError:
                results.append({"error": "Permission denied: {}".format(path)})
            except FileNotFoundError:
                results.append({"error": "File or directory not found: {}".format(path)})
        else:
            results.append({"error": "Invalid path: {}".format(path)})
    return results

def main():
    """ Main function to handle CLI execution """
    target_paths = sys.argv[1:] if len(sys.argv) > 1 else ["."]
    results = list_files(target_paths)

    # Print header
    print("{:<10} {:>10} {:<10} {:<10} {:<19} {:<19} {:<19} {:<19} {}".format(
        "Mode", "Size", "Owner", "Group", "Atime", "Mtime", "Ctime", "Birth", "Name"
    ))
    print("-" * 150)

    # Print file entries
    for entry in results:
        if "error" in entry:
            print(entry["error"])
        else:
            print("{:<10} {:>10} {:<10} {:<10} {:<19} {:<19} {:<19} {:<19} {}".format(
                entry["mode"], entry["size"], entry["owner"], entry["group"],
                entry["atime"], entry["mtime"], entry["ctime"], entry["birth"], entry["name"]
            ))


if __name__ == "__main__":
    main()
