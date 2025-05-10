#!/bin/sh

########################################################################
# rsync_backup.sh: Backup and Syncing Removable Disk Script
#
#  Description:
#  This script facilitates the backup and syncing of data to removable
#  disks. It includes functionalities like checking disk health, updating
#  timestamps, performing cleanup tasks, and syncing data between disks and
#  over SSH.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.6  2025-05-10 - Add cron execution check and usage support with unified structure.
#  v2.5  2025-04-20 - Fix inaccurate rsync return code logging by
#                     assigning RC immediately after execution.
#  v2.4  2025-04-17 - Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.3  2025-03-19 - Improved code readability by standardizing function indentation.
#                     Encapsulated script logic in a `main` function for better structure.
#                     Ensured proper preservation of return codes by storing `$?` in a
#                     variable before use.
#  v2.2  2023-12-23 - Refactored for POSIX compliance. Replaced Bash-specific syntax
#                     with POSIX standard commands and structures. Enhanced portability
#                     and compatibility across different UNIX-like systems.
#  v2.1  2023-12-17 - Refactored script to separate logic and operations.
#                     Operations are now defined in an external file
#                     'etc/rsync_backup.conf' for enhanced modularity and
#                     maintainability.
#                     Integrated github-arc.sh and cleanup-junk-files.sh scripts.
#  v2.0  2023-07-04 - Major version upgrade with no functional changes.
#  v1.27 2023-07-02 - Convert script to POSIX-compatible syntax. Show return
#                     code 1 if required directory does not exist.
#  [Further version history truncated for brevity]
#  2023 - Several refinements, including POSIX-compatible syntax and return code adjustments.
#         Implementation of disk health check immediately after backup.
#  2016 - Enhanced target host checking and filesystem ownership issues fixed.
#  2013 - Device definition bugs fixed and directories re-constructed.
#  2011 - Backup functionalities for git repositories and GitHub.
#  2010 - Improvements in error handling and SSH usage.
#  2009 - Addition of rsync function for portable media devices and SMART information.
#  2008 - Initial stable release, with basic backup and sync functionalities.
#  v1.0 2008-02-28 - Stable initial release.
#
#  Usage:
#  Run the script without any arguments. Ensure all the necessary paths
#  and variables are correctly set within the script:
#      test -x /root/bin/rsync_backup.sh && /root/bin/rsync_backup.sh>>$JOBLOG 2>&1;
#
#  The script will automatically execute operations based on the configured
#  settings and available devices.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check if the script is running from cron
is_running_from_cron() {
    if tty -s; then
        return 1  # Terminal attached → interactive session
    else
        return 0  # No terminal → likely cron
    fi
}

# Function to display the timestamp of the last backup and update it
display_and_update_timestamp() {
    if [ -f "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp" ]; then
        echo "[INFO] ls -l $T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
        ls -l "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
    fi
    echo "[INFO] touch $T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
    touch "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
}

# Function to display the installed versions of TrueCrypt and VeraCrypt
version_info() {
    if [ -x /usr/bin/truecrypt ]; then
        echo "[INFO] Checking installed version of TrueCrypt."
        /usr/bin/truecrypt -t --version
    fi
    if [ -x /usr/bin/veracrypt ]; then
        echo "[INFO] Checking installed version of VeraCrypt."
        /usr/bin/veracrypt -t --version
    fi
}

# Function to retrieve SMART information of the backup and target devices
smart_info() {
    if [ -b /dev/$B_DEVICE ]; then
        echo "[INFO] smartctl -a /dev/$B_DEVICE"
        smartctl -a /dev/$B_DEVICE
    fi
    if [ -b /dev/$T_DEVICE ]; then
        echo "[INFO] smartctl -a /dev/$T_DEVICE"
        smartctl -a /dev/$T_DEVICE
    fi
}

# Function to perform a SMART diagnostic check on the target device
smart_check() {
    if [ -b /dev/$T_DEVICE ]; then
        if [ -f "$T_HOME/$T_MOUNT/$T_DEVICE/smart_longtest" ]; then
            touch "$T_HOME/$T_MOUNT/$T_DEVICE/smart_longtest"
            echo "[INFO] smartctl -t long /dev/$T_DEVICE"
            smartctl -t long /dev/$T_DEVICE
        else
            touch "$T_HOME/$T_MOUNT/$T_DEVICE/smart_shorttest"
            echo "[INFO] smartctl -t short /dev/$T_DEVICE"
            smartctl -t short /dev/$T_DEVICE
        fi
    else
        echo "[WARN] The device /dev/$T_DEVICE does not exist or is not a block device." >&2
    fi
}

# Function to show disk space usage of backup directories
show_capacity_of_directories() {
    if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" ]; then
        echo "[INFO] Disk usage of $B_HOME/$B_MOUNT/$B_DEVICE."
        du -h --max-depth=2 "$B_HOME/$B_MOUNT/$B_DEVICE"
    fi
}

# Function to remove unnecessary files such as macOS metadata and temp files
cleanup() {
    echo "[INFO] Removing junk files in $B_HOME/$B_MOUNT/$B_DEVICE..."
    echo "[INFO] Removing ._* AppleDouble files..."
    find "$B_HOME/$B_MOUNT/$B_DEVICE" -name '._*' -exec rm -vf {} \;
    echo "[INFO] Removing .DS_Store files..."
    find "$B_HOME/$B_MOUNT/$B_DEVICE" -name '.DS_Store' -exec rm -vf {} \;
    echo "[INFO] Removing temporary Unix files ending with '.un~'..."
    find "$B_HOME/$B_MOUNT/$B_DEVICE" -name '.*.un~' -exec rm -vf {} \;
    echo "[INFO] Removing __pycache__ directories..."
    find "$B_HOME/$B_MOUNT/$B_DEVICE" -type d -name '__pycache__' -exec rm -vrf {} \;
    echo "[INFO] Cleanup completed."
}

# Function to create a local backup of Git repositories
git_backup() {
    rm -f /root/local/git.tar.gz

    echo "[INFO] Saving Git repositories to local storage."
    if rsync -avz --no-o --no-g --delete "$1@$2:/home/repo" /root/local/; then
        echo "[INFO] Synchronization completed successfully."
    else
        echo "[WARN] Synchronization failed while retrieving repositories from the remote host." >&2
        return 1
    fi

    if cd /root/local; then
        echo "[INFO] Creating archive git.tar.gz from repo/."
        if tar czvf git.tar.gz repo/ > /dev/null; then
            echo "[INFO] Archive created: /root/local/git.tar.gz"
        else
            echo "[WARN] Failed to create archive." >&2
            return 1
        fi
    else
        echo "[WARN] Failed to change directory to /root/local." >&2
        return 1
    fi

    DEST_DIR="$B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git"
    if [ -d "$DEST_DIR" ]; then
        echo "[INFO] Copying archive to $DEST_DIR".
        cp -v /root/local/git.tar.gz "$DEST_DIR/"
    else
        echo "[WARN] Destination directory not found: $DEST_DIR" >&2
        return 1
    fi

    cd
}

# Function to back up GitHub repositories locally
github_backup() {
    rm -f /root/local/github.tar.gz

    if [ -d "$B_HOME/local/github" ]; then
        echo "[INFO] Creating archive: /root/local/github.tar.gz from $B_HOME/local/github"
        tar czvf /root/local/github.tar.gz "$B_HOME/local/github" > /dev/null
    else
        echo "[WARN] Directory not found: $B_HOME/local/github" >&2
    fi

    if [ -d "$B_HOME/local/github" ]; then
        echo "[INFO] Disk usage for $B_HOME/local/github:"
        du -h --max-depth=1 "$B_HOME/local/github"
    fi

    if [ -d "$B_HOME/local/git" ]; then
        echo "[INFO] Disk usage for $B_HOME/local/git:"
        du -h --max-depth=1 "$B_HOME/local/git"
    fi

    if [ -f /root/local/github.tar.gz ] && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git" ]; then
        echo "[INFO] Copying archive to $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git/."
        cp -v /root/local/github.tar.gz "$B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git/"
    else
        echo "[WARN] Skipping copy: archive or destination not available." >&2
    fi
}

# Function to sync user directories from disk to remote server via SSH
rsync_disk2ssh_1() {
    echo -n "[INFO] Executing: rsync_disk2ssh_1 $B_DEVICE -> $T_DEVICE of $T_HOST at "
    date "+%Y/%m/%d %T"
    if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user1" ]; then
        rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/user1" \
        "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing user1: either host unreachable or source directory missing." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"

    if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user2" ]; then
        rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/user2" \
        "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing user2: either host unreachable or source directory missing." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"

    if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user3" ]; then
        rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/user3" \
        "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing user3: either host unreachable or source directory missing." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"
}

# Function to sync large files from disk to remote server via SSH
rsync_disk2ssh_2() {
    echo -n "[INFO] Executing: rsync_disk2ssh_2 $B_DEVICE -> $T_DEVICE of $T_HOST at "
    date "+%Y/%m/%d %T"
    if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" ]; then
        rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" \
        "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing largefiles: either host unreachable or source directory missing." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"
}

# Function to sync user directories between two local disks
rsync_disk2disk_1() {
    echo -n "[INFO] Executing: rsync_disk2disk_1 $B_DEVICE -> $T_DEVICE at "
    date "+%Y/%m/%d %T"
    if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user1" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/user1" ]; then
        rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/user1" \
        "$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing user1: source or target directory does not exist." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"

    if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user2" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/user2" ]; then
        rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/user2" \
        "$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing user2: source or target directory does not exist." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"

    if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user3" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/user3" ]; then
        rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/user3" \
        "$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing user3: source or target directory does not exist." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"
}

# Function to sync large files between two local disks
rsync_disk2disk_2() {
    echo -n "[INFO] Executing: rsync_disk2disk_2 $B_DEVICE -> $T_DEVICE at "
    date "+%Y/%m/%d %T"
    if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/largefiles" ]; then
        rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" \
        "$T_HOME/$T_MOUNT/$T_DEVICE/"
        RC=$?
    else
        echo "[WARN] Skipped syncing largefiles: source or target directory does not exist." >&2
        RC=1
    fi
    echo "[INFO] Return code is $RC"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    if ! is_running_from_cron; then
        echo "[ERROR] This script is intended to be run by cron only." >&2
        exit 1
    fi

    SCRIPT_DIR="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
    CONFIG_FILE="$SCRIPT_DIR/../etc/rsync_backup.conf"

    if [ -f "$CONFIG_FILE" ]; then
        echo "[INFO] Loaded configuration from $CONFIG_FILE."
        . "$CONFIG_FILE"
    else
        echo "[ERROR] Configuration file not found: $CONFIG_FILE">&2
        exit 99
    fi
}

# Execute main function
main "$@"
