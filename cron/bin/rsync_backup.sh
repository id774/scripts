#!/bin/sh

########################################################################
# rsync_backup.sh: Backup and Syncing Removable Disk Script
#
#  Description:
#  This script facilitates the backup and syncing of data to removable
#  disks. It includes functionalities like checking disk health, updating
#  timestamps, performing cleanup tasks, and syncing data between disks and
#  over SSH. The script is configured via /etc/cron.config/rsync_backup.conf
#  and intended to be run automatically from cron.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script without any arguments. Ensure all the necessary paths
#  and variables are correctly set within the script or the config file:
#      test -x /etc/cron.exec/rsync_backup.sh && /etc/cron.exec/rsync_backup.sh >> $JOBLOG 2>&1
#
#  The script will automatically execute operations based on the configured
#  settings and available devices, as defined in /etc/cron.config/rsync_backup.conf.
#
#  Version History:
#  v3.2  2025-08-29 - Enable device argument in smart_info and smart_check.
#  v3.1  2025-07-30 - Update script and config paths to /etc/cron.exec and /etc/cron.config respectively.
#  v3.0  2025-06-23 - Unified usage output to display full script header and support common help/version options.
#  v2.9  2025-06-15 - Externalize archive and repository path variables to rsync_backup.conf.
#                     Remove hardcoded paths from git_backup and github_backup functions.
#                     Refactor all rsync functions to use internal SRC_DIR and DEST_DIR variables.
#  v2.8  2025-06-06 - Add fallback logic to smartctl calls using -d sat for better USB device compatibility.
#                     Preserve default behavior if -d sat fails.
#                     Add print_serial_number function to display device serials using udevadm before processing.
#  v2.7  2025-05-16 - Add return 0 to main and exit $? at script end for consistent exit status.
#  v2.6  2025-05-10 - Add cron execution check and usage support with unified structure.
#  v2.5  2025-04-20 - Fix inaccurate rsync return code logging by assigning RC immediately after execution.
#  v2.4  2025-04-17 - Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.3  2025-03-19 - Improved code readability by standardizing function indentation.
#                     Encapsulated script logic in a `main` function for better structure.
#                     Ensured proper preservation of return codes by storing `$?` in a variable before use.
#  v2.2  2023-12-23 - Refactored for POSIX compliance. Replaced Bash-specific syntax
#                     with POSIX standard commands and structures. Enhanced portability
#                     and compatibility across different UNIX-like systems.
#  v2.1  2023-12-17 - Refactored script to separate logic and operations.
#                     Operations are now defined in an external file
#                     'etc/rsync_backup.conf' for enhanced modularity and maintainability.
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
#  v1.0  2008-02-28 - Stable initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the script is running from cron
is_running_from_cron() {
    if tty -s; then
        return 1  # Terminal attached (interactive session)
    else
        return 0  # No terminal (likely cron)
    fi
}

# Display the serial number of a given device
print_serial_number() {
    if [ -n "$T_DEVICE" ] && [ -b "/dev/$T_DEVICE" ]; then
        SERIAL=$(udevadm info --query=all --name="/dev/$T_DEVICE" 2>/dev/null | sed -n 's/^E: ID_SERIAL_SHORT=//p')
        if [ -n "$SERIAL" ]; then
            echo "[INFO] Serial number of /dev/$T_DEVICE: $SERIAL"
        else
            echo "[WARN] Serial number of /dev/$T_DEVICE could not be determined." >&2
        fi
    else
        echo "[WARN] Invalid or non-block device: /dev/$T_DEVICE" >&2
    fi
}

# Display the timestamp of the last backup and update it
display_and_update_timestamp() {
    if [ -f "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp" ]; then
        echo "[INFO] ls -l $T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
        ls -l "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
    fi
    echo "[INFO] touch $T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
    touch "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
}

# Display the installed versions of TrueCrypt and VeraCrypt
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

# Retrieve SMART information of the backup and target devices
smart_info() {
    # Normalize optional first argument to a bare device name like "sde"
    # Accepts "sde" or "/dev/sde"
    if [ -n "$1" ]; then
        case "$1" in
            /dev/*) DEV_ARG=${1#/dev/} ;;
            *)      DEV_ARG=$1 ;;
        esac
        DEV_LIST=$DEV_ARG
    else
        DEV_LIST="$B_DEVICE $T_DEVICE"
    fi

    for DEV in $DEV_LIST; do
        if [ -b "/dev/$DEV" ]; then
            echo "[INFO] Attempting smartctl -a -d sat /dev/$DEV"
            if ! smartctl -a -d sat "/dev/$DEV"; then
                echo "[INFO] -d sat failed for /dev/$DEV, falling back to default"
                smartctl -a "/dev/$DEV"
            fi
        else
            echo "[WARN] The device /dev/$DEV does not exist or is not a block device." >&2
        fi
    done
}

# Perform a SMART diagnostic check on a device
smart_check() {
    # Resolve target device
    if [ -n "$1" ]; then
        case "$1" in
            /dev/*) DEV=${1#/dev/} ;;
            *)      DEV=$1 ;;
        esac
    else
        DEV=$T_DEVICE
    fi

    if [ ! -b "/dev/$DEV" ]; then
        echo "[WARN] The device /dev/$DEV does not exist or is not a block device." >&2
        return 1
    fi

    # Decide base path for flag files: prefer target path, then backup path, else fallback to target path
    BASE_HOME=$T_HOME
    BASE_MOUNT=$T_MOUNT
    if [ -d "$T_HOME/$T_MOUNT/$DEV" ]; then
        BASE_HOME=$T_HOME
        BASE_MOUNT=$T_MOUNT
    elif [ -d "$B_HOME/$B_MOUNT/$DEV" ]; then
        BASE_HOME=$B_HOME
        BASE_MOUNT=$B_MOUNT
    fi

    # Choose long or short test by presence of longtest flag file
    LONG_FLAG="$BASE_HOME/$BASE_MOUNT/$DEV/smart_longtest"
    SHORT_FLAG="$BASE_HOME/$BASE_MOUNT/$DEV/smart_shorttest"

    if [ -f "$LONG_FLAG" ]; then
        : > "$LONG_FLAG"
        echo "[INFO] Attempting smartctl -t long -d sat /dev/$DEV"
        if ! smartctl -t long -d sat "/dev/$DEV"; then
            echo "[INFO] -d sat failed, falling back to smartctl -t long"
            smartctl -t long "/dev/$DEV"
        fi
    else
        : > "$SHORT_FLAG"
        echo "[INFO] Attempting smartctl -t short -d sat /dev/$DEV"
        if ! smartctl -t short -d sat "/dev/$DEV"; then
            echo "[INFO] -d sat failed, falling back to smartctl -t short"
            smartctl -t short "/dev/$DEV"
        fi
    fi
}

# Show disk space usage of backup directories
show_capacity_of_directories() {
    if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" ]; then
        echo "[INFO] Disk usage of $B_HOME/$B_MOUNT/$B_DEVICE."
        du -h --max-depth=2 "$B_HOME/$B_MOUNT/$B_DEVICE"
    fi
}

# Remove unnecessary files such as macOS metadata and temp files
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

# Create a local backup of Git repositories
git_backup() {
    rm -f "$ARCHIVE_DIR/$ARCHIVE_NAME_GIT"

    echo "[INFO] Saving Git repositories to local storage."
    if rsync -avz --no-o --no-g --delete "$1@$2:$REMOTE_GIT_DIR" "$ARCHIVE_DIR/"; then
        echo "[INFO] Synchronization completed successfully."
    else
        echo "[WARN] Synchronization failed while retrieving repositories from the remote host." >&2
        return 1
    fi

    if cd "$ARCHIVE_DIR"; then
        echo "[INFO] Creating archive $ARCHIVE_NAME_GIT from git/."
        if tar czvf "$ARCHIVE_NAME_GIT" git/ > /dev/null; then
            echo "[INFO] Archive created: $ARCHIVE_DIR/$ARCHIVE_NAME_GIT"
        else
            echo "[WARN] Failed to create archive." >&2
            return 1
        fi
    else
        echo "[WARN] Failed to change directory to $ARCHIVE_DIR." >&2
        return 1
    fi

    if [ -d "$DEST_GIT_ARCHIVE" ]; then
        echo "[INFO] Copying archive to $DEST_GIT_ARCHIVE"
        cp -v "$ARCHIVE_DIR/$ARCHIVE_NAME_GIT" "$DEST_GIT_ARCHIVE/"
    else
        echo "[WARN] Destination directory not found: $DEST_GIT_ARCHIVE" >&2
        return 1
    fi

    cd "$HOME"
}

# Back up GitHub repositories locally
github_backup() {
    rm -f "$ARCHIVE_DIR/$ARCHIVE_NAME_GITHUB"

    if [ -d "$GITHUB_SRC" ]; then
        echo "[INFO] Creating archive: $ARCHIVE_DIR/$ARCHIVE_NAME_GITHUB from $GITHUB_SRC"
        tar czvf "$ARCHIVE_DIR/$ARCHIVE_NAME_GITHUB" "$GITHUB_SRC" > /dev/null
    else
        echo "[WARN] Directory not found: $GITHUB_SRC" >&2
    fi

    if [ -f "$ARCHIVE_DIR/$ARCHIVE_NAME_GITHUB" ] && [ -d "$DEST_GIT_ARCHIVE" ]; then
        echo "[INFO] Copying archive to $DEST_GIT_ARCHIVE"
        cp -v "$ARCHIVE_DIR/$ARCHIVE_NAME_GITHUB" "$DEST_GIT_ARCHIVE/"
    else
        echo "[WARN] Skipping copy: archive or destination not available." >&2
    fi
}

# Sync user directories from disk to remote server via SSH
rsync_disk2ssh_1() {
    echo -n "[INFO] Executing: rsync_disk2ssh_1 $B_DEVICE -> $T_DEVICE of $T_HOST at "
    date "+%Y/%m/%d %T"

    SRC_DIR="$B_HOME/$B_MOUNT/$B_DEVICE"
    DEST_DIR="$T_HOME/$T_MOUNT/$T_DEVICE"
    SSH_TARGET="$T_USER@$T_HOST"

    for USER_DIR in user1 user2 user3; do
        if ping -c 1 "$T_HOST" > /dev/null 2>&1 && [ -d "$SRC_DIR/$USER_DIR" ]; then
            echo "[INFO] Syncing $USER_DIR via SSH"
            rsync -avz --no-o --no-g --delete -e ssh "$SRC_DIR/$USER_DIR" "$SSH_TARGET:$DEST_DIR/"
            RC=$?
        else
            echo "[WARN] Skipped syncing $USER_DIR: either host unreachable or source directory missing." >&2
            RC=1
        fi
        echo "[INFO] Return code is $RC"
    done
}

# Sync large files from disk to remote server via SSH
rsync_disk2ssh_2() {
    echo -n "[INFO] Executing: rsync_disk2ssh_2 $B_DEVICE -> $T_DEVICE of $T_HOST at "
    date "+%Y/%m/%d %T"

    SRC_DIR="$B_HOME/$B_MOUNT/$B_DEVICE"
    DEST_DIR="$T_HOME/$T_MOUNT/$T_DEVICE"
    SSH_TARGET="$T_USER@$T_HOST"

    if ping -c 1 "$T_HOST" > /dev/null 2>&1 && [ -d "$SRC_DIR/largefiles" ]; then
        echo "[INFO] Syncing largefiles via SSH"
        rsync -avz --no-o --no-g --delete -e ssh "$SRC_DIR/largefiles" "$SSH_TARGET:$DEST_DIR/"
        RC=$?
    else
        echo "[WARN] Skipped syncing largefiles: either host unreachable or source directory missing." >&2
        RC=1
    fi

    echo "[INFO] Return code is $RC"
}

# Sync user directories between two local disks
rsync_disk2disk_1() {
    echo -n "[INFO] Executing: rsync_disk2disk_1 $B_DEVICE -> $T_DEVICE at "
    date "+%Y/%m/%d %T"

    SRC_DIR="$B_HOME/$B_MOUNT/$B_DEVICE"
    DEST_DIR="$T_HOME/$T_MOUNT/$T_DEVICE"

    for USER_DIR in user1 user2 user3; do
        if [ -d "$SRC_DIR/$USER_DIR" ] && [ -d "$DEST_DIR/$USER_DIR" ]; then
            echo "[INFO] Syncing $USER_DIR locally"
            rsync -avz --no-o --no-g --delete "$SRC_DIR/$USER_DIR" "$DEST_DIR/"
            RC=$?
        else
            echo "[WARN] Skipped syncing $USER_DIR: source or target directory does not exist." >&2
            RC=1
        fi
        echo "[INFO] Return code is $RC"
    done
}

# Sync large files between two local disks
rsync_disk2disk_2() {
    echo -n "[INFO] Executing: rsync_disk2disk_2 $B_DEVICE -> $T_DEVICE at "
    date "+%Y/%m/%d %T"

    SRC_DIR="$B_HOME/$B_MOUNT/$B_DEVICE"
    DEST_DIR="$T_HOME/$T_MOUNT/$T_DEVICE"

    if [ -d "$SRC_DIR/largefiles" ] && [ -d "$DEST_DIR/largefiles" ]; then
        echo "[INFO] Syncing largefiles locally"
        rsync -avz --no-o --no-g --delete "$SRC_DIR/largefiles" "$DEST_DIR/"
        RC=$?
    else
        echo "[WARN] Skipped syncing largefiles: source or target directory does not exist." >&2
        RC=1
    fi

    echo "[INFO] Return code is $RC"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if ! is_running_from_cron; then
        echo "[ERROR] This script is intended to be run by cron only." >&2
        exit 1
    fi

    SCRIPT_DIR="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
    CONFIG_FILE="/etc/cron.config/rsync_backup.conf"

    if [ -f "$CONFIG_FILE" ]; then
        echo "[INFO] Loaded configuration from $CONFIG_FILE."
        . "$CONFIG_FILE"
    else
        echo "[ERROR] Configuration file not found: $CONFIG_FILE" >&2
        exit 9
    fi

    return 0
}

# Execute main function
main "$@"
exit $?
