#!/bin/sh

########################################################################
# rsync_backup.sh: Backup and Syncing Removable Disk Script
#
#  Description:
#  This script facilitates backup and synchronization of data to removable
#  disks. It checks disk health, updates timestamps, removes unnecessary
#  metadata files, and synchronizes data between local disks or over SSH.
#  The script is configured via /etc/cron.config/rsync_backup.conf and is
#  intended to be run automatically from cron.
#
#  Before any device operation, the script resolves the base block device
#  by running `get-device <mountpoint>` and uses its result.
#
#  Backup Data Layout:
#  The device root contains top-level data areas.
#  - base:
#    Stores the standard backup area for the conventional storage layout.
#    This area is intended for data that should fit on smaller destination
#    disks, such as 2TB portable HDDs.
#  - extended:
#    Stores the expanded backup area for larger-capacity storage layouts.
#    This area is intended to be synchronized only to destination disks that
#    explicitly provide an extended data area.
#
#  Capacity Tier Policy:
#  - base and extended are capacity-tier data areas, not arbitrary directory
#    groups.
#  - base is the portable baseline data area. It is expected to fit on smaller
#    removable disks, including 2TB 2.5-inch HDDs.
#  - extended is the large-capacity data area. It is intended for larger disks,
#    such as multi-terabyte HDDs, where additional data can be preserved.
#  - A large source disk may contain both base and extended, while a smaller
#    destination disk may intentionally contain only base.
#  - In that case, only base is synchronized. extended is not created on the
#    destination side.
#
#  Synchronization Policy:
#  - The destination data area layout is authoritative.
#  - If base exists under both the source and destination device roots,
#    base is synchronized.
#  - If extended exists under both the source and destination device roots,
#    extended is synchronized.
#  - If a data area exists only on the source side, it is skipped without
#    warning and is not created on the destination side.
#  - If a data area does not exist on either side, it is skipped without warning.
#  - This preserves smaller destination layouts, such as synchronizing only
#    base from a larger disk to a 2TB portable disk.
#  - Synchronization is performed by top-level data area, not by each logical
#    directory under base.
#  - rsync updates the contents of existing destination data areas. It does not
#    define the destination capacity layout.
#
#  FAT Destination Policy:
#  - FAT-like destination filesystems are supported for file-content backup
#    when Unix metadata is not required.
#  - This is useful for TrueCrypt or compatible encrypted volumes formatted
#    with FAT-like filesystems and used as portable backup media.
#  - FAT-like filesystems do not preserve Unix ownership, group, and permission
#    metadata in the same way as Unix filesystems.
#  - They also do not preserve symbolic-link, device-file, FIFO, or socket
#    metadata as Unix filesystems do.
#  - This script is suitable for FAT-like destinations when the synchronized
#    data consists of ordinary files and directories and when file contents are
#    the preservation target.
#  - Large files that exceed the destination filesystem limit, such as the
#    4GiB-per-file limit of FAT32, must not be placed in the data area intended
#    for such destinations.
#
#  Permission Normalization Policy:
#  - After local disk-to-disk synchronization, destination data area permissions
#    are normalized only when the destination filesystem is known to preserve
#    Unix ownership and permission metadata.
#  - FAT-like destination filesystems, such as FAT, VFAT, MS-DOS FAT, and exFAT,
#    do not preserve Unix ownership and permission metadata.
#  - Permission normalization is skipped for FAT-like, unsupported, or unknown
#    destination filesystems to avoid treating successful file synchronization
#    as a failure.
#  - Permission normalization execution and skip decisions are logged explicitly.
#  - When permission normalization is enabled, chmodtree normalizes only entries
#    whose current ownership or permissions differ from the requested values:
#    root:root ownership, 0755 directories, and 0644 files.
#  - Symbolic links preserved by rsync -a are left unmodified by ownership
#    normalization, since chmodtree excludes symbolic links from --user/--group
#    normalization by default. This avoids following a preserved symlink that
#    points outside the synchronized data area.
#
#  Operational Examples:
#  - Large HDD to large HDD:
#    If both source and destination contain base and extended, both data areas
#    are synchronized.
#  - Large HDD to 2TB portable HDD:
#    If the source contains base and extended but the destination contains only
#    base, only base is synchronized.
#  - LUKS + ext4 to TrueCrypt + FAT:
#    If the destination contains only base, only base is synchronized. FAT-like
#    permission normalization is skipped, and the backup focuses on preserving
#    ordinary file contents.
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
#  Requirements:
#  - The system must have `get-device` command installed and available in PATH.
#  - rsync must be installed.
#  - chmodtree v3.2 or later is required only when permission normalization is
#    actually run. v3.2 or later is required specifically because it excludes
#    symbolic links from ownership normalization by default; earlier versions
#    dereferenced symlinks preserved by rsync -a and could alter ownership of
#    files outside the synchronized data area.
#  - findmnt or stat is used to determine the destination filesystem type for
#    local disk-to-disk permission normalization decisions.
#
#  Version History:
#  v4.2  2026-07-11 - Replace the awk {n,} interval expression in usage() with a
#                     portable equivalent, since mawk on some systems matches it
#                     incorrectly.
#  v4.1  2026-06-15 - Use chmodtree to normalize backup ownership and permissions
#                     only for entries whose attributes differ from the requested values.
#  v4.0  2026-05-17 - Adopt base and extended data area layout for backup synchronization.
#                     Separate Git archive handling from this backup synchronization script.
#                     Add timestamped progress logs for long-running backup steps.
#  v3.3  2025-08-31 - Resolve device via get-device before device operations.
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
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
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

# Display an informational message with the current timestamp
log_info_time() {
    printf '[INFO] %s at %s\n' "$*" "$(date '+%Y/%m/%d %T')"
}

# Display the serial number of a given device
print_serial_number() {
    # Resolve base device via external get-device
    if command -v get-device >/dev/null 2>&1; then
        DEVPATH=$(get-device "$T_HOME/$T_MOUNT/$T_DEVICE" 2>/dev/null)
    else
        DEVPATH="/dev/$T_DEVICE"
    fi

    if [ -n "$T_DEVICE" ] && [ -b "$DEVPATH" ]; then
        SERIAL=$(udevadm info --query=all --name="$DEVPATH" 2>/dev/null | sed -n 's/^E: ID_SERIAL_SHORT=//p')
        if [ -n "$SERIAL" ]; then
            echo "[INFO] Serial number of $DEVPATH: $SERIAL"
        else
            echo "[WARN] Serial number of $DEVPATH could not be determined." >&2
        fi
    else
        echo "[WARN] Invalid or non-block device: $DEVPATH" >&2
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
        # Resolve with get-device using the appropriate mountpoint
        MP=""
        [ "$DEV" = "$B_DEVICE" ] && MP="$B_HOME/$B_MOUNT/$B_DEVICE"
        [ "$DEV" = "$T_DEVICE" ] && MP="$T_HOME/$T_MOUNT/$T_DEVICE"

        if command -v get-device >/dev/null 2>&1 && [ -n "$MP" ]; then
            DEVPATH=$(get-device "$MP" 2>/dev/null)
        else
            DEVPATH="/dev/$DEV"
        fi

        if [ -b "$DEVPATH" ]; then
            echo "[INFO] Attempting smartctl -a -d sat $DEVPATH"
            if ! smartctl -a -d sat "$DEVPATH"; then
                echo "[INFO] -d sat failed for $DEVPATH, falling back to default"
                smartctl -a "$DEVPATH"
            fi
        else
            echo "[WARN] The device $DEVPATH does not exist or is not a block device." >&2
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

    # Prefer get-device resolution using mountpoint
    MP=""
    [ "$DEV" = "$T_DEVICE" ] && MP="$T_HOME/$T_MOUNT/$T_DEVICE"
    [ "$DEV" = "$B_DEVICE" ] && MP="$B_HOME/$B_MOUNT/$B_DEVICE"

    if command -v get-device >/dev/null 2>&1 && [ -n "$MP" ]; then
        DEVPATH=$(get-device "$MP" 2>/dev/null)
    else
        DEVPATH="/dev/$DEV"
    fi

    if [ ! -b "$DEVPATH" ]; then
        echo "[WARN] The device $DEVPATH does not exist or is not a block device." >&2
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
        echo "[INFO] Attempting smartctl -t long -d sat $DEVPATH"
        if ! smartctl -t long -d sat "$DEVPATH"; then
            echo "[INFO] -d sat failed, falling back to smartctl -t long"
            smartctl -t long "$DEVPATH"
        fi
    else
        : > "$SHORT_FLAG"
        echo "[INFO] Attempting smartctl -t short -d sat $DEVPATH"
        if ! smartctl -t short -d sat "$DEVPATH"; then
            echo "[INFO] -d sat failed, falling back to smartctl -t short"
            smartctl -t short "$DEVPATH"
        fi
    fi
}

# Show disk space usage of backup directories
show_capacity_of_directories() {
    DEVICE_ROOT="$B_HOME/$B_MOUNT/$B_DEVICE"

    if [ ! -d "$DEVICE_ROOT" ]; then
        echo "[WARN] Skipped disk usage check: device root not found: $DEVICE_ROOT" >&2
        return 1
    fi

    echo "[INFO] Disk usage of $DEVICE_ROOT."
    du -h --max-depth=3 "$DEVICE_ROOT"
}

# Remove unnecessary files such as macOS metadata and temp files
cleanup() {
    DEVICE_ROOT="$B_HOME/$B_MOUNT/$B_DEVICE"

    if [ ! -d "$DEVICE_ROOT" ]; then
        echo "[WARN] Skipped cleanup: device root not found: $DEVICE_ROOT" >&2
        return 1
    fi

    log_info_time "Starting cleanup in $DEVICE_ROOT"
    echo "[INFO] Removing junk files in $DEVICE_ROOT..."
    echo "[INFO] Removing ._* AppleDouble files..."
    find "$DEVICE_ROOT" -name '._*' -exec rm -vf {} \;

    echo "[INFO] Removing .DS_Store files..."
    find "$DEVICE_ROOT" -name '.DS_Store' -exec rm -vf {} \;

    echo "[INFO] Removing temporary Unix files ending with '.un~'..."
    find "$DEVICE_ROOT" -name '.*.un~' -exec rm -vf {} \;

    echo "[INFO] Removing __pycache__ directories..."
    find "$DEVICE_ROOT" -type d -name '__pycache__' -exec rm -vrf {} \;

    log_info_time "Finished cleanup in $DEVICE_ROOT"
    echo "[INFO] Cleanup completed."
}

# Show disk usage for synchronized local data areas
show_local_data_area_usage() {
    SRC_AREA=$1
    DEST_AREA=$2

    if [ -d "$SRC_AREA" ]; then
        echo "[INFO] Source apparent usage: $SRC_AREA"
        du --apparent-size --max-depth=1 "$SRC_AREA" | sort -k2
    fi

    if [ -d "$DEST_AREA" ]; then
        echo "[INFO] Destination apparent usage: $DEST_AREA"
        du --apparent-size --max-depth=1 "$DEST_AREA" | sort -k2
    fi
}

# Normalize preserved local data area permissions
normalize_local_data_area_permissions() {
    DATA_AREA_PATH=$1

    if [ ! -d "$DATA_AREA_PATH" ]; then
        echo "[WARN] Data area path not found: $DATA_AREA_PATH" >&2
        return 1
    fi

    if ! command -v chmodtree >/dev/null 2>&1; then
        echo "[WARN] chmodtree command not found." >&2
        return 1
    fi

    echo "[INFO] Applying ownership and permission normalization to $DATA_AREA_PATH"
    echo "[INFO] Running: chmodtree -q --user root --group root -d 0755 -f 0644 $DATA_AREA_PATH"
    chmodtree -q --user root --group root -d 0755 -f 0644 "$DATA_AREA_PATH" || return 1

    echo "[INFO] Permission normalization completed: $DATA_AREA_PATH"
    return 0
}

# Get the filesystem type for a path
get_filesystem_type() {
    TARGET_PATH=$1
    FS_TYPE=""

    if [ ! -e "$TARGET_PATH" ]; then
        echo "[WARN] Filesystem check target not found: $TARGET_PATH" >&2
        return 1
    fi

    if command -v findmnt >/dev/null 2>&1; then
        FS_TYPE=$(findmnt -n -o FSTYPE -T "$TARGET_PATH" 2>/dev/null)
    fi

    if [ -z "$FS_TYPE" ] && command -v stat >/dev/null 2>&1; then
        FS_TYPE=$(stat -f -c %T "$TARGET_PATH" 2>/dev/null)
    fi

    if [ -z "$FS_TYPE" ]; then
        echo "[WARN] Filesystem type could not be determined: $TARGET_PATH" >&2
        return 1
    fi

    echo "$FS_TYPE"
    return 0
}

# Check whether permission normalization should run on a destination path
should_normalize_local_permissions() {
    TARGET_PATH=$1

    FS_TYPE=$(get_filesystem_type "$TARGET_PATH")
    RC=$?

    if [ "$RC" -ne 0 ]; then
        echo "[WARN] Skipping permission normalization because filesystem type is unknown: $TARGET_PATH" >&2
        return 1
    fi

    case "$FS_TYPE" in
        vfat|msdos|fat|exfat)
            echo "[INFO] Skipping permission normalization for FAT-like filesystem ($FS_TYPE): $TARGET_PATH"
            return 1
            ;;
        ext2|ext3|ext4|xfs|btrfs|f2fs|zfs|jfs|reiserfs|reiser4)
            echo "[INFO] Permission normalization is enabled for filesystem ($FS_TYPE): $TARGET_PATH"
            return 0
            ;;
        *)
            echo "[WARN] Skipping permission normalization for unsupported filesystem ($FS_TYPE): $TARGET_PATH" >&2
            return 1
            ;;
    esac
}

# Sync backup data areas from disk to remote server via SSH
rsync_disk2ssh() {
    log_info_time "Executing: rsync_disk2ssh $B_DEVICE -> $T_DEVICE of $T_HOST"

    SRC_ROOT="$B_HOME/$B_MOUNT/$B_DEVICE"
    DEST_ROOT="$T_HOME/$T_MOUNT/$T_DEVICE"
    SSH_TARGET="$T_USER@$T_HOST"
    OVERALL_RC=0

    if [ ! -d "$SRC_ROOT" ]; then
        echo "[WARN] Source device root not found: $SRC_ROOT" >&2
        return 1
    fi

    if ! ping -c 1 "$T_HOST" > /dev/null 2>&1; then
        echo "[WARN] Skipped SSH synchronization: host unreachable: $T_HOST" >&2
        return 1
    fi

    for DATA_AREA in base extended; do
        if [ -d "$SRC_ROOT/$DATA_AREA" ] && ssh "$SSH_TARGET" "test -d '$DEST_ROOT/$DATA_AREA'"; then
            echo "[INFO] Syncing $DATA_AREA via SSH"
            log_info_time "Starting SSH sync for $DATA_AREA"
            rsync -avz --no-o --no-g --no-p --delete -e ssh "$SRC_ROOT/$DATA_AREA/" "$SSH_TARGET:$DEST_ROOT/$DATA_AREA/"
            RC=$?
            log_info_time "Finished SSH sync for $DATA_AREA"
            echo "[INFO] Return code is $RC"

            if [ "$RC" -ne 0 ]; then
                # Keep the rsync failure status for the failed data area.
                OVERALL_RC=$RC
            fi
        fi
    done

    return "$OVERALL_RC"
}

# Sync backup data areas between two local disks
rsync_disk2disk() {
    log_info_time "Executing: rsync_disk2disk $B_DEVICE -> $T_DEVICE"

    SRC_ROOT="$B_HOME/$B_MOUNT/$B_DEVICE"
    DEST_ROOT="$T_HOME/$T_MOUNT/$T_DEVICE"
    OVERALL_RC=0

    if [ ! -d "$SRC_ROOT" ]; then
        echo "[WARN] Source device root not found: $SRC_ROOT" >&2
        return 1
    fi

    if [ ! -d "$DEST_ROOT" ]; then
        echo "[WARN] Target device root not found: $DEST_ROOT" >&2
        return 1
    fi

    for DATA_AREA in base extended; do
        if [ -d "$SRC_ROOT/$DATA_AREA" ] && [ -d "$DEST_ROOT/$DATA_AREA" ]; then
            echo "[INFO] Syncing $DATA_AREA locally"
            log_info_time "Starting local sync for $DATA_AREA"
            rsync -avz --no-o --no-g --no-p --delete "$SRC_ROOT/$DATA_AREA/" "$DEST_ROOT/$DATA_AREA/"
            RC=$?
            log_info_time "Finished local sync for $DATA_AREA"
            echo "[INFO] Return code is $RC"

            if [ "$RC" -ne 0 ]; then
                # Keep the rsync failure status and skip preservation steps
                # for the failed data area.
                OVERALL_RC=$RC
            elif should_normalize_local_permissions "$DEST_ROOT/$DATA_AREA"; then
                # Normalize the destination data area for preservation
                # before showing source and destination apparent sizes.
                log_info_time "Starting permission normalization for $DATA_AREA"
                if normalize_local_data_area_permissions "$DEST_ROOT/$DATA_AREA"; then
                    log_info_time "Finished permission normalization for $DATA_AREA"
                    echo "[INFO] Disk usage after syncing $DATA_AREA locally"
                    log_info_time "Starting disk usage check for $DATA_AREA"
                    show_local_data_area_usage "$SRC_ROOT/$DATA_AREA" "$DEST_ROOT/$DATA_AREA"
                    log_info_time "Finished disk usage check for $DATA_AREA"
                else
                    # Treat preservation failure as an overall backup failure
                    # even when rsync itself completed successfully.
                    echo "[WARN] Failed to normalize permissions for $DEST_ROOT/$DATA_AREA" >&2
                    OVERALL_RC=1
                fi
            else
                echo "[INFO] Disk usage after syncing $DATA_AREA locally"
                log_info_time "Starting disk usage check for $DATA_AREA"
                show_local_data_area_usage "$SRC_ROOT/$DATA_AREA" "$DEST_ROOT/$DATA_AREA"
                log_info_time "Finished disk usage check for $DATA_AREA"
            fi
        fi
    done

    return "$OVERALL_RC"
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
