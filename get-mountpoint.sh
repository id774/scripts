#!/bin/sh

########################################################################
# get-mountpoint.sh: Resolve mountpoint from a block device
#
#  Description:
#  This utility takes a block device path (e.g. /dev/sdc, /dev/sdc1,
#  /dev/mapper/truecrypt1, /dev/dm-0) and determines the mountpoint
#  where it is currently mounted. It prints a single mountpoint path
#  to stdout, making it suitable for both interactive checks and
#  automation scripts.
#
#  Features:
#    - Accepts base disks, partitions, and device-mapper paths.
#    - If the exact device is not mounted, searches mounted children
#      (e.g. partitions or mapped volumes) under the given device.
#    - Uses findmnt(8) primarily; falls back to lsblk(8) scanning.
#    - Deterministic exit codes for robust scripting.
#
#  Notes:
#    - Designed for Linux systems with lsblk(8) and findmnt(8) available.
#    - Returns the first matching mountpoint when multiple exist
#      (e.g. bind mounts or multiple children). This keeps output
#      deterministic for scripting.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      get-mountpoint.sh <device>
#    Example:
#      get-mountpoint.sh /dev/sdc1
#      => /mnt/disk1
#
#  Error Conditions:
#  0. Success.
#  1. General failure.
#  2. Device not found or not mounted.
#  3. Path is not a block device.
#  126. Required command is not executable.
#  127. Required command is not installed.
#
#  Version History:
#  v1.0 2025-08-31
#       Initial release.
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

# Print error and exit with code
fail() {
    code="$1"; shift
    echo "[ERROR] $*" >&2
    exit "$code"
}

# Check if the system is Linux
check_system() {
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if required commands are available and executable
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Validate CLI arguments and device existence
validate_args() {
    if [ $# -ne 1 ]; then
        echo "[ERROR] Exactly one device argument is required." >&2
        exit 2
    fi

    DEV="$1"

    # Allow symlinks like /dev/disk/by-id/...; ensure it exists
    if [ ! -e "$DEV" ]; then
        echo "[ERROR] Device path does not exist: $DEV" >&2
        exit 2
    fi

    # Require a block device (regular file paths are not accepted)
    if [ ! -b "$DEV" ]; then
        echo "[ERROR] Not a block device: $DEV" >&2
        exit 3
    fi

    DEVICE="$DEV"
    return 0
}

# Try to obtain mountpoint directly from the device
find_mountpoint_direct() {
    DEVPATH="$1"
    MP=$(findmnt -nr -S -- "$DEVPATH" -o TARGET 2>/dev/null || true)
    if [ -n "$MP" ]; then
        printf '%s\n' "$MP"
        return 0
    fi
    return 1
}

# Scan children (partitions / mapped volumes) to find a mounted node
find_mountpoint_children() {
    DEVPATH="$1"
    # Use -P to make parsing safe with spaces in mountpoints
    lsblk -rpn -o NAME,MOUNTPOINT -P -- "$DEVPATH" 2>/dev/null \
    | sed -n 's/.*MOUNTPOINT="\([^"]*\)".*/\1/p' \
    | awk 'length($0) > 0 { print; exit }'
}

# Resolve and print mountpoint with error handling
resolve_and_print() {
    DEV="$1"

    # 1) Exact device mounted?
    MP=$(find_mountpoint_direct "$DEV") && { printf '%s\n' "$MP"; return 0; }

    # 2) If base device given, check mounted children
    MP=$(find_mountpoint_children "$DEV")
    if [ -n "$MP" ]; then
        printf '%s\n' "$MP"
        return 0
    fi

    fail 2 "Device not mounted or no mounted children found: $DEV"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands lsblk findmnt awk sed
    validate_args "$@"
    resolve_and_print "$DEVICE"
    return $?
}

# Execute main function
main "$@"
exit $?
