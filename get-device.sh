#!/bin/sh

########################################################################
# get-device.sh: Resolve base block device from a mountpoint
#
#  Description:
#  This utility takes a mountpoint path as input and determines the
#  underlying base block device (e.g. /dev/sdc). The output is a single
#  device path string, making it suitable for both standalone use in
#  the shell and for inclusion in larger automation scripts.
#  It eliminates the need to manually inspect /proc/mounts or lsblk
#  output when identifying which physical device a filesystem resides on.
#
#  Features:
#    - Works directly from a given mountpoint, no extra arguments required.
#    - Prints the canonical base device path (/dev/*) to stdout.
#    - Supports regular partitions (e.g. /dev/sdc1).
#    - Supports device-mapper paths (e.g. /dev/mapper/truecrypt1, /dev/dm-0).
#    - Walks the dependency chain using lsblk to return the actual root device.
#    - Provides deterministic exit codes to simplify error handling in scripts.
#
#  Notes:
#    - Designed for Linux systems with lsblk(8) and findmnt(8) available.
#    - Can be invoked interactively for quick checks or from scripts
#      that require reliable device resolution.
#    - Standardized error codes (see below) allow scripts to branch
#      logic depending on the type of failure.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      get-device.sh <mountpoint>
#    Example:
#      get-device.sh ~/mnt/disk1
#      => /dev/sdc
#
#  Error Conditions:
#  0. Success.
#  1. General failure.
#  2. Mountpoint or source not found.
#  3. Source is not a block device.
#  126. Required command is not executable.
#  127. Required command is not installed.
#
#  Version History:
#  v1.0 2025-08-30
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

# Resolve source device from a mountpoint
resolve_source() {
    MP="$1"
    if command -v findmnt >/dev/null 2>&1; then
        SRC=$(findmnt -no SOURCE -- "$MP" 2>/dev/null || true)
    else
        SRC=$(awk -v mp="$MP" '$2==mp{print $1}' /proc/mounts 2>/dev/null | tail -n1)
    fi
    [ -n "$SRC" ] || return 1
    printf '%s\n' "$SRC"
    return 0
}

# Convert a device path to its base disk device
to_base_disk() {
    DEVPATH="$1"
    [ -e "$DEVPATH" ] || return 1

    if command -v lsblk >/dev/null 2>&1; then
        case "$DEVPATH" in
            /dev/mapper/*|/dev/dm-*)
                # Walk full dependency chain to the root device (raw, no tree glyphs)
                BASE=$(lsblk -rnp -o NAME -s -- "$DEVPATH" 2>/dev/null | tail -n1)
                [ -n "$BASE" ] || return 1
                printf '%s\n' "$BASE"
                return 0
                ;;
            *)
                # Prefer PKNAME for regular partitions
                PK=$(lsblk -no PKNAME -- "$DEVPATH" 2>/dev/null | head -n1)
                if [ -n "$PK" ]; then
                    printf '/dev/%s\n' "$PK"
                    return 0
                fi
                ;;
        esac
    else
        echo "[ERROR] lsblk not found" >&2
        return 4
    fi

    # Fallback: strip partition suffix
    case "$DEVPATH" in
        /dev/nvme*|/dev/mmcblk*)
            printf '%s\n' "$(echo "$DEVPATH" | sed 's/p[0-9]\+$//')"
            ;;
        *)
            printf '%s\n' "$(echo "$DEVPATH" | sed 's/[0-9]\+$//')"
            ;;
    esac
    return 0
}

# Resolve and print base device with error handling
resolve_and_print() {
    MP="$1"

    SRC=$(resolve_source "$MP") || fail 2 "Mountpoint not found: $MP"

    case "$SRC" in
        /dev/*) : ;;
        *) fail 3 "Not a block device source: $SRC" ;;
    esac

    [ -e "$SRC" ] || fail 2 "Source not found: $SRC"

    BASE=$(to_base_disk "$SRC") || fail 2 "Failed to resolve base device for $SRC"
    printf '%s\n' "$BASE"
}

# Print error and exit with code
fail() {
    # Usage: fail <exit_code> <message...>
    code="$1"; shift
    echo "[ERROR] $*" >&2
    exit "$code"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands awk lsblk findmnt tail sed head
    resolve_and_print "$@"
    return 0
}

# Execute main function
main "$@"
exit $?
