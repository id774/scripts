#!/bin/sh

########################################################################
# get-serial.sh: Print disk serial number from a given device path
#
#  Description:
#  Accepts a device path (e.g. /dev/sdc, /dev/sdc1, /dev/mapper/foo, /dev/dm-0)
#  and prints its serial number to stdout. This tool does NOT resolve
#  mountpoints; compose with get-device if needed.
#
#  Responsibilities (UNIX philosophy):
#    - Input: a /dev/* block device path only.
#    - Output: the serial number only (no extra decoration) on success.
#    - Errors: messages to stderr and deterministic exit codes.
#
#  Requirements:
#    - Linux, udevadm(8), lsblk(8), sed(1), head(1), awk(1)
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      get-serial.sh /dev/sdc
#      get-serial.sh /dev/mapper/disk1_crypt
#    Compose with get-device.sh:
#      get-serial.sh "$(get-device.sh ~/mnt/disk1)"
#
#  Exit Codes:
#  0. Success.
#  1. General failure.
#  2. Invalid argument or device not found.
#  3. Path is not a block device.
#  4. Serial number could not be determined.
#  126. Required command is not executable.
#  127. Required command is not installed.
#
#  Version History:
#  v1.2 2025-12-13
#       Unify exit behavior in print_serial and correct command name in warning message.
#  v1.1 2025-11-09
#       Remove shared fail function and inline all error handling to comply with implementation policy.
#       Unify argument error message.
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

# Validate argument: must be exactly one /dev/* path
validate_args() {
    if [ "$#" -ne 1 ]; then
        echo "[ERROR] Exactly one device argument is required." >&2
        exit 2
    fi

    DEV="$1"

    case "$DEV" in
        /dev/*)
            ;;
        *)
            echo "[ERROR] Not a device path: $DEV" >&2
            echo "[WARN] If you have a mountpoint, resolve it first: get-serial.sh \"\$(get-device.sh <mountpoint>)\"" >&2
            exit 2
            ;;
    esac

    if [ ! -e "$DEV" ]; then
        echo "[ERROR] Device not found: $DEV" >&2
        exit 2
    fi

    if [ ! -b "$DEV" ]; then
        echo "[ERROR] Path is not a block device: $DEV" >&2
        exit 3
    fi
}

# Query serial using udevadm first, then lsblk as fallback
print_serial() {
    dev="$1"

    serial=$(
        udevadm info --query=property --name="$dev" 2>/dev/null \
        | sed -n 's/^ID_SERIAL_SHORT=//p'
    )
    if [ -z "$serial" ]; then
        serial=$(
            udevadm info --query=property --name="$dev" 2>/dev/null \
            | sed -n 's/^ID_SERIAL=//p'
        )
    fi
    if [ -z "$serial" ]; then
        serial=$(lsblk -ndo SERIAL -- "$dev" 2>/dev/null | head -n 1)
    fi

    if [ -n "$serial" ]; then
        printf '%s\n' "$serial"
        return 0
    fi

    echo "[WARN] Serial number could not be determined for: $dev" >&2
    exit 4
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac
    check_system
    check_commands udevadm lsblk sed head
    validate_args "$@"
    print_serial "$DEV" || exit $?
    return $?
}

# Execute main function
main "$@"
