#!/bin/sh

########################################################################
# setup_motd.sh: Clear /etc/motd content safely and idempotently
#
#  Description:
#  This script truncates /etc/motd to an empty file if it exists.
#  No other files are modified. The operation is idempotent and uses only
#  POSIX-compliant utilities without creating backups. If /etc/motd does
#  not exist, the script exits successfully without changes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_motd.sh
#  Run as a regular user with sudo privileges; the script will invoke sudo
#  where necessary.
#
#  Notes:
#  - No backups are created; the script edits /etc/motd directly if present.
#  - The script is no-op when /etc/motd does not exist.
#  - Designed for Debian-family Linux systems, but works on general Linux.
#
#  Requirements:
#  - Linux operating system
#  - sudo privileges for modifying /etc/motd
#  - Commands: sudo, awk, sh, test
#
#  Version History:
#  v1.0 2025-08-14
#       Initial release.
#
########################################################################

MOTD_FILE="/etc/motd"

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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Clear /etc/motd with file type validation
clear_motd() {
    if [ -f "$MOTD_FILE" ]; then
        if ! sudo sh -c ": > \"$MOTD_FILE\""; then
            echo "[ERROR] Failed to clear $MOTD_FILE." >&2
            exit 1
        fi
        echo "[INFO] Setup completed."
    elif [ -d "$MOTD_FILE" ]; then
        echo "[ERROR] $MOTD_FILE is a directory, no changes were made." >&2
        exit 1
    else
        echo "[ERROR] $MOTD_FILE does not exist as a file or directory." >&2
        exit 1
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo awk sh
    check_sudo
    clear_motd
    return 0
}

# Execute main function
main "$@"
exit $?
