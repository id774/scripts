#!/bin/sh

########################################################################
# setup_memcached_conf.sh: Comment out IPv6 localhost binding in memcached.conf
#
#  Description:
#  This script checks for the presence of /etc/memcached.conf and, if found,
#  looks for a line starting with "-l ::1". If such a line exists, it is
#  replaced with "# -l ::1" to comment it out. This prevents memcached from
#  binding to IPv6 localhost. The change preserves file permissions and affects
#  only the targeted line. If no matching line is found or it is already
#  commented, no edits are made. The operation is idempotent and uses only
#  POSIX-compliant utilities without creating backups.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_memcached_conf.sh
#  Run as a regular user with sudo privileges; the script will invoke sudo
#  where necessary.
#
#  Notes:
#  - No backups are created; edits are applied directly when needed.
#  - Only lines beginning exactly with "-l ::1" (ignoring leading spaces) are
#    commented.
#  - Designed for Debian-family Linux systems with memcached installed.
#
#  Requirements:
#  - Linux operating system
#  - sudo privileges for modifying /etc/memcached.conf
#  - Commands: sudo, awk, grep, mv, sh
#
#  Version History:
#  v1.0 2025-08-14
#       Initial release.
#
########################################################################

CONF_FILE="/etc/memcached.conf"

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

# Ensure /etc/memcached.conf exists
check_conf_file() {
    if ! sudo test -f "$CONF_FILE"; then
        echo "[INFO] $CONF_FILE not found." >&2
        exit 0
    fi
}

# Detect if a non-commented "-l ::1" line exists
has_ipv6_bind() {
    sudo grep -Eq '^[[:space:]]*-l[[:space:]]+::1[[:space:]]*$' "$CONF_FILE"
}

# Comment out the "-l ::1" line
comment_ipv6_bind() {
    if has_ipv6_bind; then
        tmp="/tmp/setup_memcached_conf.$$"
        sudo awk '
            /^[[:space:]]*-l[[:space:]]+::1[[:space:]]*$/ {
                m = match($0, /^[[:space:]]*/)
                indent = substr($0, 1, RLENGTH)
                print indent "# -l ::1"
                next
            }
            { print }
        ' "$CONF_FILE" > "$tmp" && sudo mv "$tmp" "$CONF_FILE"
        echo "[INFO] Commented out \"-l ::1\" in $CONF_FILE"
    else
        echo "[INFO] No active \"-l ::1\" line found. No changes made."
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands grep mv awk
    check_sudo
    check_conf_file

    comment_ipv6_bind
    return 0
}

# Execute main function
main "$@"
exit $?
