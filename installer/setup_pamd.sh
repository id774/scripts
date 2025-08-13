#!/bin/sh

########################################################################
# setup_pamd.sh: Comment out pam_wheel.so in /etc/pam.d/su and clear /etc/motd
#
#  Description:
#  This script enforces a safer su policy by checking /etc/pam.d/su for any
#  active (uncommented) pam_wheel.so line and commenting it out when found,
#  while preserving existing indentation. If pam_wheel.so is already commented
#  or absent, the file is left untouched. Regardless of changes to /etc/pam.d/su,
#  the script always truncates /etc/motd to empty. It is idempotent, meaning
#  repeated runs will not cause redundant edits, and is implemented using only
#  POSIX-compliant utilities without creating backups.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_pamd.sh
#  Run as a regular user with sudo privileges; the script will invoke sudo
#  where necessary.
#
#  Notes:
#  - No backups are created; edits are applied directly when needed.
#  - Leading spaces in target lines are preserved; a "# " prefix is added only
#    when the line is active.
#  - /etc/motd is always emptied regardless of whether /etc/pam.d/su was modified.
#  - Designed for Debian-family Linux systems where pam_wheel.so may appear
#    in /etc/pam.d/su.
#
#  Requirements:
#  - Linux operating system (Debian/Ubuntu family recommended)
#  - sudo privileges for modifying /etc/pam.d/su and /etc/motd
#  - Commands: sudo, awk, grep, mv, sh
#
#  Version History:
#  v1.0 2025-08-13
#       Initial release.
#
########################################################################

PAM_FILE="/etc/pam.d/su"

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

# Check if /etc/pam.d/su exists
check_pam_file() {
    if ! sudo test -f "$PAM_FILE"; then
        echo "[ERROR] $PAM_FILE not found." >&2
        exit 1
    fi
}

# Decide if /etc/pam.d/su contains an active pam_wheel.so line
needs_edit() {
    sudo grep -Eq '^[[:space:]]*[^#].*pam_wheel\.so' "$PAM_FILE"
}

# Comment active pam_wheel.so lines while preserving indentation
comment_pam_wheel() {
    tmp="${PAM_FILE}.tmp.$$"
    sudo awk '
/pam_wheel\.so/ {
  if ($0 ~ /^[[:space:]]*#/) { print; next }
  sub(/^[[:space:]]*/, "&# ")
  print; next
}
{ print }
' "$PAM_FILE" > "$tmp" && sudo mv "$tmp" "$PAM_FILE"
    echo "[INFO] Commented pam_wheel.so"
}

# Truncate /etc/motd to an empty file if it exists
clear_motd() {
    if sudo test -f /etc/motd; then
        sudo sh -c ': > /etc/motd'
        echo "[INFO] Cleared /etc/motd"
    else
        echo "[INFO] /etc/motd not found. Skipping clear."
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands grep mv awk
    check_pam_file
    check_sudo

    if needs_edit; then
        comment_pam_wheel
    else
        echo "[INFO] pam_wheel.so already commented or not present."
    fi

    clear_motd
    return 0
}

# Execute main function
main "$@"
exit $?
