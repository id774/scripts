#!/bin/sh

########################################################################
# setup_pamd.sh: Manage pam_wheel.so settings in /etc/pam.d/su and clear /etc/motd
#
#  Description:
#  This script adjusts the su PAM policy by enabling an intended
#  'auth required pam_wheel.so' line if it is commented, and disabling an
#  active 'auth sufficient pam_wheel.so trust' line to prevent passwordless
#  root access for wheel group members. All changes preserve existing
#  indentation and affect only those targeted lines. If both lines are already
#  in the desired state or absent, no edits are made. Regardless of changes to
#  /etc/pam.d/su, the script truncates /etc/motd to empty if it exists. It is
#  idempotent and uses only POSIX-compliant utilities without creating backups.
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
#  - Leading spaces in target lines are preserved; "# " is prefixed or removed
#    only as required.
#  - 'pam_wheel.so trust' is always commented out if active to avoid unintended
#    passwordless root access for wheel group members.
#  - /etc/motd is always emptied if it exists, regardless of /etc/pam.d/su edits.
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

# Decide if /etc/pam.d/su contains a commented pam_wheel.so line (needs uncomment)
needs_edit() {
    sudo grep -Eq '^[[:space:]]*#.*pam_wheel\.so' "$PAM_FILE"
}

# Enable 'auth required pam_wheel.so' if commented
enable_pam_wheel_required() {
    if sudo grep -Eq '^[[:space:]]*#[[:space:]]*auth[[:space:]]+required[[:space:]]+pam_wheel\.so[[:space:]]*$' "$PAM_FILE"; then
        tmp="/tmp/setup_pamd.required.$$"
        sudo awk '
/^[[:space:]]*#[[:space:]]*auth[[:space:]]+required[[:space:]]+pam_wheel\.so[[:space:]]*$/ {
    m = match($0, /^[[:space:]]*/); indent = substr($0, 1, RLENGTH)
    rest = substr($0, RLENGTH + 1)
    if (substr(rest,1,1) == "#") {
        rest = substr(rest, 2)
        if (substr(rest,1,1) == " ") rest = substr(rest, 2)
    }
    print indent rest
    next
}
{ print }
' "$PAM_FILE" > "$tmp" && sudo mv "$tmp" "$PAM_FILE"
        echo "[INFO] Enabled auth required pam_wheel.so"
    else
        echo "[INFO] auth required pam_wheel.so already enabled."
    fi
}

# Disable 'auth sufficient pam_wheel.so trust' if active
disable_pam_wheel_trust() {
    if sudo grep -Eq '^[[:space:]]*auth[[:space:]]+sufficient[[:space:]]+pam_wheel\.so[[:space:]]+trust([[:space:]]|$)' "$PAM_FILE"; then
        tmp="/tmp/setup_pamd.trust.$$"
        sudo awk '
/^[[:space:]]*auth[[:space:]]+sufficient[[:space:]]+pam_wheel\.so[[:space:]]+trust([[:space:]]|$)/ {
    m = match($0, /^[[:space:]]*/); indent = substr($0, 1, RLENGTH)
    authpos = match($0, /auth[[:space:]]+sufficient[[:space:]]+pam_wheel\.so[[:space:]]+trust/)
    rest = substr($0, authpos)
    print indent "# " rest
    next
}
{ print }
' "$PAM_FILE" > "$tmp" && sudo mv "$tmp" "$PAM_FILE"
        echo "[INFO] Disabled auth sufficient pam_wheel.so trust"
    else
        echo "[INFO] pam_wheel.so trust already disabled."
    fi
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

    enable_pam_wheel_required
    disable_pam_wheel_trust
    clear_motd
    return 0
}

# Execute main function
main "$@"
exit $?
