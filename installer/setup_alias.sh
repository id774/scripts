#!/bin/sh

########################################################################
# setup_aliases.sh: Ensure /etc/aliases has entries for all login users
#
#  Description:
#  This script ensures that each user account with an interactive shell
#  has a corresponding entry in /etc/aliases. If an alias is missing,
#  it adds the entry in the form of "username: root".
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_aliases.sh
#  Run this script as a general user; it will invoke sudo where necessary.
#
#  Requirements:
#  - usershells.py must exist and be executable in the same directory or in PATH.
#
#  Version History:
#  v1.0 2025-08-05
#       Initial release. Add missing aliases for interactive users and run newaliases.
#
########################################################################

ALIASES_FILE="/etc/aliases"
CHANGES_MADE=0

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
    if [ "$(uname -s)" != "Linux" ]; then
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        exit 1
    fi
}

# Verify usershells.py is executable
check_script() {
    if [ ! -x "$SCRIPT_PATH" ]; then
        echo "[ERROR] Cannot execute $SCRIPT_PATH" >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Add missing alias entries to /etc/aliases
add_missing_aliases() {
    "$SCRIPT_PATH" | awk '{print $1}' | while read -r user; do
        grep -q "^${user}:" "$ALIASES_FILE" && continue
        echo "[INFO] Adding alias: $user: root"
        echo "${user}: root" | sudo tee -a "$ALIASES_FILE" >/dev/null
        CHANGES_MADE=1
    done
}

# Run newaliases if changes were made
apply_changes() {
    if [ "$CHANGES_MADE" -eq 1 ]; then
        echo "[INFO] Running newaliases..."
        sudo newaliases
        echo "[INFO] Alias update completed."
    else
        echo "[INFO] No changes needed. All aliases already present."
    fi
}

main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo grep awk tee newaliases
    check_scripts

    SCRIPT_PATH="$SCRIPTS/usershells.py"
    check_script
    check_sudo

    add_missing_aliases
    apply_changes

    return 0
}

main "$@"
exit $?
