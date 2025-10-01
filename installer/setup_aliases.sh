#!/bin/sh

########################################################################
# setup_aliases.sh: Centralize system mail delivery and clean local spools
#
#  Description:
#  This script automates the setup of email aliases for all interactive
#  user accounts on the system. For each login-capable user, it ensures
#  that a corresponding alias entry in /etc/aliases exists in the form:
#      username: root
#
#  After updating aliases, it removes the alias "username: root" for the
#  currently executing user to avoid redundant or looped delivery.
#  Then it applies the alias changes using 'newaliases' and clears any
#  existing local mail spool files under /var/mail that are non-empty.
#
#  This helps centralize cron and system-generated mail notifications to
#  root (or its alias target), while cleaning up unread or orphaned mail.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_alias.sh
#  Run this script as a general user; it will invoke sudo where necessary.
#
#  Requirements:
#  - usershells.py must exist and be executable via $SCRIPTS environment variable.
#  - The system must have a compatible MTA (e.g., Postfix or Sendmail).
#
#  Version History:
#  v1.0 2025-08-05
#       Initial release. Add missing aliases for interactive users and run newaliases.
#
########################################################################

ALIASES_FILE="/etc/aliases"

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
    "$SCRIPT_PATH" --name-only | while read -r user; do
        grep -q "^${user}:" "$ALIASES_FILE" && continue
        echo "[INFO] Adding alias: $user: root"
        echo "${user}: root" | sudo tee -a "$ALIASES_FILE" >/dev/null
    done
}

# Remove self alias like "username: root" from /etc/aliases
remove_self_alias() {
    username=$(id -un)

    if grep -q "^${username}: root$" "$ALIASES_FILE"; then
        echo "[INFO] Removing self alias: $username: root"
        sudo sed -i "/^${username}: root$/d" "$ALIASES_FILE"
    fi
}

# Always run newaliases
apply_changes() {
    echo "[INFO] Running newaliases..."

    if ! sudo newaliases; then
        echo "[ERROR] Failed to run newaliases. Check /etc/aliases for syntax errors or MTA installation." >&2
        exit 1
    fi

    echo "[INFO] Alias update completed."
}

# Ensure /var/mail/username exists and is writable
ensure_mail_spool() {
    for user in $( "$SCRIPT_PATH" --name-only ); do
        mailfile="/var/mail/$user"
        if [ ! -f "$mailfile" ]; then
            echo "[INFO] Creating missing mail spool: $mailfile"
            sudo touch "$mailfile"
            sudo chown "$user:mail" "$mailfile"
        fi
    done
}

# Set permissions on all /var/mail/* to 600
set_mail_permissions() {
    echo "[INFO] Enforcing permission 600 on all mail spools in /var/mail..."
    for mailfile in /var/mail/*; do
        [ -f "$mailfile" ] || continue
        sudo chmod 0600 "$mailfile"
    done
}

# Truncate all mail spool files in /var/mail
clear_all_mail_spools() {
    echo "[INFO] Clearing all user mail spools in /var/mail..."
    for mailfile in /var/mail/*; do
        if [ -f "$mailfile" ] && [ -s "$mailfile" ]; then
            sudo truncate -s 0 "$mailfile"
            echo "[INFO] Emptied $mailfile"
        fi
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo grep tee newaliases sed id truncate
    check_scripts

    SCRIPT_PATH="$SCRIPTS/usershells.py"
    check_script
    check_sudo

    add_missing_aliases
    remove_self_alias
    apply_changes
    ensure_mail_spool
    set_mail_permissions
    clear_all_mail_spools

    return 0
}

# Execute main function
main "$@"
exit $?
