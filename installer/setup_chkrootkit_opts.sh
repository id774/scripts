#!/bin/sh

########################################################################
# setup_chkrootkit_opts.sh: Ensure quiet mode for daily chkrootkit runs
#
#  Description:
#  This script inspects the /etc/cron.daily/chkrootkit file for an empty
#  RUN_DAILY_OPTS setting (RUN_DAILY_OPTS="") and replaces it with
#  RUN_DAILY_OPTS="-q" to enable quiet mode. This prevents chkrootkit from
#  sending daily emails when no infections are found. The change preserves
#  file permissions, affects only the targeted line, and is idempotent.
#  If the setting is already configured with "-q" or another non-empty value,
#  no edits are made.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_chkrootkit_opts.sh
#  Run as a regular user with sudo privileges; the script will invoke sudo
#  where necessary.
#
#  Notes:
#  - No backups are created; edits are applied directly when needed.
#  - Only the exact empty assignment RUN_DAILY_OPTS="" is replaced.
#  - The script preserves the rest of the file unmodified.
#  - Designed for Debian-family Linux systems with chkrootkit installed.
#
#  Requirements:
#  - Linux operating system (Debian/Ubuntu family recommended)
#  - sudo privileges for modifying /etc/cron.daily/chkrootkit
#  - Commands: sudo, awk, grep, mv, sh
#
#  Version History:
#  v1.0 2025-08-14
#       Initial release.
#
########################################################################

CRON_FILE="/etc/cron.daily/chkrootkit"

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

# Confirm /etc/cron.daily/chkrootkit exists
check_cron_file() {
    if ! sudo test -f "$CRON_FILE"; then
        echo "[ERROR] $CRON_FILE not found." >&2
        exit 1
    fi
}

# Detect if RUN_DAILY_OPTS is set to empty
has_empty_run_opts() {
    sudo grep -Eq '^[[:space:]]*RUN_DAILY_OPTS=""[[:space:]]*$' "$CRON_FILE"
}

# Replace empty RUN_DAILY_OPTS with "-q"
replace_run_opts() {
    if has_empty_run_opts; then
        tmp="/tmp/setup_chkrootkit_opts.$$"
        sudo awk '
/^[[:space:]]*RUN_DAILY_OPTS=""/ {
    sub(/RUN_DAILY_OPTS=""/, "RUN_DAILY_OPTS=\"-q\"")
}
{ print }
' "$CRON_FILE" > "$tmp" && sudo mv "$tmp" "$CRON_FILE"
        echo "[INFO] Updated RUN_DAILY_OPTS to \"-q\" in $CRON_FILE"
    else
        echo "[INFO] RUN_DAILY_OPTS is already set. No changes made."
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
    check_cron_file

    replace_run_opts
    return 0
}

# Execute main function
main "$@"
exit $?
