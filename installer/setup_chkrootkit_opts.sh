#!/bin/sh

########################################################################
# setup_chkrootkit_opts.sh: Ensure quiet mode for daily chkrootkit runs
#
#  Description:
#  This script inspects the /etc/chkrootkit/chkrootkit.conf file first,
#  and if not present or not containing an empty RUN_DAILY_OPTS setting,
#  falls back to checking /etc/cron.daily/chkrootkit. If an empty
#  RUN_DAILY_OPTS assignment (RUN_DAILY_OPTS="") is found, it is replaced
#  with RUN_DAILY_OPTS="-q" to enable quiet mode. This prevents chkrootkit
#  from sending daily emails when no infections are found. The change
#  preserves file permissions, affects only the targeted line, and is
#  idempotent. If the setting is already configured with "-q" or another
#  non-empty value, no edits are made.
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
#  - Both config and cron.daily scripts are supported; config is preferred.
#  - The script preserves the rest of the file unmodified.
#  - Designed for Debian-family Linux systems with chkrootkit installed.
#
#  Requirements:
#  - Linux operating system (Debian/Ubuntu family recommended)
#  - sudo privileges for modifying chkrootkit config or cron.daily
#  - Commands: sudo, awk, grep, mv, sh
#
#  Version History:
#  v1.0 2025-08-14
#       Initial release.
#  v1.1 2025-08-14
#       Support editing /etc/chkrootkit/chkrootkit.conf before cron.daily
#
########################################################################

# Primary and fallback targets
CONFIG_FILE="/etc/chkrootkit/chkrootkit.conf"
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

# Ensure at least one target file exists
check_targets() {
    if sudo test -f "$CONFIG_FILE"; then
        return 0
    fi
    if sudo test -f "$CRON_FILE"; then
        return 0
    fi
    echo "[ERROR] Neither $CONFIG_FILE nor $CRON_FILE found." >&2
    exit 1
}

# Select target file that needs change; echo its path or nothing
select_target_file() {
    # pattern: allow spaces and trailing comments; no value inside quotes
    pat='^[[:space:]]*RUN_DAILY_OPTS[[:space:]]*=[[:space:]]*""([[:space:]]*(#.*)?)?$'
    if sudo test -f "$CONFIG_FILE"; then
        if sudo grep -Eq "$pat" "$CONFIG_FILE"; then
            echo "$CONFIG_FILE"
            return 0
        fi
    fi
    if sudo test -f "$CRON_FILE"; then
        if sudo grep -Eq "$pat" "$CRON_FILE"; then
            echo "$CRON_FILE"
            return 0
        fi
    fi
    echo ""
    return 1
}

# Replace empty RUN_DAILY_OPTS with "-q" in the selected file
replace_run_opts() {
    target="$(select_target_file)"
    if [ -n "$target" ]; then
        tmp="/tmp/setup_chkrootkit_opts.$$"
        sudo awk '
            # Strip trailing CR if present (CRLF files)
            { sub(/\r$/, "") }
            # Replace only the empty assignment; keep any trailing comment
            /^[[:space:]]*RUN_DAILY_OPTS([[:space:]]*)=([[:space:]]*)""([[:space:]]*(#.*)?)?$/ {
                sub(/=[[:space:]]*""/, "=\"-q\"")
            }
            { print }
        ' "$target" > "$tmp" && sudo mv "$tmp" "$target"
        echo "[INFO] Updated RUN_DAILY_OPTS to \"-q\" in $target"
        return 0
    fi
    echo "[INFO] RUN_DAILY_OPTS is already set. No changes made."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands grep mv awk
    check_sudo
    check_targets

    replace_run_opts
    return 0
}

# Execute main function
main "$@"
exit $?
