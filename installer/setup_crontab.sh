#!/bin/sh

########################################################################
# setup_crontab.sh: Ensure cron.weekday and cron.weekend exist in /etc/crontab
#
#  Description:
#  This script ensures that predefined cron job entries exist in the
#  system-wide crontab file (/etc/crontab). If the specified entries
#  for cron.weekday and cron.weekend do not exist, they are automatically added.
#  Additionally, it ensures that the required directories (/etc/cron.weekday
#  and /etc/cron.weekend) exist by creating them if necessary.
#
#  The script verifies and maintains the following cron jobs:
#    "01 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday"
#    "01 23 * * 0,6 root cd / && run-parts --report /etc/cron.weekend"
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-21
#       Initial release. Ensures cron.weekday and cron.weekend entries exist.
#
#  Usage:
#      ./setup_crontab.sh
#  Run this script as a general user; it will invoke sudo where necessary.
#
#  Notes:
#  - This script uses sudo internally for privileged operations.
#  - It ensures that the necessary cron directories exist before modifying /etc/crontab.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}


# Path to the system-wide crontab file
CRONTAB_FILE="/etc/crontab"

# Cron job entries to check and add if missing
WEEKDAY_ENTRY="01 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday"
WEEKEND_ENTRY="01 23 * * 0,6 root cd / && run-parts --report /etc/cron.weekend"

# Track changes
CHANGES_MADE=0

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to check if an entry exists in crontab
check_entry() {
    entry="$1"
    grep -Fxq "$entry" "$CRONTAB_FILE"
}

# Function to add an entry to crontab if it does not exist
add_entry() {
    entry="$1"
    if ! check_entry "$entry"; then
        printf "%s\n" "$entry" | sudo tee -a "$CRONTAB_FILE" > /dev/null
        echo "Added entry: $entry"
        CHANGES_MADE=1
    fi
}

# Function to ensure required directories exist
create_directories() {
    for dir in /etc/cron.weekday /etc/cron.weekend; do
        if [ ! -d "$dir" ]; then
            sudo mkdir -p "$dir"
            echo "Created directory: $dir"
        fi
    done
}

# Main function to execute all setup tasks
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands grep uname sudo mkdir tee
    check_sudo
    create_directories
    add_entry "$WEEKDAY_ENTRY"
    add_entry "$WEEKEND_ENTRY"
    if [ "$CHANGES_MADE" -eq 1 ]; then
        echo "Crontab setup completed."
    else
        echo "No changes were made. Everything is already set up."
    fi
}

# Execute main function
main "$@"
