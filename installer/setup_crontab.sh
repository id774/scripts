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
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-05
#       Adjusted cron.weekend to only run on Saturdays.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-21
#       Initial release. Ensures cron.weekday and cron.weekend entries exist.
#
#  Usage:
#      ./setup_crontab.sh
#  Run this script as a general user; it will invoke sudo where necessary.
#
#  Notes:
#  It ensures that the necessary cron directories exist before modifying /etc/crontab.
#  The script verifies and maintains the following cron jobs:
#    "01 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday"
#    "01 23 * * 6   root cd / && run-parts --report /etc/cron.weekend"
#
########################################################################

# Path to the system-wide crontab file
CRONTAB_FILE="/etc/crontab"

# Cron job entries to check and add if missing
WEEKDAY_ENTRY="01 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday"
WEEKEND_ENTRY="01 23 * * 6   root cd / && run-parts --report /etc/cron.weekend"

# Track changes
CHANGES_MADE=0

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

# Extract and check only the command part of the cron entry
extract_and_check_command() {
    command_part=$(echo "$1" | cut -d' ' -f6-)
    grep -q "$command_part" "$CRONTAB_FILE"
}

# Function to add an entry to crontab if it does not exist
add_entry() {
    entry="$1"
    if ! extract_and_check_command "$entry"; then
        printf "%s\n" "$entry" | sudo tee -a "$CRONTAB_FILE" > /dev/null
        echo "Added entry: $entry"
        CHANGES_MADE=1
    else
        echo "Entry already exists based on command. No changes made."
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
    check_commands grep uname sudo mkdir tee cut
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
