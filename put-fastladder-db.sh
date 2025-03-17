#!/bin/sh

########################################################################
# put-fastladder-db.sh: Sync Fastladder Database to Remote Server
#
#  Description:
#  This script vacuums and transfers the Fastladder SQLite database to a
#  remote server via `rsync`.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-16
#       Added system validation, command checks, and database validation.
#       Improved error handling and argument parsing.
#  v0.1 2016-04-09
#       Initial version.
#
#  Usage:
#  Run this script to vacuum and sync the Fastladder database:
#      ./put-fastladder-db.sh [user] [host]
#
#  Requirements:
#  - Must be executed on Linux or macOS.
#  - Requires `sqlite3` and `rsync` installed.
#  - The Fastladder database (`fastladder.db`) must exist.
#
########################################################################

DB_PATH="$HOME/fastladder/db/fastladder.db"
USER="${1:-debian}"
HOST="${2:-harpuia}"

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check if the database exists
check_database() {
    if [ ! -f "$DB_PATH" ]; then
        echo "Error: Database file '$DB_PATH' not found. Please ensure Fastladder is properly installed." >&2
        exit 1
    fi
}

# Function to vacuum SQLite database
vacuum_database() {
    echo "Vacuuming database..."
    sqlite3 "$DB_PATH" vacuum || {
        echo "Error: Failed to vacuum database." >&2
        exit 1
    }
}

# Function to sync database to remote server
sync_database() {
    echo "Syncing database to $USER@$HOST..."
    rsync -auvz "$DB_PATH" "$USER@$HOST:$DB_PATH" || {
        echo "Error: Failed to sync database to remote server." >&2
        exit 1
    }
}

# Main function to execute the script
main() {
    check_commands sqlite3 rsync
    check_database
    vacuum_database
    sync_database
    echo "Fastladder database successfully synced to $USER@$HOST."
}

# Execute main function
main "$@"
