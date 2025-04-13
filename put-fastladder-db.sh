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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
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

# Function to check required commands
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

# Function to check if the database exists
check_database() {
    if [ ! -f "$DB_PATH" ]; then
        echo "[ERROR] Database file '$DB_PATH' not found. Please ensure Fastladder is properly installed." >&2
        exit 1
    fi
}

# Function to vacuum SQLite database
vacuum_database() {
    echo "Vacuuming database..."
    sqlite3 "$DB_PATH" vacuum || {
        echo "[ERROR] Failed to vacuum database." >&2
        exit 1
    }
}

# Function to sync database to remote server
sync_database() {
    echo "Syncing database to $USER@$HOST..."
    rsync -auvz "$DB_PATH" "$USER@$HOST:$DB_PATH" || {
        echo "[ERROR] Failed to sync database to remote server." >&2
        exit 1
    }
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_commands sqlite3 rsync
    check_database
    vacuum_database
    sync_database
    echo "[INFO] Fastladder database successfully synced to $USER@$HOST."
}

# Execute main function
main "$@"
