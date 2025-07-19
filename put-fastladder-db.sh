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
#  Usage:
#  Run this script to vacuum and sync the Fastladder database:
#      ./put-fastladder-db.sh [user] [host]
#
#  Requirements:
#  - Must be executed on Linux or macOS.
#  - Requires `sqlite3` and `rsync` installed.
#  - The Fastladder database (`fastladder.db`) must exist.
#
#  Version History:
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Added system validation, command checks, and database validation.
#       Improved error handling and argument parsing.
#  v0.1 2016-04-09
#       Initial version.
#
########################################################################

DB_PATH="$HOME/fastladder/db/fastladder.db"
USER="${1:-debian}"
HOST="${2:-harpuia}"

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
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

# Check if the database exists
check_database() {
    if [ ! -f "$DB_PATH" ]; then
        echo "[ERROR] Database file '$DB_PATH' not found. Please ensure Fastladder is properly installed." >&2
        exit 1
    fi
}

# Vacuum SQLite database
vacuum_database() {
    echo "Vacuuming database..."
    sqlite3 "$DB_PATH" vacuum || {
        echo "[ERROR] Failed to vacuum database." >&2
        exit 1
    }
}

# Sync database to remote server
sync_database() {
    echo "Syncing database to $USER@$HOST..."
    rsync -auvz "$DB_PATH" "$USER@$HOST:$DB_PATH" || {
        echo "[ERROR] Failed to sync database to remote server." >&2
        exit 1
    }
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands sqlite3 rsync
    check_database
    vacuum_database
    sync_database
    echo "[INFO] Fastladder database successfully synced to $USER@$HOST."
    return 0
}

# Execute main function
main "$@"
exit $?
