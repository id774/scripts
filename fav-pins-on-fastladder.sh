#!/bin/sh

########################################################################
# fav-pins-on-fastladder.sh: Manage favorite pins in Fastladder
#
#  Description:
#  This script manages favorite pins in Fastladder by:
#  - Deleting feeds with zero subscribers.
#  - Extracting favorite pinned links.
#  - Deleting all existing pins.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Added system and command checks, improved database handling.
#  v0.1 2016-03-30
#       Initial version.
#
#  Usage:
#  Run this script to manage favorite pins:
#      ./fav-pins-on-fastladder.sh
#
#  Requirements:
#  - Must be executed on Linux or macOS.
#  - Requires `sqlite3` installed.
#  - The Fastladder database (`fastladder.db`) must exist.
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

# Function to check if the database file exists
check_db_file() {
    DBFILE="$HOME/fastladder/db/fastladder.db"
    if [ ! -f "$DBFILE" ]; then
        echo "[ERROR] Fastladder database not found at $DBFILE" >&2
        exit 1
    fi
}

# Function to execute SQL commands safely
exec_sql() {
    echo "$@" | sqlite3 -separator , "$DBFILE"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_commands sqlite3
    check_db_file

    echo "[INFO] Deleting feeds with zero subscribers..."
    exec_sql "DELETE FROM feeds WHERE subscribers_count = 0;"

    echo "[INFO] Extracting favorite pinned links..."
    exec_sql "SELECT 'fav ' || link FROM pins;"

    echo "[INFO] Deleting all pinned entries..."
    exec_sql "DELETE FROM pins;"

    echo "Fastladder pin cleanup completed successfully."
}

# Execute main function
main "$@"
