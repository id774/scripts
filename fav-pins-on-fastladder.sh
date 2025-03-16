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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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

# Function to check if required commands exist
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install it and try again." >&2
            exit 127
        fi
    done
}

# Function to check if the database file exists
check_db_file() {
    DBFILE="$HOME/fastladder/db/fastladder.db"
    if [ ! -f "$DBFILE" ]; then
        echo "Error: Fastladder database not found at $DBFILE" >&2
        exit 1
    fi
}

# Function to execute SQL commands safely
exec_sql() {
    echo "$@" | sqlite3 -separator , "$DBFILE"
}

# Main execution function
main() {
    check_commands sqlite3
    check_db_file

    echo "Deleting feeds with zero subscribers..."
    exec_sql "DELETE FROM feeds WHERE subscribers_count = 0;"

    echo "Extracting favorite pinned links..."
    exec_sql "SELECT 'fav ' || link FROM pins;"

    echo "Deleting all pinned entries..."
    exec_sql "DELETE FROM pins;"

    echo "Fastladder pin cleanup completed successfully."
}

main "$@"
