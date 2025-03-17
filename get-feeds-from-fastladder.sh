#!/bin/sh

########################################################################
# get-feeds-from-fastladder.sh: Retrieve Feeds from Fastladder Database
#
#  Description:
#  This script queries the Fastladder SQLite database and retrieves:
#  - A list of all feed titles.
#  - The total count of feeds.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-16
#       Added system validation, command checks, and database validation.
#  v0.1 2016-03-23
#       Initial version.
#
#  Usage:
#  Run this script to retrieve feed titles and counts:
#      ./get-feeds-from-fastladder.sh
#
#  Requirements:
#  - Must be executed on Linux or macOS.
#  - Requires `sqlite3` installed.
#  - The Fastladder database (`fastladder.db`) must exist.
#
########################################################################

DBFILE="$HOME/fastladder/db/fastladder.db"

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
    if [ ! -f "$DBFILE" ]; then
        echo "Error: Database file '$DBFILE' not found. Please ensure Fastladder is properly installed." >&2
        exit 1
    fi
}

# Function to execute SQL query
exec_sql() {
    SQL_COMMAND="sqlite3 -csv -nullvalue 'NULL' $DBFILE"
    echo "$1" | $SQL_COMMAND || {
        echo "Error: Failed to execute SQL command." >&2
        exit 1
    }
}

# Main function to execute the script
main() {
    check_commands sqlite3
    check_database

    echo "Retrieving feed titles..."
    exec_sql "SELECT title FROM feeds;"

    echo "Retrieving feed count..."
    exec_sql "SELECT COUNT(*) FROM feeds;"

    echo "Feed retrieval completed."
}

# Execute main function
main "$@"
