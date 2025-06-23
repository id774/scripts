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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
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

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
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
    if [ ! -f "$DBFILE" ]; then
        echo "[ERROR] Database file '$DBFILE' not found. Please ensure Fastladder is properly installed." >&2
        exit 1
    fi
}

# Function to execute SQL query
exec_sql() {
    SQL_COMMAND="sqlite3 -csv -nullvalue 'NULL' $DBFILE"
    echo "$1" | $SQL_COMMAND || {
        echo "[ERROR] Failed to execute SQL command." >&2
        exit 1
    }
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands sqlite3
    check_database

    echo "[INFO] Retrieving feed titles..."
    exec_sql "SELECT title FROM feeds;"

    echo "[INFO] Retrieving feed count..."
    exec_sql "SELECT COUNT(*) FROM feeds;"

    echo "[INFO] Feed retrieval completed."
    return 0
}

# Execute main function
main "$@"
exit $?
