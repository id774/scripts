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
#  Usage:
#  Run this script to manage favorite pins:
#      ./fav-pins-on-fastladder.sh
#
#  Requirements:
#  - Must be executed on Linux or macOS.
#  - Requires `sqlite3` installed.
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
#       Added system and command checks, improved database handling.
#  v0.1 2016-03-30
#       Initial version.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check required commands
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

# Check if the database file exists
check_db_file() {
    DBFILE="$HOME/fastladder/db/fastladder.db"
    if [ ! -f "$DBFILE" ]; then
        echo "[ERROR] Fastladder database not found at $DBFILE" >&2
        exit 1
    fi
}

# Execute SQL commands safely
exec_sql() {
    echo "$@" | sqlite3 -separator , "$DBFILE"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands sqlite3
    check_db_file

    echo "[INFO] Deleting feeds with zero subscribers..."
    exec_sql "DELETE FROM feeds WHERE subscribers_count = 0;"

    echo "[INFO] Extracting favorite pinned links..."
    exec_sql "SELECT 'fav ' || link FROM pins;"

    echo "[INFO] Deleting all pinned entries..."
    exec_sql "DELETE FROM pins;"

    echo "Fastladder pin cleanup completed."
    return 0
}

# Execute main function
main "$@"
exit $?
