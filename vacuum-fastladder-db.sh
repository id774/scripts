#!/bin/sh

########################################################################
# vacuum-fastladder-db.sh: Optimize Fastladder SQLite Database
#
#  Description:
#  This script is used to vacuum and optimize the Fastladder SQLite database.
#  It creates a new database file from the existing database after running the vacuum command.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script from the command line:
#      ./vacuum-fastladder-db.sh
#
#  Notes:
#  - Ensure that the Fastladder application is not actively using the database when this script is run.
#  - This script should be run from the root directory of the Fastladder installation.
#  - Backup the existing database before running this script as a safety precaution.
#  - The script requires sqlite3 to be installed on the system.
#
#  Version History:
#  v1.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.7 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.6 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.5 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.3 2024-01-22
#       Integrated check_commands function for dependency checks.
#  v1.2 2023-12-08
#       Added file and directory existence checks for the Fastladder database.
#  v1.1 2023-12-07
#       Added check for sqlite3 program and updated documentation.
#  v1.0 2019-03-18
#       Initial release.
#
########################################################################

# Define the Fastladder database directory and file path
DB_DIR="$HOME/fastladder/db"
DB_PATH="$DB_DIR/fastladder.db"

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

# Check if the database directory and file exist
check_database() {
    if [ ! -d "$DB_DIR" ]; then
        echo "[ERROR] Fastladder database directory does not exist. Please check the path and try again." >&2
        exit 1
    fi

    if [ ! -f "$DB_PATH" ]; then
        echo "[ERROR] Fastladder database file does not exist. Please check the path and try again." >&2
        exit 2
    fi
}

# Change to the Fastladder database directory
change_to_db_dir() {
    cd "$DB_DIR" || exit 1
}

# Vacuum and optimize the database
vacuum_and_optimize_db() {
    # Remove existing temporary database file if it exists
    test -f new.db && rm -vf new.db

    # Vacuum the Fastladder SQLite database
    sqlite3 "$DB_PATH" vacuum

    # Dump the current database and create a new optimized database
    sqlite3 fastladder.db .dump | sqlite3 new.db
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands sqlite3 rm
    check_database
    change_to_db_dir
    vacuum_and_optimize_db
    return 0
}

# Execute main function
main "$@"
exit $?
