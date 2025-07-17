#!/bin/sh

########################################################################
# vacuum-safari.sh: Optimize Safari Cache Database
#
#  Description:
#  This script performs the SQLite vacuum command on Safari's cache
#  database to optimize it. Vacuuming reclaims free space and can improve
#  performance.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./vacuum-safari.sh
#
#  Notes:
#  - Ensure Safari is not running while executing this script.
#  - It's recommended to back up the cache database before running.
#
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2010-11-29
#       Initial release.
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

# Check if sqlite3 command is available
check_sqlite3() {
    if ! command -v sqlite3 >/dev/null 2>&1; then
        echo "[ERROR] sqlite3 command not found. Please install sqlite3." >&2
        exit 1
    fi
}

# Vacuum Safari's cache database
vacuum_safari_cache() {
    SAFARI_CACHE_DIR="$HOME/Library/Caches/com.apple.Safari"

    if [ -d "$SAFARI_CACHE_DIR" ]; then
        if [ -w "$SAFARI_CACHE_DIR/Cache.db" ]; then
            cd "$SAFARI_CACHE_DIR"
            sqlite3 Cache.db vacuum
            echo "[INFO] Safari cache database vacuumed."
        else
            echo "[ERROR] Safari cache Cache.db not found or not writable." >&2
            exit 1
        fi
    else
        echo "[ERROR] Safari cache directory not found." >&2
        exit 1
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_sqlite3
    vacuum_safari_cache
    return 0
}

# Execute main function
main "$@"
exit $?
