#!/bin/sh

########################################################################
# server_alive_check.sh: Monitor server availability via _is_alive files
#
#  Description:
#  This script checks for the existence and freshness of _is_alive files 
#  sent from various servers to the specified directory. The presence and 
#  last modified timestamp of these files determine the availability of 
#  the servers.
#
#  It scans all files ending with '_is_alive' under the configured directory 
#  and triggers an alert if any file is older than one hour, indicating 
#  potential server downtime or connectivity issues.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-04-07
#       Initial release. Implements _is_alive file monitoring for server health checking.
#
#  Usage:
#      ./server_alive_check.sh
#      This script is intended to be executed automatically via cron.
#
#  Cron Usage:
#  Add the following line to /etc/cron.d/server_alive_check to execute every 5 minutes:
#      */5 * * * * /path/to/server_alive_check.sh
#
#  This ensures the script runs every 5 minutes to continuously monitor
#  server availability based on the presence and freshness of _is_alive files.
#
#  Features:
#  - POSIX-compliant script structure
#  - Full function-based structure with main() entry point
#  - Scans a configurable directory for _is_alive files
#  - Alerts if files are older than one hour
#  - Outputs errors to standard error for logging and monitoring purposes
#  - Validates required commands and directory existence before execution
#
#  Warning:
#  - Ensure the monitored servers regularly generate _is_alive files to avoid false alerts.
#  - Use appropriate permissions and ownership for the monitored directory.
#
########################################################################

# Base directory containing the files to be checked
BASE_DIR="/home/share/received"

# Alert message prefix
ALERT_PREFIX="[ERROR]"

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

# Check if BASE_DIR exists
check_environment() {
    if [ ! -d "$BASE_DIR" ]; then
        echo "$ALERT_PREFIX Directory not found: $BASE_DIR" >&2
        exit 1
    fi
}

# Find all files ending with '_is_alive' under the base directory
find_is_alive_files() {
    find "$BASE_DIR" -type f -name '*_is_alive'
}

# Check if a file is older than 1 hour
is_file_stale() {
    FILE="$1"
    CURRENT_TIME=$(date +%s)
    FILE_TIME=$(stat -c %Y "$FILE" 2>/dev/null || stat -f %m "$FILE" 2>/dev/null)

    # Check if the file modification time could be retrieved
    if [ -z "$FILE_TIME" ]; then
        echo "$ALERT_PREFIX Failed to retrieve modification time for: $FILE" >&2
        return 1
    fi

    # Check if the file is older than 10 minites (600 seconds)
    if [ "$((CURRENT_TIME - FILE_TIME))" -gt 600 ]; then
        return 0
    else
        return 1
    fi
}

# Process all found files and check their status
process_files() {
    FILES=$(find_is_alive_files)

    if [ -z "$FILES" ]; then
        echo "$ALERT_PREFIX No '_is_alive' files found in: $BASE_DIR" >&2
        exit 1
    fi

    for FILE in $FILES; do
        if is_file_stale "$FILE"; then
            echo "$ALERT_PREFIX File is stale: $FILE" >&2
        fi
    done
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_environment
    check_commands find stat date
    process_files
}

# Execute main function
main "$@"
