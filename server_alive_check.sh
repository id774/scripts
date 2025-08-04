#!/bin/sh

########################################################################
# server_alive_check.sh: Monitor server availability via _is_alive files
#
#  Description:
#  This script checks for the existence and freshness of _is_alive files
#  sent from various servers to a specified directory.
#  You can optionally specify the base directory as the first argument.
#  If omitted, the default directory is /home/share/received.
#
#  It scans all files ending with '_is_alive' under the configured directory
#  and triggers an alert if any file is older than the threshold time,
#  indicating potential server downtime or connectivity issues.
#
#  Hosts whose filenames begin with 'VM' are treated as virtual hosts and
#  excluded from alerts even if stale. A separate log message is displayed
#  when only VM-prefixed hosts are stale.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./server_alive_check.sh [base_directory]
#      Optionally specify [base_directory] to override the default (/home/share/received).
#      This script is designed for periodic execution via cron.
#
#  Cron Usage:
#  Add the following line to /etc/cron.d/server_alive_check to execute every 5 minutes:
#      */5 * * * * /path/to/server_alive_check.sh [base_directory]
#  Replace [base_directory] with a custom path if needed.
#
#  This ensures the script runs every 5 minutes to continuously monitor
#  server availability based on the presence and freshness of _is_alive files.
#
#  Features:
#  - Scans a configurable directory for _is_alive files.
#  - Alerts if non-VM files are older than the threshold.
#  - Ignores VM-prefixed files when stale, with a clear info message.
#  - Outputs errors to standard error for logging and monitoring purposes.
#  - Validates required commands and directory existence before execution.
#
#  Warning:
#  - Ensure the monitored servers regularly generate _is_alive files to avoid false alerts.
#  - Use appropriate permissions and ownership for the monitored directory.
#
#  Error Conditions:
#  1. One or more regular hosts are stale.
#  2. No '_is_alive' files found.
#  3. Source directory does not exist.
#
#  Version History:
#  v1.7 2025-08-01
#       Support optional argument to specify BASE_DIR. Default remains /home/share/received.
#  v1.6 2025-07-28
#       Distinguish VM-prefixed hosts and exclude them from alerts with consistent log formatting.
#       Support gstat on macOS to ensure GNU-compatible stat behavior.
#  v1.5 2025-07-20
#       Sort the server list by filename in ascending order for consistent output.
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-05-06
#       Show timestamp and success/failure status for each file.
#       Add final summary of result, and define configurable STALE_THRESHOLD.
#  v1.2 2025-04-26
#       Remove ALERT_PREFIX and refine log levels to use [ERROR] and [WARN] appropriately.
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-04-07
#       Initial release. Implements _is_alive file monitoring for server health checking.
#
########################################################################

# Threshold in seconds to consider a file stale (default: 600 seconds = 10 minutes)
STALE_THRESHOLD=600

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

# Check if BASE_DIR exists
check_environment() {
    if [ ! -d "$BASE_DIR" ]; then
        echo "[ERROR] Directory not found: $BASE_DIR" >&2
        exit 3
    fi
}

# Detect macOS and prefer gstat if available
choice_stat_command() {
    if [ "$(uname)" = "Darwin" ]; then
        if command -v gstat >/dev/null 2>&1; then
            stat_cmd="gstat"
        else
            stat_cmd="stat"
        fi
    else
        stat_cmd="stat"
    fi
}

# Find all files ending with '_is_alive' under the base directory
find_is_alive_files() {
    find "$BASE_DIR" -type f -name '*_is_alive'
}

# Check if a file is older than 10 minites
is_file_stale() {
    FILE="$1"
    CURRENT_TIME=$(date +%s)
    FILE_TIME=$($stat_cmd -c %Y "$FILE" 2>/dev/null || stat -f %m "$FILE" 2>/dev/null)

    # Check if the file modification time could be retrieved
    if [ -z "$FILE_TIME" ]; then
        echo "[WARN] Failed to retrieve modification time for: $FILE" >&2
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
    FILES=$(find_is_alive_files | sort)

    if [ -z "$FILES" ]; then
        echo "[ERROR] No '_is_alive' files found in: $BASE_DIR" >&2
        exit 2
    fi

    CURRENT_TIME=$(date +%s)
    REGULAR_STALE_FOUND=0
    VM_STALE_FOUND=0

    for FILE in $FILES; do
        FILE_TIME=$($stat_cmd -c %Y "$FILE" 2>/dev/null || stat -f %m "$FILE" 2>/dev/null)
        FILE_DATE=$(date -d "@$FILE_TIME" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || date -r "$FILE_TIME" "+%Y-%m-%d %H:%M:%S" 2>/dev/null)

        if [ -z "$FILE_TIME" ] || [ -z "$FILE_DATE" ]; then
            echo "[WARN] Failed to retrieve modification time for: $FILE" >&2
            REGULAR_STALE_FOUND=1
            continue
        fi

        AGE=$((CURRENT_TIME - FILE_TIME))
        AGE_MIN=$((AGE / 60))
        AGE_SEC=$((AGE % 60))
        ELAPSED="elapsed time: ${AGE_MIN}m ${AGE_SEC}s"
        BASENAME=$(basename "$FILE")

        if [ "$AGE" -gt "$STALE_THRESHOLD" ]; then
            if printf '%s\n' "$BASENAME" | grep -iq '^vm'; then
                echo "[INFO] File is stale: $BASENAME (last updated: $FILE_DATE, $ELAPSED, VM-prefixed: ignored)"
                VM_STALE_FOUND=1
            else
                echo "[WARN] File is stale: $BASENAME (last updated: $FILE_DATE, $ELAPSED)" >&2
                REGULAR_STALE_FOUND=1
            fi
        else
            echo "[INFO] File is fresh: $BASENAME (last updated: $FILE_DATE, $ELAPSED)"
        fi

    done

    if [ "$REGULAR_STALE_FOUND" -eq 1 ]; then
        echo "[WARN] One or more non-VM hosts are stale." >&2
        exit 1
    elif [ "$VM_STALE_FOUND" -eq 1 ]; then
        echo "[INFO] Only VM-prefixed hosts are missing. No alert triggered."
        exit 0
    else
        echo "[INFO] All files are fresh."
        exit 0
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Set BASE_DIR from argument or use default
    if [ -n "$1" ]; then
        BASE_DIR="$1"
    else
        BASE_DIR="/home/share/received"
    fi

    check_environment
    check_commands find stat date basename grep sort
    choice_stat_command
    process_files
    return 0
}

# Execute main function
main "$@"
exit $?
