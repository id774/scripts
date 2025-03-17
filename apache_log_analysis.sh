#!/bin/sh

########################################################################
# apache_log_analysis.sh: Apache Log File Analysis Tool
#
#  Description:
#  This script analyzes Apache SSL access logs to provide insights into
#  web server traffic. It reports on top accessed URLs, referrers, user
#  agents, browser counts, daily accesses, access by time, and recent
#  accesses and referrers.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.6 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.5 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.4 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.3 2023-12-17
#       Enhanced the logic for loading the ignore list by adding a
#       fallback to check in the script's relative parent directory
#       if not found in the current directory.
#       Modified argument handling to accept a single log file path.
#  v1.2 2023-12-14
#       Added checks for log file existence and grep/zgrep/awk availability.
#       Implemented the functionality to ignore IPs listed in apache_ignore.list.
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2022-10-11
#       Initial release.
#
#  Usage:
#  ./apache_log_analysis.sh [log_file_path]
#  Example: ./apache_log_analysis.sh /var/log/apache2/ssl_access.log
#
########################################################################

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

load_ignore_list() {
    SCRIPT_DIR=$(dirname "$0")
    IGNORE_FILE="$SCRIPT_DIR/etc/apache_ignore.list"
    if [ ! -f "$IGNORE_FILE" ]; then
        IGNORE_FILE="$SCRIPT_DIR/../etc/apache_ignore.list"
    fi

    if [ -f "$IGNORE_FILE" ]; then
        IGNORE_IPS=$(awk '!/^#/ && NF' "$IGNORE_FILE" | paste -sd "|" -)
    else
        IGNORE_IPS="127.0.0.1"
        echo "Ignore file not found. Using default ignore IP: $IGNORE_IPS"
    fi
}

analyze_logs() {
    echo "[Access Count]"
    zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -nr | head -n 100

    echo "[Referer]"
    zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100

    echo "[User Agent]"
    zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk -F '"' '{print $6}' | sort | uniq -c | sort -nr | head -n 50

    echo "[Browser]"
    for UA in MSIE Firefox Chrome Safari; do
        COUNT=$(zgrep 'https' "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | grep "$UA" | wc -l)
        echo "$UA: $COUNT"
    done

    echo "[Daily Access]"
    zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk '{print $4}' | cut -b 2-12 | sort | uniq -c

    echo "[Access By Time]"
    grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk '{print $4}' | cut -b 2-15 | sort | uniq -c

    echo "[Recent Accesses]"
    grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -nr | head -n 100

    echo "[Recent Referer]"
    grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100
}

main() {
    check_commands grep zgrep awk cut sort uniq wc paste

    if [ "$#" -ne 1 ]; then
        echo "Usage: $0 log_file_path"
        echo "Example: $0 /var/log/apache2/ssl_access.log"
        exit 0
    fi

    LOG_FILE=$1

    if [ ! -f "$LOG_FILE" ]; then
        echo "Error: Log file not found at $LOG_FILE" >&2
        exit 1
    fi

    load_ignore_list
    analyze_logs
}

main "$@"
