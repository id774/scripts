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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.9 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.8 2025-03-22
#       Unify usage information by extracting help text from header comments.
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
#      ./apache_log_analysis.sh [log_file_path]
#  Example:
#      ./apache_log_analysis.sh /var/log/apache2/ssl_access.log
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

# Load list of IP addresses to ignore from a configuration file.
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
        echo "[WARN] Ignore file not found. Using default ignore IP: $IGNORE_IPS" >&2
    fi
}

# Analyze the log file and output various access statistics.
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

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    if [ "$#" -eq 0 ]; then
        usage
    fi

    check_commands grep zgrep awk cut sort uniq wc paste

    LOG_FILE=$1

    if [ ! -f "$LOG_FILE" ]; then
        echo "[ERROR] Log file not found at $LOG_FILE" >&2
        exit 1
    fi

    load_ignore_list
    analyze_logs
}

# Execute main function
main "$@"
