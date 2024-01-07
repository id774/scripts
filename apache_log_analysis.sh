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

check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Check if grep, zgrep, and awk commands are available
check_commands grep zgrep awk

# Check for correct number of arguments
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 log_file_path"
    echo "Example: $0 /var/log/apache2/ssl_access.log"
    exit 1
fi

# Set the log file path
LOG_FILE=$1

# Check if log file exists
if [ ! -f "$LOG_FILE" ]; then
    echo "Error: Log file not found at $LOG_FILE"
    exit 1
fi

# Determine the script's directory
SCRIPT_DIR=$(dirname "$0")

# Load the ignore list from the first available location
IGNORE_FILE="$SCRIPT_DIR/etc/apache_ignore.list"
if [ ! -f "$IGNORE_FILE" ]; then
    IGNORE_FILE="$SCRIPT_DIR/../etc/apache_ignore.list"
    if [ ! -f "$IGNORE_FILE" ]; then
        IGNORE_IPS="127.0.0.1"
        echo "Ignore file not found. Using default ignore IP: $IGNORE_IPS"
    else
        IGNORE_IPS=$(awk '!/^#/ && NF' "$IGNORE_FILE" | paste -sd "|" -)
    fi
else
    IGNORE_IPS=$(awk '!/^#/ && NF' "$IGNORE_FILE" | paste -sd "|" -)
fi

# Function to display top accessed URLs
echo "[Access Count]"
zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -nr | head -n 100

# Function to display top referrers
echo "[Referer]"
zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100

# Function to display top user agents
echo "[User Agent]"
zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk -F '"' '{print $6}' | sort | uniq -c | sort -nr | head -n 50

# Function to count accesses by browser type
echo "[Browser]"
for UA in MSIE Firefox Chrome Safari; do
    COUNT=$(zgrep 'https' "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | grep "$UA" | wc -l)
    echo "$UA: $COUNT"
done

# Function to display daily access count
echo "[Daily Access]"
zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk '{print $4}' | cut -b 2-12 | sort | uniq -c

# Function to display access count by time
echo "[Access By Time]"
grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk '{print $4}' | cut -b 2-15 | sort | uniq -c

# Function to display recent accesses
echo "[Recent Accesses]"
grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -nr | head -n 100

# Function to display recent referrers
echo "[Recent Referer]"
grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100

