#!/bin/bash

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

# Check if grep, zgrep, and awk commands are available
if ! command -v grep &> /dev/null; then
    echo "Error: grep command not found."
    exit 1
fi

if ! command -v zgrep &> /dev/null; then
    echo "Error: zgrep command not found."
    exit 1
fi

if ! command -v awk &> /dev/null; then
    echo "Error: awk command not found."
    exit 1
fi

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

# Load the ignore list from the first available location
IGNORE_FILE=""
if [ -f "./etc/apache_ignore.list" ]; then
    IGNORE_FILE="./etc/apache_ignore.list"
elif [ -f "$(dirname "${BASH_SOURCE[0]}")/../etc/apache_ignore.list" ]; then
    IGNORE_FILE="$(dirname "${BASH_SOURCE[0]}")/../etc/apache_ignore.list"
fi

# Use localhost as default if the ignore file is empty or not found
if [ -f "$IGNORE_FILE" ]; then
    IGNORE_IPS=$(awk '!/^#/ && NF' "$IGNORE_FILE" | paste -sd "|" -)
    if [ -z "$IGNORE_IPS" ]; then
        IGNORE_IPS="127.0.0.1"
    fi
else
    IGNORE_IPS="127.0.0.1"
    echo "Ignore file not found. Using default ignore IP: $IGNORE_IPS"
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

