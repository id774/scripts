#!/bin/sh

########################################################################
# apache_log_analysis.sh: Apache Log File Analysis Tool
#
#  Description:
#  This script analyzes Apache SSL access logs to provide insights into
#  web server traffic. It reports on top accessed URLs, referrers, user
#  agents, browser counts, daily accesses, access by time, and recent
#  accesses and referrers. It excludes requests from IPs listed in
#  apache_ignore.list, searched in ./etc/, ../etc/ and /etc/cron.config.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./apache_log_analysis.sh [log_file_path]
#  Example:
#      ./apache_log_analysis.sh /var/log/apache2/ssl_access.log
#
#  Version History:
#  v2.2 2025-08-27
#       Limit Daily Access aggregation to last year and add gdate fallback with command checks.
#  v2.1 2025-07-30
#       Search apache_ignore.list in /etc/cron.config first, before fallback to local etc/ paths.
#  v2.0 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
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

# Check if at least one of the given commands exists and is executable.
check_commands_any() {
    # Usage: check_commands_any cmd1 cmd2 ...
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -n "$cmd_path" ] && [ -x "$cmd_path" ]; then
            echo "$cmd_path"
            return 0
        fi
    done
    echo "[ERROR] None of the required commands found: $*" >&2
    return 1
}

# Load list of IP addresses to ignore from a configuration file.
load_ignore_list() {
    IGNORE_FILE=""
    for candidate in \
        "$(dirname "$0")/etc/apache_ignore.list" \
        "$(dirname "$0")/../etc/apache_ignore.list" \
        "/etc/cron.config/apache_ignore.list"
    do
        if [ -f "$candidate" ]; then
            IGNORE_FILE="$candidate"
            break
        fi
    done

    if [ -n "$IGNORE_FILE" ]; then
        IGNORE_IPS=$(awk '!/^#/ && NF' "$IGNORE_FILE" | paste -sd "|" -)
    else
        IGNORE_IPS="127.0.0.1"
        echo "[WARN] Ignore file not found. Using default ignore IP: $IGNORE_IPS" >&2
    fi
}

# Resolve date command and set 1-year cutoff timestamp (prefers gdate on macOS)
setup_time_window() {
    # Prefer gdate if available (especially on macOS), otherwise fall back to date
    DATE_CMD=$(check_commands_any gdate date) || {
        echo "[ERROR] date command not found." >&2
        exit 127
    }

    # Verify GNU date support for -d option
    if ! "$DATE_CMD" -d '1970-01-01' +%s >/dev/null 2>&1; then
        # Hint for macOS users
        UNAME_S=$(uname 2>/dev/null || echo unknown)
        if [ "$UNAME_S" = "Darwin" ]; then
            echo "[ERROR] GNU date required. Install coreutils and ensure gdate is available (e.g., brew install coreutils)." >&2
        else
            echo "[ERROR] date command lacks GNU -d support." >&2
        fi
        exit 2
    fi

    ONE_YEAR_AGO=$("$DATE_CMD" -d '1 year ago' +%s)
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
    # Filter to last 1 year based on log timestamp, then aggregate per day.
    # Apache time fields: $4 = [dd/Mon/yyyy:HH:MM:SS   $5 = +ZZZZ]
    zgrep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | \
    awk -v cutoff="${ONE_YEAR_AGO}" -v datecmd="${DATE_CMD}" '
        BEGIN {
            month["Jan"]="01"; month["Feb"]="02"; month["Mar"]="03"; month["Apr"]="04";
            month["May"]="05"; month["Jun"]="06"; month["Jul"]="07"; month["Aug"]="08";
            month["Sep"]="09"; month["Oct"]="10"; month["Nov"]="11"; month["Dec"]="12";
        }
        {
            t1=$4; gsub(/^\[/,"",t1);            # dd/Mon/yyyy:HH:MM:SS
            t2=$5; gsub(/\]$/,"",t2);            # +ZZZZ
            if (t1 == "" || t2 == "") next;
            # Build "dd Mon yyyy HH:MM:SS +ZZZZ" for GNU date
            split(t1, a, /[:\/]/);               # a1=dd a2=Mon a3=yyyy a4=HH a5=MM a6=SS
            ts = a[1] " " a[2] " " a[3] " " a[4] ":" a[5] ":" a[6] " " t2
            cmd = datecmd " -d \"" ts "\" +%s"
            cmd | getline epoch
            close(cmd)
            if (epoch >= cutoff) {
                ymd = a[3] "-" month[a[2]] "-" a[1]   # ISO形式で安定ソート
                print ymd
            }
        }
    ' | LC_ALL=C sort | uniq -c

    echo "[Access By Time]"
    grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk '{print $4}' | cut -b 2-15 | sort | uniq -c

    echo "[Recent Accesses]"
    grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -nr | head -n 100

    echo "[Recent Referer]"
    grep https "$LOG_FILE"* | grep -vE "$IGNORE_IPS" | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if [ "$#" -eq 0 ]; then
        usage
    fi

    check_commands grep zgrep awk cut sort uniq wc paste uname

    LOG_FILE=$1

    if [ ! -f "$LOG_FILE" ]; then
        echo "[ERROR] Log file not found at $LOG_FILE." >&2
        exit 1
    fi

    load_ignore_list
    setup_time_window
    analyze_logs
    return 0
}

# Execute main function
main "$@"
