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
#      ./apache_log_analysis.sh [options] [log_file_path]
#  Example:
#      ./apache_log_analysis.sh /var/log/apache2/ssl_access.log
#      ./apache_log_analysis.sh -n 200 /var/log/apache2/ssl_access.log
#      ./apache_log_analysis.sh --all /var/log/apache2/ssl_access.log
#
#  Options:
#      -n, --top N: Limit "Access Count" and "Referer" output to top N entries (default: 100). Use 0 for no limit.
#      -a, --all:   No output limit for "Access Count" and "Referer" (same as -n 0).
#
#  Version History:
#  v2.6 2026-01-08
#       Add "Blog Entry Access" section to aggregate /XXXX/YYYY/MM/DD/NNNN/ hits and sort by date descending.
#  v2.5 2026-01-06
#       Add configurable output limit for "Access Count" and "Referer" (default top 100, adjustable, or no limit).
#       Fix log filtering to avoid dropping most lines, use safer ignore matching, and parse referer more robustly.
#  v2.4 2025-12-30
#       Redefine "Access By Time" to aggregate by hour-of-day (HH) for clearer time-of-day distribution.
#  v2.3 2025-12-27
#       Exclude static assets (css/js/fonts/images) from analysis counts.
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

# Exclude static assets (css/js/fonts/images) from request path counting
# Note: matched against the request path (2nd token of "$2" in -F'"' awk)
EXCLUDE_PATH_RE='[.]((css|js|map)|(woff2?|ttf|otf|eot)|(png|jpe?g|gif|svg|webp|ico|avif))([?].*)?$'

# Exclude likely automated clients for blog-entry-only aggregation (case-insensitive)
# Note: tuned for "human views" and may exclude link preview bots as well.
# Generic crawlers / bots, search engines, SEO tools, SNS preview fetchers, CLI / programmatic clients
BLOG_BOT_UA_RE='(bot|spider|crawl|slurp|archiver|fetch|scanner|monitor|'\
'googlebot|bingbot|duckduckbot|baiduspider|yandexbot|'\
'ahrefsbot|semrushbot|mj12bot|dotbot|'\
'facebookexternalhit|twitterbot|slackbot|'\
'curl|wget|python-requests|go-http-client)'

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

    IGNORE_DEFAULT_IP="127.0.0.1"
    if [ -z "$IGNORE_FILE" ]; then
        echo "[WARN] Ignore file not found. Using default ignore IP: $IGNORE_DEFAULT_IP" >&2
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

    CUTOFF_YMD=$("$DATE_CMD" -d '1 year ago' +%Y-%m-%d)
}

# Print log lines excluding ignored IPs and static asset requests
filter_log_lines() {
    # Use zgrep to cover both plain and gz logs
    IGNORE_FILE_USE=${IGNORE_FILE:-/dev/null}
    zgrep -h -e '' "$LOG_FILE"* | \
    awk -v ex="${EXCLUDE_PATH_RE}" -v default_ip="${IGNORE_DEFAULT_IP}" -F '"' '
        FNR==NR {
            line = $0
            sub(/#.*/, "", line)
            gsub(/^[[:space:]]+/, "", line)
            gsub(/[[:space:]]+$/, "", line)
            if (line == "") next
            split(line, f, /[[:space:]]+/)
            if (f[1] == "") next
            ignore[f[1]] = 1
            next
        }
        BEGIN {
            ignore[default_ip] = 1
        }
        {
            # Exclude ignored IPs (fixed-string match on the first field)
            ip = $1
            if (ip in ignore) next

            # $2: request line (e.g., GET /path HTTP/1.1)
            split($2, a, " ")
            if (a[2] ~ ex) next
            print $0
        }
    ' "$IGNORE_FILE_USE" -
}

# Print lines with optional top-N limitation.
print_top() {
    if [ "$NO_LIMIT" -eq 1 ] || [ "$TOP_N" -eq 0 ]; then
        cat
        return 0
    fi
    head -n "$TOP_N"
    return 0
}

# Print top accessed request paths.
print_access_count() {
    echo "[Access Count]"
    filter_log_lines | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -nr | print_top
}

# Print top referers.
print_referer() {
    echo "[Referer]"
    filter_log_lines | awk -F '"' '{print $4}' | sort | uniq -c | sort -nr | print_top
}

# Print top user agents.
print_user_agent() {
    echo "[User Agent]"
    filter_log_lines | awk -F '"' '{print $6}' | sort | uniq -c | sort -nr | head -n 50
}

# Print browser counts.
print_browser() {
    echo "[Browser]"
    for UA in MSIE Firefox Chrome Safari; do
        COUNT=$(filter_log_lines | grep "$UA" | wc -l)
        echo "$UA: $COUNT"
    done
}

# Print access counts by hour of day (HH).
print_access_by_time() {
    echo "[Access By Time]"
    filter_log_lines | awk '{t=$4; gsub(/^\[/,"",t); split(t,a,":"); print a[2]}' | LC_ALL=C sort | uniq -c
}

# Print daily access counts for the last year (YYYY-MM-DD).
print_daily_access() {
    echo "[Daily Access]"
    # Filter to last 1 year by YYYY-MM-DD derived from log timestamp, then aggregate per day.
    # Apache time fields: $4 = [dd/Mon/yyyy:HH:MM:SS   $5 = +ZZZZ]
    filter_log_lines | \
    awk -v cutoff_ymd="${CUTOFF_YMD}" '
        BEGIN {
            month["Jan"]="01"; month["Feb"]="02"; month["Mar"]="03"; month["Apr"]="04";
            month["May"]="05"; month["Jun"]="06"; month["Jul"]="07"; month["Aug"]="08";
            month["Sep"]="09"; month["Oct"]="10"; month["Nov"]="11"; month["Dec"]="12";
        }
        {
            t1=$4; gsub(/^\[/,"",t1);            # dd/Mon/yyyy:HH:MM:SS
            t2=$5; gsub(/\]$/,"",t2);            # +ZZZZ
            if (t1 == "" || t2 == "") next;
            split(t1, a, /[:\/]/);               # a1=dd a2=Mon a3=yyyy a4=HH a5=MM a6=SS
            ymd = a[3] "-" month[a[2]] "-" a[1]  # Sort by ISO format
            if (ymd >= cutoff_ymd) print ymd
        }
    ' | LC_ALL=C sort | uniq -c
}

# Print blog-like entry access counts: */YYYY/MM/DD/NNNN/
print_blog_entry_access() {
    echo "[Blog Entry Access]"
    # Aggregate blog-like pages with pattern: */YYYY/MM/DD/NNNN/
    # Sort by date (YYYYMMDD) descending, then entry id descending.
    filter_log_lines | \
    awk -v bot_re="${BLOG_BOT_UA_RE}" -F '"' '
        BEGIN {
            bot_re_l = tolower(bot_re)
        }
        {
            # Exclude non-200 responses to approximate actual page views
            status = $3
            sub(/^[[:space:]]+/, "", status)
            split(status, f, /[[:space:]]+/)
            status = f[1]
            if (status != "200") next
            ua = tolower($6)
            if (ua == "" || ua == "-") next
            if (ua ~ bot_re_l) next
            print $0
        }
    ' | awk -F '"' '{print $2}' | awk '{print $2}' | \
    awk '
        # Accept optional query/hash after the canonical trailing slash.
        # Always aggregate by the canonical path only (exclude ?... and #...).
        match($0, /(\/[0-9]{4}\/[0-9]{2}\/[0-9]{2}\/[0-9]+\/)([?#].*)?$/) {
            p = substr($0, RSTART, RLENGTH)
            q = index(p, "?"); h = index(p, "#")
            cut = 0
            if (q > 0) cut = q
            if (h > 0 && (cut == 0 || h < cut)) cut = h
            if (cut > 0) p = substr(p, 1, cut - 1)
            cnt[p]++
            split(p, a, "/")
            dkey[p] = a[2] a[3] a[4]
            idkey[p] = a[5]
        }
        END {
            for (p in cnt) {
                printf "%s %010d %d %s\n", dkey[p], idkey[p], cnt[p], p
            }
        }
    ' | LC_ALL=C sort -k1,1nr -k2,2nr | awk '{print $3, $4}'
}

# Analyze the log file and output various access statistics.
analyze_logs() {
    print_access_count
    print_blog_entry_access
    print_referer
    print_daily_access
    print_access_by_time
    print_user_agent
    print_browser
}

# Main entry point of the script
main() {
    TOP_N=100
    NO_LIMIT=0

    while [ "$#" -gt 0 ]; do
        case "$1" in
            -h|--help|-v|--version)
                usage
                ;;
            -a|--all)
                NO_LIMIT=1
                TOP_N=0
                shift
                ;;
            -n|--top)
                if [ "$#" -lt 2 ]; then
                    echo "[ERROR] Option '$1' requires an argument." >&2
                    exit 2
                fi
                TOP_N=$2
                shift 2
                ;;
            --top=*)
                TOP_N=${1#*=}
                shift
                ;;
            --)
                shift
                break
                ;;
            -*)
                echo "[ERROR] Unknown option: $1" >&2
                exit 2
                ;;
            *)
                break
                ;;
        esac
    done

    if [ "$#" -eq 0 ]; then
        usage
    fi

    case "$TOP_N" in
        ''|*[!0-9]*)
            echo "[ERROR] Invalid value for --top: $TOP_N (must be a non-negative integer)." >&2
            exit 2
            ;;
    esac

    if [ "$TOP_N" -eq 0 ]; then
        NO_LIMIT=1
    fi

    check_commands grep zgrep awk sort uniq wc paste uname cat head

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
