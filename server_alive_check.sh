#!/bin/sh

########################################################################
# server_alive_check.sh: Monitor server availability via _is_alive files
#
#  Description:
#  This script checks for the existence and freshness of _is_alive files
#  sent from various servers to a specified directory.
#  It also emits a human readable summary with counts and percentages.
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
#      The script prints detailed per file logs during processing and
#      appends summary lines at the end, showing counts and percentages
#      for non VM hosts ("regular") and VM hosts ("vm").
#      Hosts marked obsolete are excluded from monitored totals.
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
#  - Skips hosts that have a corresponding _is_obsolete marker file.
#  - Outputs errors to standard error for logging and monitoring purposes.
#  - Emits human readable summary lines with counts and percentages
#    for regular and VM classes, excluding obsolete hosts from totals.
#  - Keeps existing alert behavior and exit codes unchanged.
#  - Validates required commands and directory existence before execution.
#
#  Summary Output:
#  - regular: non-VM hosts (filenames not starting with 'VM' or 'vm')
#  - vm     : VM hosts      (filenames starting with 'VM' or 'vm')
#  - obsolete: hosts with a '<host>_is_obsolete' marker file; excluded from totals
#  - fresh : file mtime <= STALE_THRESHOLD seconds
#  - stale : file mtime >  STALE_THRESHOLD seconds
#  - unknown: failed to get mtime; treated as non-fresh for alerting
#
#  Example:
#      [INFO] Hosts: total=12, monitored=10, obsolete=2 (ignored)
#      [INFO] Regular: fresh=7 (70.0%), stale=1 (10.0%), unknown=0
#      [INFO] VM     : fresh=2 (20.0%), stale=0 (0.0%),  unknown=0
#      [INFO] Threshold: 600 seconds
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
#  v2.1 2025-12-15
#       Fix alert flag on unknown timestamps to honor VM exclusion policy.
#  v2.0 2025-09-14
#       Add human readable summary output with counters and percentages.
#       Refactor process_files into smaller functions for readability and maintainability.
#  v1.8 2025-08-21
#       Exclude hosts from monitoring when <host>_is_obsolete marker exists.
#       Do not count them in freshness or alert decisions.
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

# Initialize summary counters
init_summary_counters() {
    TOTAL=0
    OBSOLETE=0
    MONITORED=0
    REG_FRESH=0
    REG_STALE=0
    REG_UNKNOWN=0
    VM_FRESH=0
    VM_STALE=0
    VM_UNKNOWN=0
}

# Classify host name as regular or vm based on prefix
classify_host_name() {
    _base="$1"
    case "$_base" in
        [Vv][Mm]* ) echo "vm" ;;
        * )         echo "regular" ;;
    esac
}

# Bump summary counter
bump_summary_counter() {
    _class="$1"
    _state="$2"
    case "$_class" in
        regular)
            case "$_state" in
                fresh)   REG_FRESH=$((REG_FRESH + 1)) ;;
                stale)   REG_STALE=$((REG_STALE + 1)) ;;
                unknown) REG_UNKNOWN=$((REG_UNKNOWN + 1)) ;;
            esac
            ;;
        vm)
            case "$_state" in
                fresh)   VM_FRESH=$((VM_FRESH + 1)) ;;
                stale)   VM_STALE=$((VM_STALE + 1)) ;;
                unknown) VM_UNKNOWN=$((VM_UNKNOWN + 1)) ;;
            esac
            ;;
    esac
}

# Emit summary output with counts and percentages
emit_summary() {
    # Calculate totals per class
    REG_TOTAL=$((REG_FRESH + REG_STALE + REG_UNKNOWN))
    VM_TOTAL=$((VM_FRESH + VM_STALE + VM_UNKNOWN))

    # Calculate monitored if not set
    if [ -z "$MONITORED" ] || [ "$MONITORED" -lt 0 ]; then
        MONITORED=$((TOTAL - OBSOLETE))
    fi

    pct() {
        # args: numerator denominator
        awk -v a="$1" -v b="$2" 'BEGIN { if (b==0) { printf("0.0%%"); } else { printf("%.1f%%", (a*100.0)/b); } }'
    }

    echo "[INFO] Hosts: total=$TOTAL, monitored=$MONITORED, obsolete=$OBSOLETE (ignored)"
    echo "[INFO] Regular: fresh=$REG_FRESH ($(pct "$REG_FRESH" "$REG_TOTAL")), stale=$REG_STALE ($(pct "$REG_STALE" "$REG_TOTAL")), unknown=$REG_UNKNOWN"
    echo "[INFO] VM     : fresh=$VM_FRESH ($(pct "$VM_FRESH" "$VM_TOTAL")), stale=$VM_STALE ($(pct "$VM_STALE" "$VM_TOTAL")), unknown=$VM_UNKNOWN"
    echo "[INFO] Threshold: $STALE_THRESHOLD seconds"
}

# Find all files ending with '_is_alive' under the base directory
find_is_alive_files() {
    find "$BASE_DIR" -type f -name '*_is_alive'
}

# Increment TOTAL and check if a _is_obsolete marker exists for the same host
is_obsolete() {
    _file="$1"
    TOTAL=$((TOTAL + 1))
    _dir=$(dirname "$_file")
    _base=$(basename "$_file")
    _host=${_base%_is_alive}
    _marker="$_dir/${_host}_is_obsolete"
    [ -f "$_marker" ]
}

# Build two groups: obsolete list (for grouped print) and target files (non-obsolete)
build_groups() {
    FILES="$1"
    OBSOLETE_LIST=""
    TARGET_FILES=""

    for FILE in $FILES; do
        BASENAME=$(basename "$FILE")
        DIRNAME=$(dirname "$FILE")
        if is_obsolete "$FILE"; then
            HOST=${BASENAME%_is_alive}
            MARKER="${DIRNAME}/${HOST}_is_obsolete"
            OBSOLETE=$((OBSOLETE + 1))
            # Defer print for obsolete to keep them grouped at top
            OBSOLETE_LIST="${OBSOLETE_LIST}
[INFO] Host is obsolete: ${HOST} (marker: $(basename "$MARKER")) - skipped from monitoring"
        else
            TARGET_FILES="${TARGET_FILES}
${FILE}"
        fi
    done

    # Export group variables for next steps
    CLEAN_TARGETS=$(printf "%s\n" "$TARGET_FILES" | sed '/^[[:space:]]*$/d')
    export OBSOLETE_LIST CLEAN_TARGETS
}

# Print grouped obsolete hosts first (if any)
print_grouped_obsolete() {
    if [ -n "$OBSOLETE_LIST" ]; then
        printf "%s\n" "$OBSOLETE_LIST" | sed '1{/^$/d;}'
    fi
}

# Get mtime and formatted date for a file; echo "TIME DATE" or empty on failure
get_file_times() {
    _file="$1"
    _time=$($stat_cmd -c %Y "$_file" 2>/dev/null || stat -f %m "$_file" 2>/dev/null)
    if [ -z "$_time" ]; then
        echo ""
        return 1
    fi
    _date=$(date -d "@$_time" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || date -r "$_time" "+%Y-%m-%d %H:%M:%S" 2>/dev/null)
    if [ -z "$_date" ]; then
        echo ""
        return 1
    fi
    echo "$_time $_date"
    return 0
}

# Evaluate freshness and print per-file log; update flags and counters
evaluate_and_count() {
    FILE="$1"
    CURRENT_TIME="$2"
    BASENAME=$(basename "$FILE")

    TIMES=$(get_file_times "$FILE")
    if [ -z "$TIMES" ]; then
        echo "[WARN] Failed to retrieve modification time for: $FILE" >&2
        CLASS=$(classify_host_name "$BASENAME")
        bump_summary_counter "$CLASS" "unknown"
        # Respect VM exclusion policy: do not set regular alert flag for VM
        if [ "$CLASS" = "regular" ]; then
            REGULAR_STALE_FOUND=1
        else
            VM_STALE_FOUND=1
        fi
        return 0
    fi

    FILE_TIME=$(printf "%s" "$TIMES" | awk '{print $1}')
    FILE_DATE=$(printf "%s" "$TIMES" | awk '{print $2" "$3}')

    AGE=$((CURRENT_TIME - FILE_TIME))
    AGE_MIN=$((AGE / 60))
    AGE_SEC=$((AGE % 60))
    ELAPSED="elapsed time: ${AGE_MIN}m ${AGE_SEC}s"

    if [ "$AGE" -gt "$STALE_THRESHOLD" ]; then
        if printf '%s\n' "$BASENAME" | grep -iq '^vm'; then
            echo "[INFO] File is stale: $BASENAME (last updated: $FILE_DATE, $ELAPSED, VM-prefixed: ignored)"
            VM_STALE_FOUND=1
            CLASS=$(classify_host_name "$BASENAME")
            bump_summary_counter "$CLASS" "stale"
        else
            echo "[WARN] File is stale: $BASENAME (last updated: $FILE_DATE, $ELAPSED)" >&2
            REGULAR_STALE_FOUND=1
            CLASS=$(classify_host_name "$BASENAME")
            bump_summary_counter "$CLASS" "stale"
        fi
    else
        echo "[INFO] File is fresh: $BASENAME (last updated: $FILE_DATE, $ELAPSED)"
        CLASS=$(classify_host_name "$BASENAME")
        bump_summary_counter "$CLASS" "fresh"
    fi

    return 0
}

# Print final summary and decide exit status message
finalize_and_exitcode() {
    # Emit human readable summary
    emit_summary

    if [ "$REGULAR_STALE_FOUND" -eq 1 ]; then
        echo "[WARN] One or more regular hosts are stale." >&2
        return 1
    elif [ "$VM_STALE_FOUND" -eq 1 ]; then
        echo "[INFO] Only VM-prefixed hosts are missing. No alert triggered."
        return 0
    else
        if [ -n "$OBSOLETE_LIST" ]; then
            echo "[INFO] All files are fresh or explicitly skipped as obsolete."
            return 0
        else
            echo "[INFO] All files are fresh."
            return 0
        fi
    fi
}

# Orchestrate the whole check (former process_files)
process_files() {
    FILES=$(find_is_alive_files | sort)

    if [ -z "$FILES" ]; then
        echo "[ERROR] No '_is_alive' files found in: $BASE_DIR" >&2
        exit 2
    fi

    CURRENT_TIME=$(date +%s)
    REGULAR_STALE_FOUND=0
    VM_STALE_FOUND=0

    init_summary_counters

    # First pass: split obsolete and targets (also increments TOTAL)
    build_groups "$FILES"

    # Print grouped obsolete hosts first
    print_grouped_obsolete

    # Update monitored count after obsolete filtering
    if [ -n "$CLEAN_TARGETS" ]; then
        MONITORED=$((TOTAL - OBSOLETE))
    fi

    # Second pass: evaluate freshness for non-obsolete files (no subshell)
    while IFS= read -r FILE; do
        [ -z "$FILE" ] && continue
        evaluate_and_count "$FILE" "$CURRENT_TIME"
    done <<EOF
$CLEAN_TARGETS
EOF

    # Final summary and exit code decision
    finalize_and_exitcode
    return $?
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
    check_commands find stat date basename grep sort sed dirname awk
    choice_stat_command
    process_files
    return $?
}

# Execute main function
main "$@"
