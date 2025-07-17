#!/bin/sh

########################################################################
# check_scripts.sh: Validate installer scripts with -h option
#
#  Description:
#  This script tests all shell scripts (*.sh) located under the $SCRIPTS
#  and $SCRIPTS/installer directories by executing each with the '-h'
#  option. The purpose is to detect syntax errors, typos, or broken scripts
#  early by verifying that each script:
#    - Executes without errors
#    - Exits with return code 0
#    - (Normally) Displays help message output
#
#  If a script fails to execute correctly, or exits with a non-zero
#  return code, the failure is reported at the end. All scripts are
#  tested sequentially, and processing continues regardless of individual
#  failures to ensure full coverage.
#
#  This script is intended as a lightweight automated health check
#  for installer and setup scripts within a project.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./check_scripts.sh
#
#  Requirements:
#  - Environment variable $SCRIPTS must be set and point to the base directory containing the scripts.
#  - The shell scripts should be executable and respond to the '-h' option.
#
#  Expected Behavior:
#  - Each script should display its help message (if implemented) and exit cleanly with return code 0.
#  - The script will output an [INFO] log for each test and summarize the total, successful, and failed tests.
#  - If all scripts succeed, the script exits with return code 0.
#
#  Abnormal Behavior:
#  - If a script exits with a return code greater than 0, it is considered a failure.
#  - Syntax errors, missing interpreters, execution failures, or incorrect responses to the '-h' option are detected.
#  - All failed scripts are listed in the final error report.
#  - If any failures are detected, the script exits with return code 1.
#
#  Features:
#  - POSIX-compliant structure; suitable for use in minimal environments.
#  - No dependencies on non-standard utilities (e.g., mktemp).
#  - Graceful handling of failures with continued testing across all scripts.
#  - Summarized reporting for easy identification of issues.
#
#  Version History:
#  v1.1 2025-05-10
#       Test all scripts in $SCRIPTS/cron/bin regardless of file extension.
#  v1.0 2025-04-28
#       Initial release. Implements sequential help-option testing with POSIX-compliant structure.
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

# Check if required commands exist
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

# Validate the SCRIPTS environment variable
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
        exit 1
    fi
}

# Test a single script
test_script() {
    script="$1"
    echo "[INFO] Testing: $script -h"
    if sh "$script" -h >/dev/null 2>&1; then
        SUCCESS=`expr "$SUCCESS" + 1`
    else
        REPORT="${REPORT}\n[ERROR] Failed: $script"
        FAIL=`expr "$FAIL" + 1`
    fi
    TOTAL=`expr "$TOTAL" + 1`
}

# Main test function
test_function() {
    # Initialize counters
    TOTAL=0
    SUCCESS=0
    FAIL=0

    # Initialize report string
    REPORT=""

    # Find and test scripts under $SCRIPTS and $SCRIPTS/installer
    for dir in "$SCRIPTS" "$SCRIPTS/installer"; do
        if [ -d "$dir" ]; then
            for script in "$dir"/*.sh; do
                if [ -f "$script" ]; then
                    test_script "$script"
                fi
            done
        fi
    done

    # Include all files (with or without extension) in cron/bin
    if [ -d "$SCRIPTS/cron/bin" ]; then
        for script in "$SCRIPTS/cron/bin"/*; do
            [ -f "$script" ] && test_script "$script"
        done
    fi

    final_report

    # Show error details if any
    if [ "$FAIL" -gt 0 ]; then
        echo "[ERROR] Failed Scripts:"
        # Remove leading newline when printing
        printf "%b\n" "$REPORT"
    fi

    # Exit with success if all scripts passed
    if [ "$FAIL" -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

final_report() {
    echo "======================================="
    echo "[INFO] Test Summary"
    echo "Total Scripts: $TOTAL"
    echo "Success:       $SUCCESS"
    echo "Failed:        $FAIL"
    echo "======================================="
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_scripts
    check_commands sh expr
    test_function
}

# Execute main function
main "$@"
