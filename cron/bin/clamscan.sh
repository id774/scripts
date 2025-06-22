#!/bin/sh

########################################################################
# clamscan.sh: Automated ClamAV Scan Script
#
#  Description:
#  This script performs an automated ClamAV scan on specified directories.
#  - Runs `freshclam` to update virus definitions.
#  - Reads an exclusion file for directories and files to exclude.
#  - Runs `clamscan` recursively on the target directories.
#  - Logs scan results to a specified log file.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-05-16
#       Add return 0 to main and exit $? at script end for consistent exit status.
#  v1.3 2025-05-10
#       Refactor into POSIX-compliant, function-based structure with cron check and usage display.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-21
#       Moved clamscan_exclude from /root/bin to /root/etc and updated references.
#  v1.0 2025-03-16
#       Refactored script for POSIX compliance and improved maintainability.
#       Added safety checks and validation for exclusion file handling.
#  v0.5 2011-06-15
#       Forked from clamav_upgrade.sh.
#  v0.4 2009-10-22
#       Upgrade repository svn to git.
#  v0.3 2009-06-16
#       Refactoring.
#  v0.2 2007-11-10
#       Added clamscan, preserved source and object code.
#  v0.1 2007-10-16
#       Initial version.
#
#  Usage:
#      ./clamscan.sh
#      This script is intended to be executed periodically by cron.
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

# Function to check if the script is running from cron
is_running_from_cron() {
    if tty -s; then
        return 1  # Terminal attached (interactive session)
    else
        return 0  # No terminal (likely cron)
    fi
}

# Initialize environment
initialize() {
    if ! is_running_from_cron; then
        echo "[ERROR] This script is intended to be run by cron only." >&2
        exit 1
    fi

    TARGETDIRS="/"
    LOGFILE="/var/log/clamav/clamav.log"
    EXECDIR="$(cd "$(dirname "$0")" && pwd)"
    EXCLUDEFILE="$EXECDIR/../etc/clamscan_exclude"
    OPTS=""
}

# Update virus definitions
update_virus_definitions() {
    echo "[INFO] Stopping freshclam..." >&2
    systemctl stop clamav-freshclam.service

    echo "[INFO] Updating virus definitions..." >&2
    if ! freshclam; then
        echo "[WARN] Failed to update ClamAV virus definitions." >&2
    fi

    echo "[INFO] Restarting freshclam..." >&2
    systemctl start clamav-freshclam.service
}

# Read exclusions
load_exclude_options() {
    if [ -s "$EXCLUDEFILE" ]; then
        while IFS= read -r line; do
            case "$line" in
                */) OPTS="$OPTS --exclude-dir=${line%/}" ;;
                *)  OPTS="$OPTS --exclude=$line" ;;
            esac
        done < "$EXCLUDEFILE"
    fi
}

# Run clamscan
run_clamscan() {
    for dir in $TARGETDIRS; do
        echo "[INFO] Scanning: $dir"
        clamscan "$dir" $OPTS -r -i -l "$LOGFILE"
    done
    echo "[INFO] ClamAV scan completed. Logs available at: $LOGFILE"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    initialize
    update_virus_definitions
    load_exclude_options
    run_clamscan

    return 0
}

# Execute main function
main "$@"
exit $?
