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
#  Run this script to perform an automated ClamAV scan:
#      ./clamscan.sh
#
#  Requirements:
#  - Must be executed on a system with `clamav` installed.
#  - Requires `freshclam` for virus database updates.
#  - Expects an exclusion file for directories and files to be ignored.
#
########################################################################

# Define scan targets and log file
TARGETDIRS="/"
LOGFILE="/var/log/clamav/clamav.log"
EXECDIR="$(dirname "$0")"
EXCLUDEFILE="$EXECDIR/../etc/clamscan_exclude"

# Update virus definitions
systemctl stop clamav-freshclam.service
freshclam || {
    echo "[ERROR] Failed to update ClamAV virus definitions." >&2
    exit 1
}
systemctl start clamav-freshclam.service

# Construct exclusion options
OPTS=""
if [ -s "$EXCLUDEFILE" ]; then
    while IFS= read -r line; do
        case "$line" in
            */) OPTS="$OPTS --exclude-dir=${line%/}" ;;
            *)  OPTS="$OPTS --exclude=$line" ;;
        esac
    done < "$EXCLUDEFILE"
fi

# Run ClamAV scan
for dir in $TARGETDIRS; do
    echo "Scanning: $dir"
    clamscan "$dir" $OPTS -r -i -l "$LOGFILE"
done

echo "ClamAV scan completed. Logs available at: $LOGFILE"
exit 0
