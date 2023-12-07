#!/bin/sh

########################################################################
# safari-vacuum.sh: Optimize Safari Cache Database
#
#  Description:
#  This script performs the SQLite vacuum command on Safari's cache
#  database to optimize it. Vacuuming reclaims free space and can improve
#  performance.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2010-11-29
#       Initial release.
#
#  Usage:
#  ./safari-vacuum.sh
#
#  Notes:
#  - Ensure Safari is not running while executing this script.
#  - It's recommended to back up the cache database before running.
#
########################################################################

# Check if sqlite3 command is available
if ! command -v sqlite3 >/dev/null 2>&1; then
    echo "sqlite3 command not found. Please install sqlite3."
    exit 1
fi

# Navigate to Safari's cache directory and perform vacuum
SAFARI_CACHE_DIR="$HOME/Library/Caches/com.apple.Safari"
if [ -d "$SAFARI_CACHE_DIR" ] && [ -w "$SAFARI_CACHE_DIR/Cache.db" ]; then
    cd "$SAFARI_CACHE_DIR"
    sqlite3 Cache.db vacuum
    echo "Safari cache database vacuumed."
else
    echo "Safari cache directory or Cache.db not found or not writable."
    exit 1
fi

