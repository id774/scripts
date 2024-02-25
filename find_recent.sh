#!/bin/sh

########################################################################
# find_recent.sh: List files updated after a specified datetime
#
#  Description:
#  This script lists all files in the current directory and its subdirectories
#  that have been modified after a specified date and optional time.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-02-25
#       Initial release. Added functionality to list files based on modification date.
#
#  Usage:
#  Run this script with a date argument in the format YYYY-MM-DD and an optional
#  time in the format HH:MM to list files modified after that datetime.
#  Examples: ./find_recent.sh 2024-01-01
#            ./find_recent.sh 2024-01-01 13:00
#
#  Notes:
#  - Ensure the 'find' command is available on your system.
#  - If the time is not specified, it defaults to 00:00 (midnight) of the given date.
#
#  Error Conditions and Return Codes:
#  0. Success.
#  1. Missing arguments.
#  2. Incorrect datetime format.
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
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

# Check if necessary commands are available
check_commands find grep ls

# Check for the presence of at least one argument
if [ $# -lt 1 ]; then
    echo "Usage: $0 <date in YYYY-MM-DD format> [time in HH:MM format]"
    exit 1
fi

DATE=$1

# If a time argument is provided, append it to the date
if [ $# -ge 2 ]; then
    TIME=$2
    DATETIME="$DATE $TIME"
else
    DATETIME="$DATE"
fi

# Check if the datetime format is correct (basic check)
if ! echo "$DATETIME" | grep -qE '^[0-9]{4}-[0-9]{2}-[0-9]{2}( [0-9]{2}:[0-9]{2})?$'; then
    echo "Error: Datetime format is incorrect. Please use YYYY-MM-DD [HH:MM] format."
    exit 2
fi

# List files modified after the specified datetime
find . -type f -newermt "$DATETIME" -exec ls -l {} +

