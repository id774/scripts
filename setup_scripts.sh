#!/bin/sh

########################################################################
# setup_scripts.sh: Setup Script for Scripts Collection
#
#  Description:
#  This script sets the appropriate permissions for a collection of scripts.
#  It adjusts read/write/execute permissions for users, groups, and others.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.2 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.1 2023-12-08
#       Added documentation and environment variable check for SCRIPTS.
#  v1.0 2008-08-22
#       Initial release.
#
#  Usage:
#  Run this script to set up the permissions for a collection of scripts.
#  Ensure that the SCRIPTS environment variable is set to the path of
#  your script collection before running this script.
#
#  Note:
#  - This script should be run from the root directory of the script collection.
#  - Make sure to back up your scripts before running this script as a precaution.
#  - SCRIPTS environment variable must be set to the path of the script collection.
#
########################################################################

# Check if SCRIPTS variable is set
if [ -z "$SCRIPTS" ]; then
    echo "Error: SCRIPTS environment variable is not set."
    echo "Please set the SCRIPTS variable to the path of your script collection."
    exit 1
fi

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
check_commands chmod find

# Set permissions for all files in the script collection
chmod -R u+rw,g+r,g-w,o+r,o-w "$SCRIPTS"/*

# Set execute permissions for script files
find "$SCRIPTS"/ -type f \( -name "*.sh" -o -name "*.py" -o -name "*.rb" \) -exec chmod u+x,g+x,o+x {} \;

