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
#  v1.5 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
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
#  Notes:
#  - This script should be run from the root directory of the script collection.
#  - Make sure to back up your scripts before running this script as a precaution.
#  - SCRIPTS environment variable must be set to the path of the script collection.
#
########################################################################

# Function to check if required commands exist
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to validate the SCRIPTS environment variable
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
        exit 1
    fi
}

# Function to set file permissions
set_permissions() {
    chmod -R u+rw,g+r,g-w,o+r,o-w "$SCRIPTS"/*

    # Set execute permissions for script files
    find "$SCRIPTS"/ -type f \( -name "*.sh" -o -name "*.py" -o -name "*.rb" \) -exec chmod u+x,g+x,o+x {} \;
}

# Main function
main() {
    check_scripts
    check_commands chmod find
    set_permissions
}

# Execute main function
main "$@"
