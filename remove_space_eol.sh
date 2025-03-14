#!/bin/sh

########################################################################
# remove_space_eol.sh: End-of-Line Whitespace Removal Script
#
#  Description:
#  This script removes trailing whitespace from each line in the specified files.
#  It's useful for cleaning up source code files and other text files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#       Added command existence check for sed and mv.
#  v1.1 2023-12-05
#       Refactored for better error handling and added comments.
#  v1.0 2008-08-22
#       Initial release. Removes trailing whitespace from files.
#
#  Usage:
#  find [directory...] -type f -name "*" -exec /path/to/remove_space_eol.sh {} \;
#
########################################################################

# Function to check required commands
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

check_commands sed mv

# Process each file passed as an argument
while [ $# -gt 0 ]; do
    # Create a temporary file
    if mv "$1" "$1.tmp"; then
        # Remove trailing whitespace and overwrite the original file
        if sed -e 's/[[:blank:]]*$//' "$1.tmp" > "$1"; then
            echo "Removed trailing whitespace from '$1'"
        else
            echo "Error processing '$1'" >&2
        fi
        # Remove the temporary file
        rm "$1.tmp"
    else
        echo "Error moving '$1' to temporary file" >&2
    fi

    # Move to the next file
    shift
done

