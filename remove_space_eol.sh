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
#  v1.3 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
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
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to remove trailing whitespace from a file
process_file() {
    file="$1"

    if mv "$file" "$file.tmp"; then
        if sed -e 's/[[:blank:]]*$//' "$file.tmp" > "$file"; then
            echo "Removed trailing whitespace from '$file'"
        else
            echo "Error processing '$file'" >&2
        fi
        rm "$file.tmp"
    else
        echo "Error moving '$file' to temporary file" >&2
    fi
}

# Main function to execute the script
main() {
    check_commands sed mv

    while [ $# -gt 0 ]; do
        process_file "$1"
        shift
    done
}

# Execute main function
main "$@"
