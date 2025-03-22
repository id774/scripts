#!/bin/sh

########################################################################
# cleanup-junk-files.sh: Clean Up Junk Files
#
#  Description:
#  This script removes common junk files from a specified directory.
#  It targets files like .DS_Store, ._* AppleDouble files, temporary
#  Unix files ending with '.un~', and __pycache__ directories.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.6 2025-03-16
#       Encapsulated all logic in functions and introduced main function.
#  v1.5 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.4 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.3 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.2 2023-12-20
#       Added feature to remove __pycache__ directories.
#  v1.1 2023-12-06
#       Refactored for improved readability, added detailed comments and notes.
#  v1.0 2016-08-05
#       Initial release.
#
#  Usage:
#      cleanup-junk-files.sh <target_directory>
#  Example:
#      cleanup-junk-files.sh /path/to/directory
#
#  Notes:
#  - This script will recursively delete the specified junk files in the target directory.
#  - Use with caution and ensure that the target directory is correct.
#  - It is advisable to perform a dry run or backup important files before execution.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

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

# Perform cleanup of junk files
cleanup_junk_files() {
    echo "Cleaning up junk files in $1..."

    echo "Removing ._* AppleDouble files..."
    find "$1" -name '._*' -exec rm -vf {} \;

    echo "Removing .DS_Store files..."
    find "$1" -name '.DS_Store' -exec rm -vf {} \;

    echo "Removing temporary Unix files ending with '.un~'..."
    find "$1" -name '.*.un~' -exec rm -vf {} \;

    echo "Removing __pycache__ directories..."
    find "$1" -type d -name '__pycache__' -exec rm -vrf {} \;

    echo "Cleanup completed."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Check if rm and find commands exist
    check_commands rm find

    # Check if a directory is provided
    if [ -z "$1" ]; then
        echo "Error: No target directory provided."
        echo "Usage: $0 <target_directory>"
        exit 0
    fi

    cleanup_junk_files "$1"
}

# Execute main function
main "$@"
